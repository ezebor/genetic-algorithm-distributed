package domain.entities

import akka.remote.DaemonMsgCreate
import app.ExecutionScript
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE, POPULATION_SIZE}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Success, Try}

type Id = (Int, Int)
type Coordinate = (Id, Int, Id)

case class Block(frameLocationId: Id, imageSourceId: Int, pixelsSourceId: Id, fitness: Double)(implicit customRandom: Random = random) extends Gene(fitness) {
  override def mutate: Gene = this
  override def isHealthy: Boolean = fitness >= 1

  override def toString: String = s"Block - frameLocationId: ($frameLocationId), image id: ${imageSourceId}, pixels source id: ${pixelsSourceId}"
  
  def pixelWithFixedLocation(pixel: Pixel): Pixel = Pixel(
    pixel.x % DIMENSION_BLOCK_SIZE + frameLocationId._1,
    pixel.y % DIMENSION_BLOCK_SIZE + frameLocationId._2,
    pixel.argb
  )
}

case class Frame(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] => Frame(aBlocks)

  protected override def calculateFitness: Double = blocks
    .foldLeft(0d) { case (totalFitness, Block(_, _, _, aFitness)) =>
      totalFitness + aFitness / blocks.size
    }

  // TODO: llevar lógica optimizada de crossover al GenericTypes (indexado de genes). Usar trait Id
  override def crossoverWith(couple: Chromosome, crossoverLikelihood: Double): (List[Gene], List[Gene]) = {
    def addGeneAccordingToLikelihood(nextBlock: Block, aGenes: Map[Id, List[Block]]): Map[Id, List[Block]] =
      if(nextBlock.isHealthy || random.nextInt(100) + 1 <= crossoverLikelihood * 100) aGenes.updated(nextBlock.frameLocationId, List(nextBlock))
      else aGenes

    val indexedBlocks = blocks.groupBy(_.frameLocationId)

    val childrenGenes = couple.getGenes.foldLeft(indexedBlocks, indexedBlocks) { case ((leftBlocks, rightBlocks), nextBlock: Block) =>
      (
        addGeneAccordingToLikelihood(nextBlock, leftBlocks),
        addGeneAccordingToLikelihood(nextBlock, rightBlocks)
      )
    }

    (
      childrenGenes._1.values.map(_.head).toList,
      childrenGenes._2.values.map(_.head).toList
    )
  }

  override def mutate: Chromosome = this
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(aFrame: Frame) =>
      Image(Success(aFrame))

  override def isHealthy: Boolean = frame.map(_.fitness).getOrElse(0d) >= 0.99
}

object ImagesManager {
  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  private def luminance(terms: StatisticsTerms): Vector[Double] = terms match
    case StatisticsTerms((meansA, meansB), _) => meansA.map { meanA =>
      meansB.map(meanB => (2 * meanA * meanB + C1) / (Math.pow(meanA, 2) + Math.pow(meanB, 2) + C1)).sum
    }
  private def contrast(terms: StatisticsTerms): Vector[Double] = terms match
    case StatisticsTerms(_, (variancesA, variancesB, _)) => variancesA.map { varianceA =>
      variancesB.map(varianceB => (2 * varianceA * varianceB + C2) / (Math.pow(varianceA, 2) + Math.pow(varianceB, 2) + C2)).sum
    }
  private def structure(terms: StatisticsTerms): Vector[Double] = terms match
    case StatisticsTerms(_, (variancesA, variancesB, covariances)) => variancesA.indices.map { case index =>
      (covariances(index) + C3) / variancesB.map(varianceB => variancesA(index) * varianceB + C3).sum
    }.toVector

  private case class StatisticsTerms(means: (Vector[Double], Vector[Double]), variances: (Vector[Double], Vector[Double], Vector[Double]))

  private def generateStatisticsTerms(pixelsA: Vector[Vector[Pixel]], pixelsB: Vector[Vector[Pixel]]): StatisticsTerms = {
    val size = pixelsA.head.size

    val sumPixels: (Vector[Double], Vector[Double]) = pixelsA.head.indices.foldLeft(pixelsA.map(_ => 0d), pixelsB.map(_ => 0d)) { case ((totalSumPixelsA, totalSumPixelsB), pixelIndex) =>
      (
        totalSumPixelsA.indices.map(referenceIndex => totalSumPixelsA(referenceIndex) + pixelsA(referenceIndex)(pixelIndex).argb).toVector,
        totalSumPixelsB.indices.map(referenceIndex => totalSumPixelsB(referenceIndex) + pixelsB(referenceIndex)(pixelIndex).argb).toVector
      )
    }

    val means: (Vector[Double], Vector[Double]) = (
      sumPixels._1.map(_ / size),
      sumPixels._2.map(_ / size)
    )

    val variances: (Vector[Double], Vector[Double], Vector[Double]) = pixelsA.head.indices.foldLeft(pixelsA.map(_ => 0d), pixelsB.map(_ => 0d), pixelsA.map(_ => 0d)) { case ((totalVariancesA, totalVariancesB, totalCovariances), pixelIndex) =>
      (
        totalVariancesA.indices.map (referenceIndex => totalVariancesA(referenceIndex) + Math.pow(pixelsA(referenceIndex)(pixelIndex).argb - means._1(referenceIndex), 2) / (size - 1)).toVector,
        totalVariancesB.indices.map (referenceIndex => totalVariancesB(referenceIndex) + Math.pow(pixelsB(referenceIndex)(pixelIndex).argb - means._2(referenceIndex), 2) / (size - 1)).toVector,
        totalVariancesA.indices.map {referenceIndex =>
          totalCovariances(referenceIndex) + pixelsB.indices.map(pixelsBIndex => (pixelsA(referenceIndex)(pixelIndex).argb - means._1(referenceIndex)) * (pixelsB(pixelsBIndex)(pixelIndex).argb - means._2(pixelsBIndex)) / (size - 1)).sum
        }.toVector
      )
    }

    StatisticsTerms(
      means,
      (
        variances._1.map(value => Math.sqrt(value)),
        variances._2.map(value => Math.sqrt(value)),
        variances._3
      )
    )
  }

  def toBlocks(coordinates: List[Coordinate]): List[Block] = {
    val groupedCoordinates: List[(Id, List[Coordinate])] = coordinates
      .groupBy { case (frameLocationId, _, _) => frameLocationId }
      .toList

    val statisticsPerCoordinate = groupedCoordinates.map { case (frameLocationId, aCoordinates) =>
      Future {
        val controlPixels = referencesImmutableImages.keys.map { case imageId =>
          referencesPixels(imageId)(frameLocationId)
        }.toVector

        val targetPixels = aCoordinates.map { case (_, imageId, pixelsSourceId) =>
          referencesPixels(imageId)(pixelsSourceId)
        }.toVector

        val terms = generateStatisticsTerms(
          targetPixels,
          controlPixels
        )

        (
          aCoordinates.toVector,
          luminance(terms),
          contrast(terms),
          structure(terms)
        )
      }
    }

    val futureBlocks = statisticsPerCoordinate.foldLeft(Future(List[Block]())) { case (result, nextFutureCoordinates) =>
      for {
        blocks <- result
        case (aCoordinates, luminance, contrast, structure) <- nextFutureCoordinates
      } yield {
        val nextBlocks = aCoordinates.indices.map { index =>
          val aCoordinate = aCoordinates(index)
          val fitness = luminance(index) * contrast(index) * structure(index)
          val roundedFitness = BigDecimal(fitness).setScale(2, BigDecimal.RoundingMode.HALF_DOWN).toDouble
          Block(
            aCoordinate._1,
            aCoordinate._2,
            aCoordinate._3,
            roundedFitness
          )
        }.toList
        blocks ::: nextBlocks
      }
    }

    Await.result(futureBlocks, Duration.Inf)
  }

  lazy val frameLocationIds: IndexedSeq[Id] = Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE)
    .flatMap(x => (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE).map(_ => x))
    .zip(
      (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE)
        .flatMap(_ => Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE))
    )

  def blocksToImmutableImage(blocks: List[Block]): ImmutableImage = {
    val immutableImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    for {
      case Block((locationX, locationY), imageId, pixelsSourceId @ (sourceX, sourceY), _) <- blocks
      aPixel <- referencesPixels(imageId)(pixelsSourceId)
    } yield {
      immutableImage.setPixel(Pixel(
        locationX + aPixel.x - sourceX,
        locationY + aPixel.y - sourceY,
        aPixel.argb
      ))
    }
    immutableImage
  }

  def toCoordinates(imageId: Int): List[Coordinate] = frameLocationIds
    .map { case id: Id =>
      (id, imageId, id)
    }.toList

  lazy val referencesImmutableImages: Map[Int, ImmutableImage] = {
    List(
      ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/paisaje.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    ).zipWithIndex
      .map { case (immutableImage, index) =>
        (index, immutableImage)
      }
      .toMap
  }

  lazy val referencesPixels: Map[Int, Map[Id, Vector[Pixel]]] = {
    println("Creating references pixels")
    val references = referencesBlocks.map { case (imageId, indexedBlocks) =>
      val indexedPixels = indexedBlocks
        .keys
        .map { frameLocationId =>
          frameLocationId -> referencesImmutableImages(imageId)
            .pixels(
              frameLocationId._1,
              frameLocationId._2,
              ExecutionScript.DIMENSION_BLOCK_SIZE,
              ExecutionScript.DIMENSION_BLOCK_SIZE
            )
            .toVector
        }
        .toMap

      imageId -> indexedPixels
    }
    println("References pixels created")
    references
  }

  lazy val referencesBlocks: Map[Int, Map[Id, Block]] = {
    println("Creating references blocks")
    val references = referencesImmutableImages.map { case (imageId, _) =>
      val indexedBlocks = toCoordinates(imageId).foldLeft(Map[Id, Block]()) { case (result, (frameLocationId, anImageSourceId, pixelsSourceId))  =>
        result.updated(frameLocationId, Block(frameLocationId, anImageSourceId, pixelsSourceId, 1.0))
      }
      imageId -> indexedBlocks
    }
    println("References blocks created")
    references
  }

  lazy val referencesImages: List[Image] = {
    println("Creating references images")
    val references = referencesBlocks.map { case (_, indexedBlocks) =>
      blocksToSingleImage(indexedBlocks.values.toList)
    }.toList
    println("References images created")
    references
  }

  def blocksToSingleImage(blocks: List[Block]): Image = Image(Success(Frame(blocks)))

  def blocksToImages(blocks: List[Block]): List[Image] = {
    val indexedBlocks = blocks
      .toVector
      .groupBy(_.frameLocationId)
      .values

    indexedBlocks.head.indices.map { index =>
      val blocks = indexedBlocks.foldLeft(List[Block]()) { case (result, nextBlocks) =>
        nextBlocks(index) :: result
      }
      Image(Success(Frame(blocks)))
    }.toList
  }

  def initialPopulation(): ImagesPopulation = {
    val images = referencesImages
      .map(anImage => anImage.frame.get.blocks)
      .map(aBlocks => ImagesManager.mixCoordinates(aBlocks))
      .flatMap { aCoordinates =>
        blocksToImages(toBlocks(aCoordinates))
      }

    ImagesPopulation(
      images.flatMap { case image: Image =>
          (1 to POPULATION_SIZE / referencesImages.size).map(_ => image.copy(image.frame))
      }
    )
  }

  def mixCoordinates(blocks: List[Block]): List[Coordinate] = {
    val mixedFrameLocationIds = random.shuffle[Id, IndexedSeq[Id]](blocks.map(_.frameLocationId).toIndexedSeq).toVector
    blocks.foldLeft(List[Coordinate](), 0) { case ((coordinates, index), Block(_, imageSourceId, pixelsSourceId, _)) =>
      val fixedIndex = index % mixedFrameLocationIds.size
      (
        (mixedFrameLocationIds(fixedIndex), imageSourceId, pixelsSourceId) :: coordinates,
        fixedIndex + 1
      )
    }._1
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = ImagesPopulation(newIndividuals.map { case image: Image =>
    image
  })

  override def empty(): Population = ImagesPopulation(List[Image]())

  // TODO: llevar lógca común de convergencia de mutación al padre
  // TODO: mutationLikelihood ya no es una probabilidad sino una cantidad --> renombrar
  override def mutate(mutationLikelihood: Double): Population = {
    // TODO: llevar lógica de "tomar máximo X bad genes al padre
    val blocksIndexedByHealthy = images
      .flatMap { case image: Image =>
        image.frame.get.blocks
      }
      .groupBy(_.isHealthy)

    val MAXIMUM_QUANTITY_OF_BAD_BLOCKS = 1000

    val goodBlocksIndexedByFrameLocationId = blocksIndexedByHealthy
      .getOrElse(true, List())
      .groupBy(_.frameLocationId)

    val badBlocksDistinctSourcesFuture = Future {
      val missingFrameLocationIds: Vector[Id] = ImagesManager.frameLocationIds.toVector.diff(goodBlocksIndexedByFrameLocationId.keys.toVector)
      val missingSources: Vector[(Int, Id)] = ImagesManager.frameLocationIds.toVector
        .diff(goodBlocksIndexedByFrameLocationId.values.flatten.map(_.pixelsSourceId).toVector)
        // TODO: no harcodear image id
        .map(aPixelSourceId => (0, aPixelSourceId))

      val frameLocationIdsToMutate = missingFrameLocationIds.take(MAXIMUM_QUANTITY_OF_BAD_BLOCKS)
      val sourcesToMutate = missingSources.take(frameLocationIdsToMutate.size)
      val blocksToKeep = missingFrameLocationIds
        .drop(frameLocationIdsToMutate.size)
        .zip(missingSources.drop(frameLocationIdsToMutate.size))
        .map { case (aFrameLocationId, (anImageSourceId, aPixelsSourceId)) =>
          Block(aFrameLocationId, anImageSourceId, aPixelsSourceId, 0d)
        }
        .toList

      (
        frameLocationIdsToMutate,
        sourcesToMutate,
        blocksToKeep
      )
    }

    val goodDistinctBlocksFuture = Future {
      goodBlocksIndexedByFrameLocationId.values.map(_.head).toList
    }

    val newCoordinatesFuture: Future[List[Coordinate]] = for {
      case (frameLocationIds, sources, _) <- badBlocksDistinctSourcesFuture
    } yield {
      (1 to mutationLikelihood.toInt).flatMap { _ =>
        frameLocationIds.foldLeft(List[Coordinate](), sources) { case ((result, aSources), nextFrameLocationId) =>
          val randomIndex = random.nextInt(aSources.size)
          val randomSource = aSources(randomIndex)
          (
            (nextFrameLocationId, randomSource._1, randomSource._2) :: result,
            aSources.diff(Vector(randomSource))
          )
        }._1
      }.toList
    }

    val newImages: Future[List[Image]] = for {
      newCoordinates <- newCoordinatesFuture
      goodDistinctBlocks <- goodDistinctBlocksFuture
      case (_, _, blocksToKeep) <- badBlocksDistinctSourcesFuture
    } yield {
      if (newCoordinates.isEmpty) (1 to mutationLikelihood.toInt).map(_ => ImagesManager.blocksToSingleImage(goodDistinctBlocks)).toList
      else
        ImagesManager
          .blocksToImages(ImagesManager.toBlocks(newCoordinates))
          .map { case Image(Success(Frame(blocks))) =>
            val frameBlocks = goodDistinctBlocks
              ::: blocksToKeep
              ::: blocks

            ImagesManager.blocksToSingleImage(frameBlocks)
          }
    }

    copyWith(
      Await.result(newImages, Duration.Inf)
    )
  }
}