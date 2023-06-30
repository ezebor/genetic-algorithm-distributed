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

case class Block(frameLocationId: Id, imageId: Int, pixelsSourceId: Id, fitness: Double)(implicit customRandom: Random = random) extends Gene {
  override def mutate: Gene = this

  override def toString: String = s"Block - frameLocationId: ($frameLocationId), image id: ${imageId}, pixels source id: ${pixelsSourceId}"
  
  def pixelWithFixedLocation(pixel: Pixel): Pixel = Pixel(
    pixel.x % DIMENSION_BLOCK_SIZE + frameLocationId._1,
    pixel.y % DIMENSION_BLOCK_SIZE + frameLocationId._2,
    pixel.argb
  )
}

case class Frame(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] => {
      val indexedBlocks = blocks.groupBy(_.frameLocationId)

      def mergeBlocks(newBlocks: List[Block]): List[Block] = {
        val mergedBlocks = newBlocks.foldLeft(indexedBlocks) { case (result, nextBlock) =>
          result.updated(nextBlock.frameLocationId, List(nextBlock))
        }
        mergedBlocks
          .values
          .toList
          .flatten
      }

      Frame(mergeBlocks(aBlocks))
    }

  protected override def calculateFitness: Double = blocks
    .foldLeft(0d) { case (totalFitness, Block(_, _, _, aFitness)) =>
      totalFitness + aFitness / blocks.size
    }

  override def crossoverWith(couple: Chromosome, crossoverLikelihood: Double): (List[Gene], List[Gene]) = super.crossoverWith(couple, crossoverLikelihood) match
    case (leftChildGenes: List[Block], rightChildGenes: List[Block]) => (blocks ::: leftChildGenes, blocks ::: rightChildGenes)
  
  override def mutate: Chromosome = this
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(aFrame: Frame) =>
      Image(Success(aFrame))
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
      .groupBy { case (frameLocationId, _, _) => frameLocationId}
      .toList

    val statisticsPerCoordinate = groupedCoordinates.map { case (frameLocationId, aCoordinates) =>
      val referencesPixels = referencesImmutableImages.keys.map { case imageId =>
        ImagesManager.pixelsAt(imageId, frameLocationId)
      }.toVector

      val targetPixels = aCoordinates.map { case (_, imageId, pixelsSourceId) =>
        ImagesManager.pixelsAt(imageId, pixelsSourceId)
      }.toVector

      val terms = generateStatisticsTerms(
        targetPixels,
        referencesPixels
      )

      (
        aCoordinates.toVector,
        luminance(terms),
        contrast(terms),
        structure(terms)
      )
    }

    statisticsPerCoordinate.flatMap { case (aCoordinates, luminance, contrast, structure) =>
      aCoordinates.indices.map{ index =>
        val aCoordinate = aCoordinates(index)
        val fitness = luminance(index) * contrast(index) * structure(index)
        val roundedFitness = BigDecimal(fitness).setScale(2, BigDecimal.RoundingMode.HALF_DOWN).toDouble
        Block(
          aCoordinate._1,
          aCoordinate._2,
          aCoordinate._3,
          roundedFitness
        )
      }
    }
  }

  def ssim(imageId: Int, pixelsSourceId: Id): Double = {
    val terms = generateStatisticsTerms(
      Vector(ImagesManager.pixelsAt(imageId, pixelsSourceId)),
      ImagesManager.referencesBlocks(pixelsSourceId).toVector.map { case Block(_, anImageId, aPixelsSourceId, _) =>
        ImagesManager.pixelsAt(anImageId, aPixelsSourceId)
      }
    )
    luminance(terms).head * contrast(terms).head * structure(terms).head
  }

  lazy val blockIds: IndexedSeq[Id] = Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE)
    .flatMap(x => (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE).map(_ => x))
    .zip(
      (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE)
        .flatMap(_ => Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE))
    )

  def toImmutableImage(blocks: List[Block]): ImmutableImage = {
    val immutableImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    for {
      case Block((locationX, locationY), imageId, pixelsSourceId @ (sourceX, sourceY), _) <- blocks
      aPixel <- ImagesManager.pixelsAt(imageId, pixelsSourceId)
    } yield {
      immutableImage.setPixel(Pixel(
        locationX + aPixel.x - sourceX,
        locationY + aPixel.y - sourceY,
        aPixel.argb
      ))
    }
    immutableImage
  }

  def toCoordinates(imageId: Int): List[Coordinate] = blockIds
    .map { case id: Id =>
      (id, imageId, id)
    }.toList

  lazy val referencesImmutableImages: Map[Int, ImmutableImage] = {
    val initialImmutableImages = List(
      ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE),
      ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    )

    val immutableImagesVariants = initialImmutableImages.flatMap { reference =>
      List(
        reference.rotateLeft(),
        reference.rotateLeft().rotateLeft(),
        reference.rotateLeft().rotateLeft().rotateLeft(),
        reference.flipX(),
        reference.flipX().rotateLeft(),
        reference.flipX().rotateLeft().rotateLeft(),
        reference.flipX().rotateLeft().rotateLeft().rotateLeft(),
      )
    }

    val images: List[ImmutableImage] = initialImmutableImages ::: immutableImagesVariants

    images
      .zipWithIndex
      .map { case (immutableImage, index) =>
        (index, immutableImage)
      }
      .toMap
  }

  lazy val referencesImages: List[Image] = {
    val coordinates = referencesImmutableImages
      .toList
      .flatMap { case (imageId, _) =>
        toCoordinates(imageId)
      }

    coordinatesToImages(coordinates)
  }

  def blocksToImages(blocks: List[Block]): List[Image] = blocks
    .groupBy(_.imageId)
    .map { case (_, blocks) =>
      Image(
        Success(
          Frame(
            blocks
          )
        )
      )
    }
    .toList

  def coordinatesToImages(coordinates: List[Coordinate]): List[Image] = blocksToImages(toBlocks(coordinates))

  lazy val referencesBlocks: Map[Id, List[Block]] = referencesImages
    .flatMap { case Image(Success(Frame(blocks))) =>
      blocks
    }
    .groupBy(_.frameLocationId)

  def initialPopulation(): ImagesPopulation = {
    ImagesPopulation(
      referencesImages.flatMap(image => (1 to POPULATION_SIZE / referencesImages.size).map(_ => image.copy(image.frame)))
    )
  }

  def pixelsAt(imageId: Int, pixelsSourceId: Id): Vector[Pixel] = referencesImmutableImages(imageId)
    .pixels(
      pixelsSourceId._1,
      pixelsSourceId._2,
      ExecutionScript.DIMENSION_BLOCK_SIZE,
      ExecutionScript.DIMENSION_BLOCK_SIZE
    )
    .toVector
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = ImagesPopulation(newIndividuals.map { case image: Image =>
    image
  })

  override def empty(): Population = ImagesPopulation(List[Image]())

  override def mutate(mutationLikelihood: Double): Population = {
    val blocks = super.mutate(mutationLikelihood).individuals.flatMap { case image: Image =>
      image.frame.get.blocks
    }

    val coordinates: List[Coordinate] = blocks
      .zip(random.shuffle[Block, IndexedSeq[Block]](blocks.toIndexedSeq))
      .map { case (Block(frameLocationId, imageId, _, _), Block(_, _, pixelsSourceId, _)) =>
        (frameLocationId, imageId, pixelsSourceId)
      }

    copyWith(
      ImagesManager.coordinatesToImages(coordinates)
    )
  }
}