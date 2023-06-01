package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE, POPULATION_SIZE}

import scala.annotation.tailrec
import scala.util.{Random, Success, Try}

case class Block(pixels: Vector[Pixel])(implicit customRandom: Random = random) extends Gene {

  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  def id: (Int, Int) = {
    val firstPixel = pixels.sortWith((p1, p2) => p1.x <= p2.x && p1.y <= p2.y).head
    (firstPixel.x, firstPixel.y)
  }

  private def luminance(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(meanA, meansB, _, _, _) => meansB.map { meanB =>
      (2 * meanA * meanB + C1) / (Math.pow(meanA, 2) + Math.pow(meanB, 2) + C1)
    }.sum
  private def contrast(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationsB, _) => standardDeviationsB.map { standardDeviationB =>
      (2 * standardDeviationA * standardDeviationB + C2) / (Math.pow(standardDeviationA, 2) + Math.pow(standardDeviationB, 2) + C2)
    }.sum
  private def structure(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationsB, covariances) => standardDeviationsB.indices.map { referenceIndex =>
      (covariances(referenceIndex) + C3) / (standardDeviationA * standardDeviationsB(referenceIndex) + C3)
    }.sum

  private case class StatisticsTerms(meanA: Double, meansB: Vector[Double], standardDeviationA: Double, standardDeviationsB: Vector[Double], covariances: Vector[Double])

  private def generateStatisticsTerms(blocksB: Vector[Block]): StatisticsTerms = {
    val pixelsA = pixels
    val pixelsB = blocksB.map(_.pixels)

    val initialValues = pixelsB.map(_ => 0d)

    val (sumPixelsA, sumPixelsB) = pixels.indices.foldLeft((0d, initialValues)) { case ((totalSumPixelsA, totalSumPixelsB), index) =>
      (
        totalSumPixelsA + pixelsA(index).argb,
        pixelsB
          .indices
          .map(referenceIndex => totalSumPixelsB(referenceIndex) + pixelsB(referenceIndex)(index).argb)
          .toVector
      )
    }

    val meanA = sumPixelsA / size
    val meansB = sumPixelsB.map(sumB => sumB / size)

    val (varianceA, variancesB, covariances) = pixels.indices.foldLeft((0d, initialValues, initialValues)) { case ((totalVarianceA, totalVariancesB, totalCovariances), index) =>
      (
        totalVarianceA + Math.pow(pixelsA(index).argb - meanA, 2) / (size - 1),
        pixelsB
          .indices
          .map(referenceIndex => totalVariancesB(referenceIndex) + Math.pow(pixelsB(referenceIndex)(index).argb - meansB(referenceIndex), 2) / (size - 1))
          .toVector,
        pixelsB
          .indices
          .map(referenceIndex => totalCovariances(referenceIndex) + (pixelsA(index).argb - meanA) * (pixelsB(referenceIndex)(index).argb - meansB(referenceIndex)) / (size - 1))
          .toVector
      )
    }

    StatisticsTerms(
      meanA,
      meansB,
      Math.sqrt(varianceA),
      variancesB.map(varianceB => Math.sqrt(varianceB)),
      covariances,
    )
  }

  lazy val ssim: Double = {
    val terms = generateStatisticsTerms(ImagesManager.referencesBlocks(id).toVector)
    luminance(terms) * contrast(terms) * structure(terms)
  }

  def mutateWith(otherBlock: Block): Block = {
    val packOfPixels = pixels.zip(otherBlock.pixels)

    val mutatedPixels = packOfPixels.foldLeft(Vector[Pixel]()) { case (result, (nextPixelLeft, nextPixelRight)) =>
      Pixel(nextPixelLeft.x, nextPixelLeft.y, nextPixelRight.argb) +: result
    }

    Block(mutatedPixels)
  }

  lazy val size: Int = pixels.size

  override def mutate: Gene = this

  override def toString: String = s"Block id: ($id)"
}

case class Frame(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] => Frame(aBlocks)

  lazy val ssim: Double = blocks
    .map(_.ssim)
    .sum

  protected override def calculateFitness: Double = ssim / blocks.size

  override def crossoverWith(couple: Chromosome, crossoverLikelihood: Double): (List[Gene], List[Gene]) = super.crossoverWith(couple, crossoverLikelihood) match
    case (leftChildGenes: List[Block], rightChildGenes: List[Block]) => (blocks ::: leftChildGenes, blocks ::: rightChildGenes)

  override def mutate: Chromosome = {
    @tailrec
    def recursiveMutate(source1: IndexedSeq[Block], source2: IndexedSeq[Block], sink: List[Block]): List[Block] = {
      if(source1.isEmpty || source2.isEmpty) return sink

      val mutatedBlock: Block = source1.head.mutateWith(source2.head)
      recursiveMutate(source1.tail, source2.tail, mutatedBlock :: sink)
    }

    val mutatedPixels = recursiveMutate(
      blocks.toIndexedSeq,
      random.shuffle[Block, IndexedSeq[Block]](blocks.toIndexedSeq),
      List()
    )

    copyWith(mutatedPixels)
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(aFrame: Frame) =>
      Image(Success(aFrame))
}

object ImagesManager {
  lazy val blockIds: IndexedSeq[(Int, Int)] = Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE)
    .flatMap(x => (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE).map(_ => x))
    .zip(
      (1 to DIMENSION_IMAGE_SIZE / DIMENSION_BLOCK_SIZE)
        .flatMap(_ => Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE))
    )

  def toImmutableImage(blocks: List[Block]): ImmutableImage = {
    val immutableImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    for {
      aBlock <- blocks
      aPixel <- aBlock.pixels
    } yield {
      immutableImage.setPixel(aPixel)
    }
    immutableImage
  }

  def toBlocks(immutableImage: ImmutableImage): List[Block] = blockIds
    .map { case (x, y) =>
      Block(immutableImage
        .subimage(x, y, DIMENSION_BLOCK_SIZE, DIMENSION_BLOCK_SIZE)
        .pixels()
        .map(aPixel => Pixel(aPixel.x + x, aPixel.y + y, aPixel.argb))
        .toVector)
    }.toList

  lazy val referencesImages: List[Image] = {
    val immutableImages = {
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

      initialImmutableImages ::: immutableImagesVariants
    }

    immutableImages.map(
      immutableImage =>
        Image(
          Success(
            Frame(
              toBlocks(immutableImage)
            )
          )
        )
    )
  }

  lazy val referencesBlocks: Map[(Int, Int), List[Block]] = referencesImages
    .flatMap { case Image(Success(Frame(blocks))) =>
      blocks
    }
    .groupBy(_.id)

  def initialPopulation(): ImagesPopulation = {
    ImagesPopulation(
      referencesImages.flatMap(image => (1 to POPULATION_SIZE / referencesImages.size).map(_ => image.copy(image.frame)))
    )
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = ImagesPopulation(newIndividuals.map { case image: Image =>
    image
  })

  override def empty(): Population = ImagesPopulation(List[Image]())
}