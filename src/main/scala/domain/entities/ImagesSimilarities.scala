package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE}

import scala.annotation.tailrec
import scala.util.{Random, Success, Try}

case class Block(pixels: Vector[Pixel])(implicit customRandom: Random = random) extends Gene {

  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  def id: (Int, Int) = (pixels.head.x, pixels.head.y)

  private def luminance(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(meanA, meanB, _, _, _) => (2 * meanA * meanB + C1) / (Math.pow(meanA, 2) + Math.pow(meanB, 2) + C1)
  private def contrast(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationB, _) => (2 * standardDeviationA * standardDeviationB + C2) / (Math.pow(standardDeviationA, 2) + Math.pow(standardDeviationB, 2) + C2)
  private def structure(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationB, covariance) => (covariance + C3) / (standardDeviationA * standardDeviationB + C3)

  private case class StatisticsTerms(meanA: Double, meanB: Double, standardDeviationA: Double, standardDeviationB: Double, covariance: Double)

  private def generateStatisticsTerms(blockB: Block): StatisticsTerms = {
    val pixelsA = pixels
    val pixelsB = blockB.pixels

    // TODO: hacer 2 loops: uno para calcular las medias y otro para calcular sigma y covarianza
    val terms = pixels.indices.foldLeft((0d, 0d, 0d, 0d, 0d)) { case ((sumPixelsA, sumPixelsB, sumSquarePixelsA, sumSquarePixelsB, covarianceNumerator), index) =>
      (
        sumPixelsA + pixelsA(index).argb,
        sumPixelsB + pixelsB(index).argb,
        sumSquarePixelsA + Math.pow(pixelsA(index).argb, 2),
        sumSquarePixelsB + Math.pow(pixelsB(index).argb, 2),
        covarianceNumerator + (pixelsA(index).argb - sumPixelsA / size) * (pixelsB(index).argb - sumPixelsB / size)
      )
    }

    terms match
      case (sumPixelsA, sumPixelsB, sumSquarePixelsA, sumSquarePixelsB, covarianceNumerator) => StatisticsTerms(
        sumPixelsA / size,
        sumPixelsB / size,
        Math.sqrt((sumSquarePixelsA / (size - 1)) - (Math.pow(sumPixelsA, 2) / (Math.pow(size, 2) - size))),
        Math.sqrt((sumSquarePixelsB / (size - 1)) - (Math.pow(sumPixelsB, 2) / (Math.pow(size, 2) - size))),
        covarianceNumerator / (size - 1)
    )
  }

  lazy val ssim: Double = ImagesManager.referencesBlocks(id)
    .foldLeft(0d) { (totalSsim, referenceBlock) =>
      val terms = generateStatisticsTerms(referenceBlock)
      totalSsim + luminance(terms) * contrast(terms) * structure(terms)
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
    .map(aBlock => aBlock.ssim / blocks.size)
    .sum

  protected override def calculateFitness: Double = ssim / blocks.size

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
  lazy val referencesImages: List[Image] = {
    def toBlocks(immutableImage: ImmutableImage): List[Block] = {
      (for {
        x <- Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE)
        y <- Range(0, DIMENSION_IMAGE_SIZE, DIMENSION_BLOCK_SIZE)
      } yield {
        Block(immutableImage
          .subimage(x, y, DIMENSION_BLOCK_SIZE, DIMENSION_BLOCK_SIZE)
          .pixels()
          .map(aPixel => Pixel(aPixel.x + x, aPixel.y + y, aPixel.argb))
          .toVector)
      }).toList
    }

    val immutableImages = List(
      ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE),
      ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png").scaleTo(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
    )

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

  def initialPopulation(populationSize: Int): ImagesPopulation = {
    ImagesPopulation(
      referencesImages.flatMap(image => (1 to populationSize / referencesImages.size).map(_ => image.copy(image.frame)))
    )
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = ImagesPopulation(newIndividuals.map { case image: Image =>
    image
  })
}