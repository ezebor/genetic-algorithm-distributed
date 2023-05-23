package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

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

    val terms = pixels.indices.foldLeft((0d, 0d, 0d, 0d, 0d)) { case ((sumPixelsA, sumSquarePixelsA, sumPixelsB, sumSquarePixelsB, sumPixelAByPixelB), index) =>
      (
        sumPixelsA + pixelsA(index).average(),
        sumSquarePixelsA + Math.pow(pixelsA(index).average(), 2),
        sumPixelsB + pixelsB(index).average(),
        sumSquarePixelsB + Math.pow(pixelsB(index).average(), 2),
        sumPixelAByPixelB + pixelsA(index).average() * pixelsB(index).average()
      )
    }

    terms match
      case (sumPixelsA, sumSquarePixelsA, sumPixelsB, sumSquarePixelsB, sumPixelAByPixelB) => StatisticsTerms(
        sumPixelsA / size,
        sumPixelsB / size,
        Math.sqrt((sumSquarePixelsA / (size - 1)) - (Math.pow(sumPixelsA, 2) / (Math.pow(size, 2) - size))),
        Math.sqrt((sumSquarePixelsB / (size - 1)) - (Math.pow(sumPixelsB, 2) / (Math.pow(size, 2) - size))),
        (sumPixelAByPixelB / (size - 1)) - (sumPixelsA * sumPixelsB / (Math.pow(size, 2) - size))
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
    .map(_.ssim)
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
  val immutableImages: List[ImmutableImage] = List(
    // TODO: pasar a constantes el tamaño de imagen (proporcional al tamaño de bloque de 11x11)
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(110, 110),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png").scaleTo(110, 110)
  )

  lazy val referencesBlocks: Map[(Int, Int), List[Block]] = references
    .flatten
    .groupBy(_.id)

  lazy val references: List[List[Block]] = immutableImages
    .map(immutableImage => ImagesManager.blocksOf(immutableImage).values.toList)

  def blocksOf(immutableImage: ImmutableImage, blockDimensionSize: Int = 11): Map[(Int, Int), Block] = {
    def sortAndGroupDimension(dimension: Vector[Int]): Vector[Vector[Int]] =
      Set
        .from(dimension)
        .toVector
        .sortWith((a, b) => a <= b)
        .grouped(blockDimensionSize)
        .toVector

    lazy val orderedPixelsTable: (Vector[Vector[Int]], Vector[Vector[Int]]) = {
      val table: (Vector[Int], Vector[Int]) = immutableImage.pixels().foldLeft((Vector[Int](), Vector[Int]())) { (table, pixel) =>
        (pixel.x +: table._1, pixel.y +: table._2)
      }

      (sortAndGroupDimension(table._1), sortAndGroupDimension(table._2))
    }

    val blocks = for {
      blockX <- orderedPixelsTable._1
      blockY <- orderedPixelsTable._2
      positionsBlock = blockX.flatMap(index => (1 to blockDimensionSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      Block(positionsBlock.map((x, y) => immutableImage.pixel(x, y)))
    }

    blocks.map(block => (block.id, block)).toMap
  }

  def initialPopulation(populationSize: Int): ImagesPopulation = {
    val imagesPixels = references
      .zip((1 to populationSize).grouped(populationSize / references.size))
      .flatMap { case (blocks, timesToRepeat) =>
        timesToRepeat.map(_ => blocks)
      }

    ImagesPopulation(
      imagesPixels.map(blocks =>
        Image(
          Success(
            Frame(
              blocks.map(aBlock => Block(aBlock.pixels))
            )
          )
        )
      )
    )
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = ImagesPopulation(newIndividuals.map { case image: Image =>
    image
  })
}