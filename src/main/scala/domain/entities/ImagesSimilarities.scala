package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.util.{Random, Success, Try}

// TODO: convertir lista de píxeles a lista de posiciones (en vector, List[Int])
// TODO: levantar la imagen en el individuo y obtener de ahí los pixeles via las posiciones del bloque
case class Block(pixels: List[Pixel])(implicit customRandom: Random = random) extends Gene {

  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  lazy val mean: Double = pixels.foldLeft(0)((total, aPixel) => total + aPixel.average()) / size
  lazy val standardDeviation: Double = Math.sqrt(pixels.map(pixel => Math.pow(pixel.average() - mean, 2)).sum / (size - 1))
  private lazy val covariance: Block => Double = { reference =>
    val referenceValues = reference.values.toArray
    val selfValues = values.toArray
    val valuesSum = referenceValues.indices.map(index => (referenceValues(index) - reference.mean) * (selfValues(index) - mean)).sum
    valuesSum / (size - 1)
  }

  override def mutate: Gene = Block(
    pixels
    .map(pixel => (customRandom.nextDouble(), pixel))
    .sortWith { case ((randomA, _), (randomB, _)) => randomA <= randomB}
    .map((_, pixel) => pixel)
  )

  def luminance(reference: Block): Double = (2 * mean * reference.mean + C1) / (Math.pow(mean, 2) + Math.pow(reference.mean, 2) + C1)
  def contrast(reference: Block): Double = (2 * standardDeviation * reference.standardDeviation + C2) / (Math.pow(standardDeviation, 2) + Math.pow(reference.standardDeviation, 2) + C2)
  def structure(reference: Block): Double = (covariance(reference) + C3) / (standardDeviation * reference.standardDeviation + C3)

  def ssim: Block => Double = { referenceBlock =>
    luminance(referenceBlock) * contrast(referenceBlock) * structure(referenceBlock)
  }

  def index: (Int, Int) = {
    val head = pixels.head
    (head.x, head.y)
  }

  def size: Int = pixels.size

  def values: List[Int] = pixels.map(_.average())

  override def toString: String = s"Block: $index"
}

case class Frame(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] =>
      Frame(aBlocks
        .map(block => block.index -> block)
        .toMap
        .values
        .toList
      )(customRandom)

  protected override def calculateFitness: Double = blocks.foldLeft(0d) {(total, aBlock) =>
    val referencesBlocks = ReferencesManager.indexedBlocks.getOrElse(aBlock.index, List())
    total + (referencesBlocks.map(aBlock.ssim).sum / aBlock.size)
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case aFrame: Success[Frame] => Image(aFrame)(customRandom)
}

// TODO: refactor de nombre de variables + optimización de intoBlocks (y poner esa función acá dentro)
object ReferencesManager {
  private def intoBlocks(immutableImage: ImmutableImage, blockSize: Int = 11): List[Block] = {
    def dimensionOrderedIndexes(dimension: Pixel => Int): List[List[Int]] = Set
      .from(immutableImage.pixels().map(dimension))
      .toList
      .sortWith((a, b) => a <= b)
      .grouped(blockSize).toList

    val rows: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.x)
    val columns: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.y)

    for {
      blockX <- rows
      blockY <- columns
      positionsBlock = blockX.flatMap(index => (1 to blockSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      Block(positionsBlock.map((x, y) => immutableImage.pixel(x, y)))
    }
  }

  private val immutableImages: List[ImmutableImage] = List(
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png"),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png")
  )

  private lazy val imagesBlocks: List[List[Block]] = immutableImages.map(immutableImage => intoBlocks(immutableImage))

  lazy val indexedBlocks: Map[(Int, Int), List[Block]] = {
    val indexedBlocksList = for {
      imageBlocks <- imagesBlocks
      block <- imageBlocks
    } yield {
      block.index -> block
    }

    indexedBlocksList.foldLeft(Map(): Map[(Int, Int), List[Block]]) { (result, indexedBlock) =>
      result.updated(indexedBlock._1, indexedBlock._2 :: result.getOrElse(indexedBlock._1, List()))
    }
  }

  def population(populationSize: Int): Population = {
    Population(
      imagesBlocks.flatMap(imageBlocks => (1 to populationSize / imagesBlocks.size).map(_ => Image(Success(Frame(imageBlocks)))))
    )
  }
}