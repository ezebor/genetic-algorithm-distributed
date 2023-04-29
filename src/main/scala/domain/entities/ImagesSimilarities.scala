package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.util.{Random, Success, Try}

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
    val referenceValues = reference.pixels.map(_.average()).toArray
    val selfValues = pixels.map(_.average()).toArray
    val valuesSum = referenceValues.indices.map(index => (referenceValues(index) - reference.mean) * (selfValues(index) - mean)).sum
    valuesSum / (size - 1)
  }

  def mutate: Block = {
    val newPixels: List[Pixel] = pixels
      .map(pixel => (customRandom.nextDouble(), pixel))
      .sortWith { case ((randomA, _), (randomB, _)) => randomA <= randomB}
      .map((_, pixel) => pixel)

    Block(newPixels)
  }

  def luminance(reference: Block): Double = (2 * mean * reference.mean + C1) / (Math.pow(mean, 2) + Math.pow(reference.mean, 2) + C1)
  def contrast(reference: Block): Double = (2 * standardDeviation * reference.standardDeviation + C2) / (Math.pow(standardDeviation, 2) + Math.pow(reference.standardDeviation, 2) + C2)
  def structure(reference: Block): Double = (covariance(reference) + C3) / (standardDeviation * reference.standardDeviation + C3)

  def ssim: Block => Double = { reference =>
    luminance(reference) * contrast(reference) * structure(reference)
  }

  lazy val size: Int = pixels.size
}

case class BlockCoordinates(imageId: Int, blockId: Int)(implicit customRandom: Random = random) extends Gene {

  lazy val block: Block = ReferencesManager.blockAt(this)
  lazy val mean: Double = block.mean
  lazy val standardDeviation: Double = block.standardDeviation

  override def mutate: Gene = {
    ReferencesManager.updatePixelsDictionary(this, block.mutate)
  }

  override def toString: String = s"Block coordinates: $imageId $blockId"
}

case class Frame(blockCoordinates: List[BlockCoordinates])(implicit customRandom: Random = random) extends Chromosome(blockCoordinates)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocksCoordinates: List[BlockCoordinates] =>
      Frame(ReferencesManager.updatePixelsDictionary(aBlocksCoordinates))(customRandom)

  protected override def calculateFitness: Double = blockCoordinates.foldLeft(0d) { (total, blockCoordinates) =>
    val referencesBlocks: List[Block] = ReferencesManager.referencesBlocksAt(blockCoordinates)
    total + (referencesBlocks.map(referenceBlock => referenceBlock.ssim(blockCoordinates.block)).sum / blockCoordinates.block.size)
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case aFrame: Success[Frame] => Image(aFrame)(customRandom)
}

object ReferencesManager {
  private def intoPixelsChunks(immutableImage: ImmutableImage, chunkSize: Int = 11): List[List[Pixel]] = {
    def dimensionOrderedIndexes(dimension: Pixel => Int): List[List[Int]] = Set
      .from(immutableImage.pixels().map(dimension))
      .toList
      .sortWith((a, b) => a <= b)
      .grouped(chunkSize).toList

    val rows: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.x)
    val columns: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.y)

    for {
      blockX <- rows
      blockY <- columns
      positionsBlock = blockX.flatMap(index => (1 to chunkSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      positionsBlock.map((x, y) => immutableImage.pixel(x, y))
    }
  }

  private val immutableImages: List[ImmutableImage] = List(
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png"),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png")
  )

  var mutablePixelsDictionary: collection.mutable.Map[Int, Map[Int, List[Pixel]]] = collection.mutable.Map()
  lazy val pixelsReferences: Map[Int, Map[Int, List[Pixel]]] = immutablePixelsDictionary(immutableImages.size)

  private def immutablePixelsDictionary(populationSize: Int): Map[Int, Map[Int, List[Pixel]]] = {
    val imagesPixels = immutableImages
      .map(immutableImage => intoPixelsChunks(immutableImage))
      .zip((0 until populationSize).grouped(populationSize / immutableImages.size))

    val preliminaryDictionary = (for {
      (image, range) <- imagesPixels
    } yield {
      image.indices.flatMap(blockId => range.map(imageId => (imageId -> (blockId -> image(blockId)))))
    }).flatten

    preliminaryDictionary.foldLeft(Map(): Map[Int, Map[Int, List[Pixel]]]) {(result, nextEntry) =>
      val block: Map[Int, List[Pixel]] = result.getOrElse(nextEntry._1, Map())
      result.updated(nextEntry._1, block.updated(nextEntry._2._1, nextEntry._2._2))
    }
  }

  def population(populationSize: Int): Population = {
    mutablePixelsDictionary.addAll(immutablePixelsDictionary(populationSize))
    Population(
      (for {
        imageId <- mutablePixelsDictionary.keys
      } yield {
        val blocks: List[BlockCoordinates] = mutablePixelsDictionary(imageId)
          .toList
          .map((blockId, _) => BlockCoordinates(imageId, blockId))
        Image(Success(Frame(blocks)))
      }).toList
    )
  }

  def updatePixelsDictionary(blockCoordinates: BlockCoordinates, block: Block): BlockCoordinates = {
    mutablePixelsDictionary(blockCoordinates.imageId).updated(blockCoordinates.blockId, block.pixels)
    blockCoordinates
  }

  def updatePixelsDictionary(blocksCoordinates: List[BlockCoordinates]): List[BlockCoordinates] = {
    blocksCoordinates.map(blockCoordinates => updatePixelsDictionary(blockCoordinates, blockCoordinates.block))
  }

  def blockAt(coordinates: BlockCoordinates): Block = Block(mutablePixelsDictionary(coordinates.imageId)(coordinates.blockId))

  def referencesBlocksAt(coordinates: BlockCoordinates): List[Block] = {
    pixelsReferences
      .values
      .map(_.getOrElse(coordinates.blockId, List()))
      .map(pixels => Block(pixels))
      .toList
  }
}