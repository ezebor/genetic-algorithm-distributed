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

  override def mutate: Gene = BlockCoordinates(imageId, blockId)

  override def toString: String = s"Block coordinates: $imageId $blockId"
}

case class Frame(imageId: Int, blocksCoordinates: List[BlockCoordinates])(implicit customRandom: Random = random) extends Chromosome(blocksCoordinates)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocksCoordinates: List[BlockCoordinates] => Frame(imageId, aBlocksCoordinates)


  protected override def calculateFitness: Double = blocksCoordinates.foldLeft(0d) { (total, blockCoordinates) =>
    val referencesBlocks: List[Block] = ReferencesManager.referencesBlocksAt(blockCoordinates)
    total + (referencesBlocks.map(referenceBlock => referenceBlock.ssim(blockCoordinates.block)).sum / blockCoordinates.block.size)
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(Frame(imageId, blocksCoordinates)) =>
      Image(Success(Frame(imageId, blocksCoordinates)))
}

object ReferencesManager {
  private def intoPixelsChunks(immutableImage: ImmutableImage, chunkSize: Int = 11): List[Block] = {
    def dimensionOrderedIndexes(dimension: Pixel => Int): List[List[Int]] = Set
      .from(immutableImage.pixels().map(dimension))
      .toList
      .sortWith((a, b) => a <= b)
      .grouped(chunkSize).toList

    // TODO: optimizar usando for comprehension con tuplas, evitando recorrer 2 veces los píxeles de la imagen con dimensionOrderedIndexes (que devuelva la tupla (x, y))
    val rows: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.x)
    val columns: List[List[Int]] = dimensionOrderedIndexes(pixel => pixel.y)

    for {
      blockX <- rows
      blockY <- columns
      positionsBlock = blockX.flatMap(index => (1 to chunkSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      Block(positionsBlock.map((x, y) => immutableImage.pixel(x, y)))
    }
  }

  private val immutableImages: List[ImmutableImage] = List(
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png"),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png")
  )

  var mutablePixelsDictionary: collection.mutable.Map[Int, collection.mutable.Map[Int, Block]] = collection.mutable.Map()
  lazy val pixelsReferences: Map[Int, Map[Int, Block]] = immutablePixelsDictionary(immutableImages.size)
  var currentId: Int = 1
  def nextId: Unit = currentId += 1

  private def immutablePixelsDictionary(populationSize: Int): Map[Int, Map[Int, Block]] = {
    val imagesPixels = immutableImages
      .map(immutableImage => intoPixelsChunks(immutableImage))
      .zip((1 to populationSize).grouped(populationSize / immutableImages.size))
      .flatMap { case (blocks, timesToRepeat) =>
        timesToRepeat.map(_ => blocks)
      }

    imagesPixels.foldLeft(Map(): Map[Int, Map[Int, Block]]) { case (result, blocks) =>
      val partialResult = blocks
        .zipWithIndex
        .foldLeft(result) { case (partialResult, (block, blockId)) =>
          val blockEntry: Map[Int, Block] = partialResult.getOrElse(currentId, Map())
          partialResult.updated(currentId, blockEntry.updated(blockId, block))
        }
      nextId
      partialResult
    }
  }

  // TODO: optimizar
  def population(populationSize: Int): Population = {
    val immutableDictionary = immutablePixelsDictionary(populationSize)
    immutableDictionary.keys.foreach { case imageId =>
      if(!mutablePixelsDictionary.contains(imageId)) mutablePixelsDictionary += (imageId -> collection.mutable.Map())

      val blocks = immutableDictionary(imageId)
      blocks.foreach { case (blockId, block) =>
        mutablePixelsDictionary(imageId) += (blockId -> block)
      }
    }

    ImagesPopulation(
      (for {
        imageId <- mutablePixelsDictionary.keys
      } yield {
        val blocks: List[BlockCoordinates] = mutablePixelsDictionary(imageId)
          .toList
          .map((blockId, _) => BlockCoordinates(imageId, blockId))
        Image(Success(Frame(imageId, blocks)))
      }).toList
    )
  }

  def addBlock(imageId: Int, blockId: Int, block: Block): Unit = {
    if(!mutablePixelsDictionary.contains(imageId)) mutablePixelsDictionary += (imageId -> collection.mutable.Map())
    val blocksEntry = mutablePixelsDictionary(imageId)
    blocksEntry += (blockId -> block)
    mutablePixelsDictionary += (imageId -> blocksEntry)
  }

  def blocksCoordinatesOf(imageId: Int): List[BlockCoordinates] = {
    mutablePixelsDictionary(imageId)
      .keys
      .map(blockId => BlockCoordinates(imageId, blockId))
      .toList
  }

  def blockAt(coordinates: BlockCoordinates): Block = mutablePixelsDictionary(coordinates.imageId)(coordinates.blockId)

  def referencesBlocksAt(coordinates: BlockCoordinates): List[Block] = {
    pixelsReferences
      .values
      .map(_.getOrElse(coordinates.blockId, Block(List())))
      .toList
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = newIndividuals match
    case images: List[Image] => ImagesPopulation(images)

  override def individuals: List[Individual] = images.map { case Image(Success(Frame(imageId, _))) =>
    val indexedBlocks: collection.mutable.Map[Int, Block] = ReferencesManager.mutablePixelsDictionary(imageId)
    Image(Success(Frame(imageId, indexedBlocks.keys.map(blockId => BlockCoordinates(imageId, blockId)).toList)))
  }

  override def crossoverWith(otherPopulation: Population, crossoverLikelihood: Double): Population = {
    // TODO: generar los children a partir de los padres para que no queden huecos negros en los children
    super.crossoverWith(otherPopulation, crossoverLikelihood) match
      case children: ImagesPopulation => {
        children.copyWith(
          children.images.map { case Image(Success(Frame(_, blocksCoordinates))) =>
            ReferencesManager.nextId
            val newBlocksCoordinates = blocksCoordinates.map { case blocksCoordinates @ BlockCoordinates(_, blockId) =>
              ReferencesManager.addBlock(ReferencesManager.currentId, blockId, blocksCoordinates.block)
              BlockCoordinates(ReferencesManager.currentId, blockId)
            }
            Image(Success(Frame(ReferencesManager.currentId, newBlocksCoordinates)))
          }
        )
      }
  }
}