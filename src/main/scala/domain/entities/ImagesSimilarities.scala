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

case class Block(pixels: Vector[Pixel])(implicit customRandom: Random = random) {

  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  def copyWith(otherBlock: Block): Block = Block(
    pixels.indices.map { index =>
      val thisPixel = pixels(index)
      val otherPixel = otherBlock.pixels(index)
      Pixel(thisPixel.x, thisPixel.y, otherPixel.argb)
    }.toVector
  )

  private def luminance(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(meanA, meanB, _, _, _) => (2 * meanA * meanB + C1) / (Math.pow(meanA, 2) + Math.pow(meanB, 2) + C1)
  private def contrast(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationB, _) => (2 * standardDeviationA * standardDeviationB + C2) / (Math.pow(standardDeviationA, 2) + Math.pow(standardDeviationB, 2) + C2)
  private def structure(terms: StatisticsTerms): Double = terms match
    case StatisticsTerms(_, _, standardDeviationA, standardDeviationB, covariance) => (covariance + C3) / (standardDeviationA * standardDeviationB + C3)

  case class StatisticsTerms(meanA: Double, meanB: Double, standardDeviationA: Double, standardDeviationB: Double, covariance: Double)
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

  def ssim(blockId: Int): Double = PersistenceManager
    .referencesBlocksAt(blockId)
    .foldLeft(0d) { (totalSsim, referenceBlock) =>
      val terms = generateStatisticsTerms(referenceBlock)
      totalSsim + luminance(terms) * contrast(terms) * structure(terms)
    }

  def mutateWith(otherBlock: Block): (Block, Block) = {
    val packOfPixels = pixels.zip(otherBlock.pixels)

    val mutatedPixels = packOfPixels.foldLeft((Vector[Pixel](), Vector[Pixel]())) { case ((pixelsLeft, pixelsRight), (nextPixelLeft, nextPixelRight)) =>
      (
        Pixel(nextPixelLeft.x, nextPixelLeft.y, nextPixelRight.argb) +: pixelsLeft,
        Pixel(nextPixelRight.x, nextPixelRight.y, nextPixelLeft.argb) +: pixelsRight
      )
    }

    (Block(mutatedPixels._1), Block(mutatedPixels._2))
  }

  lazy val size: Int = pixels.size
}

case class BlockCoordinates(imageId: Int, blockId: Int)(implicit customRandom: Random = random) extends Gene {

  lazy val block: Block = PersistenceManager.blockAt(imageId, blockId)

  lazy val ssim = block.ssim(blockId)

  override def mutate: Gene = BlockCoordinates(imageId, blockId)

  override def toString: String = s"Block coordinates: $imageId $blockId"
}

case class Frame(imageId: Int, blocksCoordinates: List[BlockCoordinates])(implicit customRandom: Random = random) extends Chromosome(blocksCoordinates)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocksCoordinates: List[BlockCoordinates] => Frame(imageId, aBlocksCoordinates)

  protected override def calculateFitness: Double = blocksCoordinates.foldLeft(0d) { (total, blockCoordinates) =>
    total + (blockCoordinates.ssim / blocksCoordinates.size)
  }

  override def mutate: Chromosome = {
    @tailrec
    def recursiveMutate(source1: IndexedSeq[BlockCoordinates], source2: IndexedSeq[BlockCoordinates]): Unit = {
      if(source1.nonEmpty && source2.nonEmpty) {
        val newBlocks: (Block, Block) = source1.head.block.mutateWith(source2.head.block)
        PersistenceManager.append(source1.head.blockId, newBlocks._1)
        PersistenceManager.append(source2.head.blockId, newBlocks._2)

        recursiveMutate(source1.tail, source2.tail)
      }
    }

    recursiveMutate(
      blocksCoordinates.toIndexedSeq,
      random.shuffle[BlockCoordinates, IndexedSeq[BlockCoordinates]](blocksCoordinates.toIndexedSeq)
    )

    super.mutate
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(Frame(imageId, blocksCoordinates)) =>
      Image(Success(Frame(imageId, blocksCoordinates)))
}

type DataModel = Map[Int, Map[Int, Block]]

object PersistenceManager {
  private val mutablePixelsDictionary: collection.mutable.Map[Int, collection.mutable.Map[Int, Block]] = collection.mutable.Map()
  private lazy val references: DataModel = ImagesManager.initialDataModel(immutableImages.size, immutableImages, currentId)
  private var currentId: Int = 1
  private val quantityOfPixels: Int = 550

  private val immutableImages: List[ImmutableImage] = List(
    // TODO: pasar a constantes el tamaño de imagen (proporcional al tamaño de bloque de 11x11)
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(quantityOfPixels, quantityOfPixels),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png").scaleTo(quantityOfPixels, quantityOfPixels)
  )

  def shuffledPixelsCoordinatesX: IndexedSeq[Int] = random.shuffle[Int, IndexedSeq[Int]](0 until quantityOfPixels)

  private def nextId(): Unit = currentId += 1

  private def imagesIds: List[Int] = mutablePixelsDictionary.keys.toList

  private def coordinatesOf(imageId: Int): List[BlockCoordinates] = (for {
    (blockId, _) <- blocksOf(imageId)
  } yield BlockCoordinates(imageId, blockId)).toList
  def blocksOf(imageId: Int): collection.mutable.Map[Int, Block] = mutablePixelsDictionary.getOrElse(imageId, collection.mutable.Map())
  def blockAt(imageId: Int, blockId: Int): Block = blocksOf(imageId).getOrElse(blockId, Block(Vector())())

  def referencesBlocksAt(blockId: Int): List[Block] = references
    .values
    .map(_.getOrElse(blockId, Block(Vector())()))
    .toList

  def append(imageId: Int, blockId: Int, block: Block): Unit = {
    if(!mutablePixelsDictionary.contains(imageId)) mutablePixelsDictionary += (imageId -> collection.mutable.Map())
    val blocksEntry = blocksOf(imageId)
    blocksEntry += (blockId -> block)
    mutablePixelsDictionary += (imageId -> blocksEntry)
  }

  def append(blockId: Int, block: Block): Unit = {
    nextId()
    append(currentId, blockId, block)
  }

  def create(dataModel: DataModel): Unit = {
    mutablePixelsDictionary.clear()
    currentId = 0
    dataModel.keys.foreach { case imageId =>
      val blocks = dataModel(imageId)
      blocks.foreach { case (blockId, block) =>
        append(imageId, blockId, block)
      }
      nextId()
    }
  }

  private def toDataModel(population: ImagesPopulation): DataModel = {
    population.individuals.foldLeft(Map[Int, Map[Int, Block]]()) { case (result, Image(Success(Frame(imageId, blocksCoordinates)))) =>
      val blocksEntries: Map[Int, Block] = blocksCoordinates.foldLeft(result.getOrElse(imageId, Map())) { case (result, blockCoordinates @ BlockCoordinates(_, blockId)) =>
        result.updated(blockId, blockCoordinates.block)
      }
      result.updated(imageId, blocksEntries)
    }
  }

  def append(images: List[Image]): List[Image] = {
    images.map { case Image(Success(Frame(_, blocksCoordinates))) =>
      nextId()
      Image(Success(Frame(
        currentId,
        blocksCoordinates.map { case aBlockCoordinates @ BlockCoordinates(_, blockId) =>
          PersistenceManager.append(currentId, blockId, aBlockCoordinates.block)
          BlockCoordinates(currentId, blockId)
        }
      )))
    }
  }

  def population(): ImagesPopulation = {
    ImagesPopulation(
      PersistenceManager.imagesIds.map(imageId =>
        Image(
          Success(
            Frame(imageId, PersistenceManager.coordinatesOf(imageId))
          )
        )
      )
    )
  }

  private def save(dataModel: DataModel): ImagesPopulation = {
    create(dataModel)
    population()
  }

  def save(imagesPopulation: ImagesPopulation): ImagesPopulation = {
    save(PersistenceManager.toDataModel(imagesPopulation))
  }

  def createInitialPopulation(populationSize: Int): ImagesPopulation = {
    save(ImagesManager.initialDataModel(populationSize, immutableImages, currentId))
  }
}

object ImagesManager {
  private def intoPixelsChunks(immutableImage: ImmutableImage, chunkSize: Int = 11): Vector[Block] = {
    def sortAndGroupDimension(dimension: Vector[Int]): Vector[Vector[Int]] =
      Set
        .from(dimension)
        .toVector
        .sortWith((a, b) => a <= b)
        .grouped(chunkSize)
        .toVector

    lazy val orderedPixelsTable: (Vector[Vector[Int]], Vector[Vector[Int]]) = {
      val table: (Vector[Int], Vector[Int]) = immutableImage.pixels().foldLeft((Vector[Int](), Vector[Int]())) { (table, pixel) =>
        (pixel.x +: table._1, pixel.y +: table._2)
      }

      (sortAndGroupDimension(table._1), sortAndGroupDimension(table._2))
    }

    for {
      blockX <- orderedPixelsTable._1
      blockY <- orderedPixelsTable._2
      positionsBlock = blockX.flatMap(index => (1 to chunkSize).map(_ => index)).zip(blockY.flatMap(_ => blockY))
    } yield {
      Block(positionsBlock.map((x, y) => immutableImage.pixel(x, y)))
    }
  }

  def initialDataModel(populationSize: Int, references: List[ImmutableImage], initialId: Int): DataModel = {
    val imagesPixels = references
      .map(immutableImage => intoPixelsChunks(immutableImage))
      .zip((1 to populationSize).grouped(populationSize / references.size))
      .flatMap { case (blocks, timesToRepeat) =>
        timesToRepeat.map(_ => blocks)
      }

    imagesPixels.foldLeft((initialId, Map(): DataModel)){ case ((currentId, result), blocks) =>
      val partialResult = blocks
        .zipWithIndex
        .foldLeft(result) { case (partialResult, (block, blockId)) =>
          val blockEntry: Map[Int, Block] = partialResult.getOrElse(currentId, Map())
          partialResult.updated(currentId, blockEntry.updated(blockId, block))
        }

      (currentId + 1, partialResult)
    }._2
  }
}

case class ImagesPopulation(images: List[Image]) extends Population(images) {
  override def copyWith(newIndividuals: List[Individual]): Population = newIndividuals match
    case newImages: List[Image] => ImagesPopulation(newImages)

  private lazy val inMemoryImages: List[Image] = images.map {
    case Image(Success(Frame(imageId, _))) =>
      val indexedBlocks: collection.mutable.Map[Int, Block] = PersistenceManager.blocksOf(imageId)
      Image(Success(Frame(imageId, indexedBlocks.keys.map(blockId => BlockCoordinates(imageId, blockId)).toList)))
  }

  override def individuals: List[Individual] = inMemoryImages

  override def selectStrongerPopulation(size: Int): Population = super.selectStrongerPopulation(size) match
    case population: ImagesPopulation => PersistenceManager.save(population)

  override def crossoverWith(otherPopulation: Population, crossoverLikelihood: Double): Population = {
    super.crossoverWith(otherPopulation, crossoverLikelihood) match
      case children: ImagesPopulation => copyWith(PersistenceManager.append(children.images))
  }
}