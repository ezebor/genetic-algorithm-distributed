package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

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

  def mutate: Block = {
    // TODO: optimizar para no recorrer 2 veces el vector de píxeles (shuffle de random)
    val newPixels: Vector[Pixel] = pixels
      .map(pixel => (customRandom.nextDouble(), pixel))
      .sortWith { case ((randomA, _), (randomB, _)) => randomA <= randomB}
      .map((_, pixel) => pixel)

    Block(newPixels)
  }

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

  def ssim: Block => Double = { reference =>
    val terms = generateStatisticsTerms(reference)
    luminance(terms) * contrast(terms) * structure(terms)
  }

  lazy val size: Int = pixels.size
}

case class BlockCoordinates(imageId: Int, blockId: Int)(implicit customRandom: Random = random) extends Gene {

  lazy val block: Block = PersistenceManager.blockAt(imageId, blockId)

  override def mutate: Gene = BlockCoordinates(imageId, blockId)

  override def toString: String = s"Block coordinates: $imageId $blockId"
}

case class Frame(imageId: Int, blocksCoordinates: List[BlockCoordinates])(implicit customRandom: Random = random) extends Chromosome(blocksCoordinates)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocksCoordinates: List[BlockCoordinates] => Frame(imageId, aBlocksCoordinates)


  // TODO: obtener el fitness de la persistencia
  protected override def calculateFitness: Double = blocksCoordinates.foldLeft(0d) { (total, blockCoordinates) =>
    // TODO: que sus bloques ya tengan todo calculado
    val referencesBlocks: List[Block] = PersistenceManager.blocksAt(blockCoordinates.blockId)
    total + (referencesBlocks.map(referenceBlock => blockCoordinates.block.ssim(referenceBlock)).sum / blocksCoordinates.size)
  }

  override def mutate: Chromosome = {
    // TODO: actualizar las coordenadas de cada pixel de cada bloque. Usar el número de bloque y el tamaño de bloque = 11 para calcular las nuevas coordenadas del pixel dentro del bloque reubicado
    // TODO: llevar a const el tamaño de bloque
    copyWith(
      ImagesManager.shufflePixelsOf(blocksCoordinates)
    )
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case Success(Frame(imageId, blocksCoordinates)) =>
      Image(Success(Frame(imageId, blocksCoordinates)))
}

// TODO: incluir el fitness del block (tupla con mean, sd y covariance respecto a las referencias)
type DataModel = Map[Int, Map[Int, Block]]

object PersistenceManager {
  private var mutablePixelsDictionary: collection.mutable.Map[Int, collection.mutable.Map[Int, Block]] = collection.mutable.Map()
  private lazy val references: DataModel = ImagesManager.initialDataModel(immutableImages.size, immutableImages, currentId)
  private var currentId: Int = 1

  private val immutableImages: List[ImmutableImage] = List(
    // TODO: pasar a constantes el tamaño de imagen (proporcional al tamaño de bloque de 11x11)
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/cyndaquil.png").scaleTo(550, 550),
    ImmutableImage.loader().fromFile("src/main/scala/resources/ssim/charmander.png").scaleTo(550, 550)
  )

  private def nextId(): Unit = currentId += 1

  private def imagesIds: List[Int] = mutablePixelsDictionary.keys.toList

  private def coordinatesOf(imageId: Int): List[BlockCoordinates] = (for {
    (blockId, _) <- blocksOf(imageId)
  } yield BlockCoordinates(imageId, blockId)).toList
  def blocksOf(imageId: Int): collection.mutable.Map[Int, Block] = mutablePixelsDictionary.getOrElse(imageId, collection.mutable.Map())
  def blockAt(imageId: Int, blockId: Int): Block = blocksOf(imageId).getOrElse(blockId, Block(Vector())())

  def blocksAt(blockId: Int): List[Block] = references
    .values
    .map(_.getOrElse(blockId, Block(Vector())()))
    .toList

  // TODO: persistir junto con el bloque, su fitness (calcularla en esta función)
  def append(imageId: Int, blockId: Int, block: Block): Unit = {
    if(!mutablePixelsDictionary.contains(imageId)) mutablePixelsDictionary += (imageId -> collection.mutable.Map())
    val blocksEntry = blocksOf(imageId)
    blocksEntry += (blockId -> block)
    // TODO: al persistir el bloque, calcular sus valores estadísticos
    mutablePixelsDictionary += (imageId -> blocksEntry)
  }

  def create(dataModel: DataModel): Unit = {
    val newDictionary = collection.mutable.Map[Int, collection.mutable.Map[Int, Block]]()
    currentId = 0
    dataModel.keys.foreach { case imageId =>
      if (!newDictionary.contains(imageId)) newDictionary += (imageId -> collection.mutable.Map())

      val blocks = dataModel(imageId)
      blocks.foreach { case (blockId, block) =>
        // TODO: usar addBlock
        if(!newDictionary(imageId).contains(blockId)) newDictionary(imageId) += (blockId -> Block(Vector())())
        newDictionary(imageId) += (blockId -> block)
      }

      nextId()
    }
    
    mutablePixelsDictionary = newDictionary
  }

  def toDataModel(population: ImagesPopulation): DataModel = {
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

  def shufflePixelsOf(blockCoordinates: List[BlockCoordinates]): List[BlockCoordinates] = ???
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

  override def mutate(mutationLikelihood: Double): Population = {
    super.mutate(mutationLikelihood) match
      case mutants: ImagesPopulation => copyWith(PersistenceManager.append(mutants.images))
  }
}