package domain.entities

import akka.remote.DaemonMsgCreate
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
  private lazy val standardDeviation: Double = Math.sqrt(pixels.map(pixel => Math.pow(pixel.average() - mean, 2)).sum / (size - 1))
  private lazy val covariance: Block => Double = { reference =>
    val referenceValues = reference.values.toArray
    val selfValues = values.toArray
    val valuesSum = referenceValues.indices.map(index => (referenceValues(index) - reference.mean) * (selfValues(index) - mean)).sum
    valuesSum / (size - 1)
  }

  override def mutate: Gene = ??? // TODO: hacer un shuffle de los pixeles del block

  def ssim: Block => Double = { referenceBlock =>
    1
  }

  def index: (Int, Int) = {
    val head = pixels.head
    (head.x, head.y)
  }

  def size: Int = pixels.size

  def values: List[Int] = pixels.map(_.average())
}

case class Frame(references: Map[(Int, Int), List[Block]])(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] =>
      // TODO: poner acá lóigca para eliminar bloques con pixeles solapados
      Frame(references)(aBlocks)(customRandom)

  protected override def calculateFitness: Double = blocks.foldLeft(0d) {(total, aBlock) =>
    val referencesBlocks = references.getOrElse(aBlock.index, List())
    total + (referencesBlocks.map(aBlock.ssim).sum / aBlock.size)
  }
}

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case aFrame: Try[Frame] => Image(aFrame)(customRandom)
}