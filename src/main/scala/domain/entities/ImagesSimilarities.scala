package domain.entities

import akka.remote.DaemonMsgCreate
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.util.{Random, Success, Try}

case class Block(references: List[ImageComposition])(pixels: List[Pixel])(implicit customRandom: Random = random) extends Gene {

  private val K1: Double = 0.01
  private val K2: Double = 0.03f
  private val L: Double = 255
  private val C1: Double = Math.pow(K1 * L, 2)
  private val C2: Double = Math.pow(K2 * L, 2)
  private val C3: Double = C2 / 2

  lazy val mean: Double = pixels.foldLeft(0)((total, aPixel) => total + aPixel.average()) / pixels.size

  override def mutate: Gene = ??? // TODO: hacer un shuffle de los pixeles del block

  def ssim: Double = 1.2
}

case class Frame(blocks: List[Block])(implicit customRandom: Random = random) extends Chromosome(blocks)(customRandom) {
  override def copyWith(genes: List[Gene]): Chromosome = genes match
    case aBlocks: List[Block] =>
      // TODO: poner acá lóigca para eliminar bloques con pixeles solapados
      Frame(aBlocks)(customRandom)

  protected override def calculateFitness: Double = blocks.foldLeft(0d)((total, aBlock) => total + aBlock.ssim) / blocks.size
}

case class ImageComposition(mean: Double, standardDeviation: Double, size: Int)

case class Image(frame: Try[Frame])(implicit customRandom: Random = random) extends Individual(frame)(customRandom) {
  override protected def copyWith(chromosome: Try[Chromosome]): Individual = chromosome match
    case aFrame: Try[Frame] => Image(aFrame)(customRandom)
}