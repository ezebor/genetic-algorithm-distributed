package domain.serializers

import akka.serialization.Serializer
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import spray.json.*

import scala.util.Success

class ExecuteImagesSimilaritiesJsonSerializer extends ExecuteJsonSerializer {
  protected override def chromosomeOf = {
    case Image(Success(frame)) => frame.getGenes
  }

  override def createPopulation(individuals: Vector[JsValue]): Population = {
    val images = individuals.map { case JsArray(serializedBlocks) =>
      val blocks = serializedBlocks.map { case serializedBlock: JsArray =>
        serializedBlock.elements match
          case Seq(JsNumber(frameLocationIdX), JsNumber(frameLocationIdY), JsNumber(imageId), JsNumber(pixelsSourceIdX), JsNumber(pixelsSourceIdY)) =>
            val pixelsSourceId = (pixelsSourceIdX.intValue, pixelsSourceIdY.intValue)
            Block(
              (frameLocationIdX.intValue, frameLocationIdY.intValue),
              imageId.intValue,
              pixelsSourceId,
              ImagesManager.ssim(ImagesManager.pixelsAt(imageId.intValue, pixelsSourceId), pixelsSourceId)
            )
      }.toList

      Image(Success(Frame(
        blocks
      )))
    }

    ImagesPopulation(images.toList)
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = JsArray(
    genes.map { case aBlock: Block =>
      JsArray(
        JsNumber(aBlock.frameLocationId._1),
        JsNumber(aBlock.frameLocationId._2),
        JsNumber(aBlock.imageId),
        JsNumber(aBlock.pixelsSourceId._1),
        JsNumber(aBlock.pixelsSourceId._2),
      )
    }
  )
}
