package domain.serializers

import akka.serialization.Serializer
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import spray.json.*
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE}
import com.sksamuel.scrimage.ImmutableImage

import scala.util.Success

class ExecuteImagesSimilaritiesJsonSerializer extends ExecuteJsonSerializer {
  protected override def chromosomeOf = {
    case Image(Success(frame)) => frame.getGenes
  }

  override def createPopulation(genes: Vector[JsValue]): Population = {
    val images = genes.map { case JsArray(argbValues) =>
      val immutableImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
      argbValues
        .indices
        .foreach { index =>
          val argb = argbValues(index) match
            case JsNumber(value) => value.intValue
          immutableImage.setPixel(Pixel(index / DIMENSION_IMAGE_SIZE, index % DIMENSION_IMAGE_SIZE, argb))
        }

      Image(Success(Frame(
        ImagesManager.toBlocks(immutableImage)
      )))
    }
    ImagesPopulation(images.toList)
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = {
    val immutableImage = ImagesManager.toImmutableImage {
      genes match
        case blocks: Vector[Block] => blocks.toList
    }

    JsArray(
      immutableImage
        .pixels()
        .toVector
        .map(aPixel => JsNumber(aPixel.argb))
    )
  }
}
