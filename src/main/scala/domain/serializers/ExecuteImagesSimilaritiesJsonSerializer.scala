package domain.serializers

import akka.serialization.Serializer
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

  override def createPopulation(genes: Vector[JsValue]): Population = {
    val images = genes.flatMap { case unSerializedImages: JsObject =>
      val unSerializedImagesMap = unSerializedImages.fields
      unSerializedImagesMap.map { case (imageId, JsArray(argbValues)) =>
        val pixels = argbValues
          .indices
          .map { index =>
            val argb = argbValues(index) match
              case JsNumber(value) => value.intValue
            Pixel(index / 550, index % 550, argb)
          }

        val blocks = pixels
          .grouped(11)
          .zipWithIndex

        val blockCoordinates: List[BlockCoordinates] = blocks.map { case (aPixels, blockId) =>
          BlockCoordinates(imageId.toInt, blockId)
        }.toList

        Image(Success(Frame(imageId.toInt, blockCoordinates)))
      }
    }
    ImagesPopulation(images.toList)
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = {
    val imageId = genes.head match
      case BlockCoordinates(anImageId, _) => anImageId
    val serializedArgb = genes.flatMap { case aBlockCoordinates: BlockCoordinates =>
      aBlockCoordinates.block.pixels.map(pixel => JsNumber(pixel.argb))
    }

    JsObject(
      s"$imageId" -> JsArray(serializedArgb)
    )
  }
}
