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
    val images = genes.map { case JsArray(argbValues) =>
      val pixels = argbValues
        .indices
        .map { index =>
          val argb = argbValues(index) match
            case JsNumber(value) => value.intValue
          Pixel(index / 110, index % 110, argb)
        }

      val blocks: List[Block] = pixels
        .grouped(11)
        .map { aPixels =>
          Block(aPixels.toVector)
        }.toList

      Image(Success(Frame(blocks)))
    }
    ImagesPopulation(images.toList)
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = {
    JsArray(
      genes
        .flatMap { case aBlock: Block =>
        aBlock.pixels.map(pixel => JsNumber(pixel.argb))
      }
    )
  }
}
