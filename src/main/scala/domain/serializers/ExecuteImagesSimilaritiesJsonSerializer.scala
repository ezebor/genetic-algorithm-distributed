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

  protected override def serializeGene = (gene: Gene) => gene match {
    case BlockCoordinates(imageId, blockId) => JsObject(
      "imageId" -> JsNumber(imageId),
      "blockId" -> JsNumber(blockId)
    )
  }

  private def buildBlockCoordinates = (blockCoordinates: JsObject) => blockCoordinates.getFields("imageId", "blockId") match {
    case Seq(JsNumber(imageId), JsNumber(blockId)) => BlockCoordinates(imageId.intValue, blockId.intValue)
  }

  protected override def deserializeIndividual = (coordinates: List[JsValue]) => {
    Image(Success(Frame(
      for case blockCoordinates: JsObject <- coordinates
        yield buildBlockCoordinates(blockCoordinates)
    )))
  }
}
