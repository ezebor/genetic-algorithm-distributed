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
    case Block(pixels) => JsArray(
      pixels.map(pixel => JsObject(
        "x" -> JsNumber(pixel.x),
        "y" -> JsNumber(pixel.y),
        "argb" -> JsNumber(pixel.argb),
      )).toVector
    )
  }

  private def buildBlock = (gene: JsArray) => Block(gene.elements.map { case pixel: JsObject =>
    pixel.getFields("x", "y", "argb") match {
      case Seq(JsNumber(x), JsNumber(y), JsNumber(argb)) => Pixel(x.intValue, y.intValue, argb.intValue)
    }
  }.toList)

  protected override def deserializeIndividual = (genes: List[JsValue]) => {
    Image(Success(Frame(
      for case gene: JsArray <- genes
        yield buildBlock(gene)
    )))
  }
}
