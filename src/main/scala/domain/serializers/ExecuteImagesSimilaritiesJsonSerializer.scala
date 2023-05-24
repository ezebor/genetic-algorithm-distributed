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
      val blockSize = DIMENSION_BLOCK_SIZE * DIMENSION_BLOCK_SIZE
      val immutableImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
      println(s"CANTIDAD DE ARGB: ${argbValues.length}")
      argbValues
        .indices
        .foreach { index =>
          val argb = argbValues(index) match
            case JsNumber(value) => value.intValue
          immutableImage.setPixel(Pixel(index % DIMENSION_IMAGE_SIZE, index / DIMENSION_IMAGE_SIZE, argb))
        }

      val blocks: List[Block] = ImagesManager.toBlocks(immutableImage)
      println(s"pixels deserialized (size = ${blocks.map(_.size)}")

      Image(Success(Frame(
        blocks//ImagesManager.toBlocks(immutableImage)
      )))
    }
    ImagesPopulation(images.toList)
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = {
    val a = genes
      .flatMap { case aBlock: Block =>
        aBlock.pixels
      }.length
    println(s"CANTIDAD A SERIALIZAR: ${ImagesManager.blockIds}")
    JsArray(
      genes
        .flatMap { case aBlock: Block =>
        aBlock.pixels.map(pixel => JsNumber(pixel.argb))
      }
    )
  }
}
