package domain.serializers

import akka.serialization.Serializer
import app.ExecutionScript.{DIMENSION_BLOCK_SIZE, DIMENSION_IMAGE_SIZE}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import domain.Execute
import domain.Operators.*
import domain.entities.*
import spray.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Success

class ExecuteImagesSimilaritiesJsonSerializer extends ExecuteJsonSerializer {
  protected override def chromosomeOf = {
    case Image(Success(frame)) => frame.getGenes
  }

  override def createPopulation(individuals: Vector[JsValue]): Population = {
    val futureGroupedImages = individuals.map { case JsArray(serializedBlocks) =>
      Future {
        val blocks = serializedBlocks.map { case serializedBlock: JsArray =>
          serializedBlock.elements match
            case Seq(JsNumber(frameLocationIdX), JsNumber(frameLocationIdY), JsNumber(imageId), JsNumber(pixelsSourceIdX), JsNumber(pixelsSourceIdY), JsNumber(fitness)) =>
              val pixelsSourceId = (pixelsSourceIdX.intValue, pixelsSourceIdY.intValue)
              Block(
                (frameLocationIdX.intValue, frameLocationIdY.intValue),
                imageId.intValue,
                pixelsSourceId,
                fitness.doubleValue
              )
        }.toList

        ImagesManager.blocksToImages(blocks)
      }
    }

    val futureImages = futureGroupedImages.foldLeft(Future(List[Image]())) { case (result, nextFutureImage) =>
      for {
        images <- result
        nextImage <- nextFutureImage
      } yield {
        nextImage ::: images
      }
    }

    ImagesPopulation(Await.result(futureImages, Duration.Inf))
  }

  override protected def serializeGenes(genes: Vector[Gene]): JsValue = JsArray(
    // TODO: agrupar bloques por image id, y devolver un object con key imageID
    genes.map { case aBlock: Block =>
      JsArray(
        JsNumber(aBlock.frameLocationId._1),
        JsNumber(aBlock.frameLocationId._2),
        JsNumber(aBlock.imageSourceId),
        JsNumber(aBlock.pixelsSourceId._1),
        JsNumber(aBlock.pixelsSourceId._2),
        JsNumber(aBlock.fitness),
      )
    }
  )
}
