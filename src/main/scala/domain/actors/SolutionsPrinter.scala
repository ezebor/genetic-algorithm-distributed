package domain.actors

import akka.actor.*
import app.ExecutionScript.DIMENSION_IMAGE_SIZE
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.PngWriter
import domain.PrinterOnline
import domain.entities.*

import scala.util.Success

object SolutionsPrinter {
  def props(): Props = Props(new SolutionsPrinter())
}

class SolutionsPrinter extends BaseActor {

  override def receive: Receive = offline
  
  def offline: Receive = {
    case PrinterOnline => {
      def print: Operator = {
        case ImagesPopulation(images) =>
          images
            .zipWithIndex
            .map { (image, index) =>
              val newImage = ImmutableImage.create(DIMENSION_IMAGE_SIZE, DIMENSION_IMAGE_SIZE)
              image.frame match
                case Success(Frame(blocks)) =>
                  for {
                    case aBlock @ Block(_, imageId, pixelsSourceId) <- blocks
                    aPixel <- ImagesManager.pixelsAt(imageId, pixelsSourceId)
                  } yield {
                    newImage.setPixel(aBlock.pixelWithFixedLocation(aPixel))
                  }
                  newImage.output(PngWriter.NoCompression, s"src/main/scala/resources/ssim/result_$index.png")
            }
        case BasketsPopulation(baskets) =>
          log.info(s"Found solutions: $baskets")
      }

      context.become(this.waitingPopulations(
        print,
        EmptyPopulation,
        1
      ))
    }
  }
}
