package domain.actors

import akka.actor.*
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.PngWriter
import domain.PrinterOnline
import domain.entities.{BlockCoordinates, Frame, Image, Population}

import scala.util.Success

object SolutionsPrinter {
  def props(): Props = Props(new SolutionsPrinter())
}

class SolutionsPrinter extends Actor with ActorLogging {

  override def receive: Receive = offline
  
  def offline: Receive = {
    case PrinterOnline => {
      def online: Receive = {
        case Population(images: List[Image]) =>
          images
            .map { image =>
              val newImage = ImmutableImage.create(500, 500)
              image.frame match
                case Success(Frame(imageId, blockCoordinates)) =>
                  blockCoordinates.flatMap(coordinates => coordinates.block.pixels).foreach(pixel => newImage.setPixel(pixel))
                  newImage.output(PngWriter.NoCompression, s"src/main/scala/resources/ssim/result_$imageId.png")
            }
        case solutions: Population =>
          log.info(s"Found solutions: $solutions")
      }

      context.become(online)
    }
  }
}
