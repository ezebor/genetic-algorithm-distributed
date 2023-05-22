package domain.actors

import akka.actor.*
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.PngWriter
import domain.PrinterOnline
import domain.entities.{BasketsPopulation, BlockCoordinates, EmptyPopulation, Frame, Image, ImagesPopulation, Population}

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
            .map { image =>
              val newImage = ImmutableImage.create(500, 500)
              image.frame match
                case Success(Frame(imageId, blockCoordinates)) =>
                  blockCoordinates.flatMap(coordinates => coordinates.block.pixels).foreach(pixel => newImage.setPixel(pixel))
                  newImage.output(PngWriter.NoCompression, s"src/main/scala/resources/ssim/result_$imageId.png")
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
