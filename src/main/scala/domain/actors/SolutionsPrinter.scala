package domain.actors

import akka.actor.*
import domain.entities.Population

object SolutionsPrinter {
  def props(): Props = Props(new SolutionsPrinter())
}

class SolutionsPrinter extends Actor with ActorLogging {
  override def receive: Receive = {
    case solutions: Population =>
      log.info(s"Found solutions: $solutions")
  }
}
