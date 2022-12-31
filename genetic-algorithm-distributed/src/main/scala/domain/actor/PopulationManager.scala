package domain.actor

import akka.actor.*
import domain.Operators.*
import domain.SolutionDescription.*

object PopulationManager {
  def props(population: Population) = Props(new PopulationManager(population))
}

class PopulationManager(population: Population) extends Actor with ActorLogging {
  
  override def receive: Receive = ???
}
