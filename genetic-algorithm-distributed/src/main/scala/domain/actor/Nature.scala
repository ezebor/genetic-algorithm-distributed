package domain.actor

import akka.actor.*
import domain.Operators.*
import domain.SolutionDescription.*

object Nature {
  def props(survivalLikelihood: Double,
            crossoverLikelihood: Double,
            mutationLikelihood: Double) = Props(new Nature(survivalLikelihood, crossoverLikelihood, mutationLikelihood))
}

class Nature(
              survivalLikelihood: Double,
              crossoverLikelihood: Double,
              mutationLikelihood: Double
            ) extends Actor with ActorLogging {

  override def receive: Receive = {
    case NaturalSelection(candidates) => ???
    case Crossover(leftParent, candidates) => ???
    case Mutation(candidates) => ???
    case FinalPopulationSelection(quantity, candidates) => ???
  }
}
