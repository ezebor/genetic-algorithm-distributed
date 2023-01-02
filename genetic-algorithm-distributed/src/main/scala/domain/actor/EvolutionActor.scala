package domain.actor

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Operators.*
import domain.SolutionDescription.*

object EvolutionActor {
  def props(): Props = Props(new EvolutionActor())
}

class EvolutionActor() extends Actor with ActorLogging {

  override def receive: Receive = {
    case message: String => log.info(s"llegó mensaje al Evolution actor: $message")
    case NaturalSelection(candidates) => ???
    case Crossover(leftParent, candidates) => ???
    case Mutation(candidates) => ???
    case FinalPopulationSelection(quantity, candidates) => ???
  }
}
