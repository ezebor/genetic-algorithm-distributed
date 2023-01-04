package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Operators.*
import domain.SolutionDescription.*
import domain.individuals.GenericIndividual

object EvolutionMaster {
  def props(router: ActorRef): Props = Props(new EvolutionMaster(router))
}

class EvolutionMaster(router: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Execute(EVOLUTION, population) =>
      println(population)
  }
}
