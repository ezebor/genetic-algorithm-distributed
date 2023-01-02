package domain.actor

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Operators.*
import domain.SolutionDescription.*

object EvolutionMaster {
  def props(router: ActorRef): Props = Props(new PopulationActor(router))
}

class PopulationActor(router: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case message: String =>
      router ! message
  }
}
