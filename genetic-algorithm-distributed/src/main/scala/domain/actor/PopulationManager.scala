package domain.actor

import akka.actor.*
import domain.Operators.*
import domain.SolutionDescription.*
import akka.actor.{Actor, ActorIdentity, ActorLogging, ActorRef, ActorSystem, Address, Identify, Props}
import akka.cluster.{Cluster, Member}
import akka.cluster.ClusterEvent.{InitialStateAsEvents, MemberEvent, MemberRemoved, MemberUp, UnreachableMember}

object PopulationManager {
  def props(populationSize: Int): Props = Props(new PopulationManager(populationSize))
}

class PopulationManager(populationSize: Int) extends Actor with ActorLogging {
  val cluster = Cluster(context.system)

  override def preStart(): Unit = {
    cluster.subscribe(
      self,
      initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent]
    )
  }

  override def postStop(): Unit = {
    cluster.unsubscribe(self)
  }

  override def receive: Receive = {
    case message: String =>
      log.info(message)
  }
}
