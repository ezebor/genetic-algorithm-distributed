package domain.actor

import akka.actor.*
import domain.Operators.*
import domain.SolutionDescription.*
import akka.actor.{Actor, ActorIdentity, ActorLogging, ActorRef, ActorSystem, Address, Identify, Props}
import akka.cluster.{Cluster, Member}
import akka.cluster.ClusterEvent.{InitialStateAsEvents, MemberEvent, MemberRemoved, MemberUp, UnreachableMember}

object Nature {
  def props(survivalLikelihood: Double,
            crossoverLikelihood: Double,
            mutationLikelihood: Double): Props = Props(new Nature(survivalLikelihood, crossoverLikelihood, mutationLikelihood))
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
