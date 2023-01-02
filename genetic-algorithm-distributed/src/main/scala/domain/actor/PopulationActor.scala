package domain.actor

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Operators.*
import domain.SolutionDescription.*

class PopulationActorPriorityMailbox(settings: ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    PriorityGenerator {
      case _: MemberEvent => 0
      case _ => 4
    }
  )

object PopulationActor {
  def props(shardRegion: ActorRef): Props = Props(new PopulationActor(shardRegion))
}

class PopulationActor(shardRegion: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case message: String => 
      log.info(s"llegó mensaje al population actor: $message")
      shardRegion ! message
  }
}
