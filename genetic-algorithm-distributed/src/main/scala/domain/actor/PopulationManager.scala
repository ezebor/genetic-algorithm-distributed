package domain.actor

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import domain.Operators.*
import domain.SolutionDescription.*

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
    case shardRegion: ActorRef => shardRegion ! "Te recibí shard region!!"
  }
}
