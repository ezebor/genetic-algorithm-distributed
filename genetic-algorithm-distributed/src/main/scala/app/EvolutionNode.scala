package app

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.sharding.ShardRegion.Passivate
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings, ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.cluster.{Cluster, Member}
import app.PopulationNode.system
import com.typesafe.config.ConfigFactory
import domain.SolutionDescription.{CROSSOVER_LIKELIHOOD, MUTATION_LIKELIHOOD, SURVIVAL_LIKELIHOOD}
import domain.actor.{EvolutionActor, PopulationActor}

object EvolutionNodeSettings {
  val numberOfShards = 20 // use 10x number of nodes in your cluster
  val numberOfEntities = 200 // 10x number of shards

  val extractEntityId: ShardRegion.ExtractEntityId = {
    case operator @ _ =>
      val entityId = operator.hashCode().abs % numberOfEntities
      (entityId.toString, operator)
  }

  val extractShardId: ShardRegion.ExtractShardId = {
    case operator @ _ =>
      val shardId = operator.hashCode.abs % numberOfShards
      shardId.toString
    case ShardRegion.StartEntity(entityId) =>
      (entityId.toLong % numberOfShards).toString
  }
}

class EvolutionNode(port: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = $port
      |""".stripMargin)
    .withFallback(ConfigFactory.load("resources/application.conf"))

  val system = ActorSystem("GeneticAlgorithmSystem", config)

  val evolutionShardRegionRef: ActorRef = ClusterSharding(system).start(
    typeName = "evolutionShardingRegion",
    entityProps = EvolutionActor.props(),
    settings = ClusterShardingSettings(system).withRememberEntities(true),
    extractEntityId = EvolutionNodeSettings.extractEntityId,
    extractShardId = EvolutionNodeSettings.extractShardId
  )

  if(port == 2552) {
    val populationActorProxy = system.actorOf(PopulationActor.props(evolutionShardRegionRef))

    Thread.sleep(10000)
    (1 to 11).foreach(i => populationActorProxy ! s"$i")
  }
}

object EvolutionNode1 extends EvolutionNode(2551)
object EvolutionNode2 extends EvolutionNode(2552)
