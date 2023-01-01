package app

import akka.actor.*
import com.typesafe.config.ConfigFactory
import domain.actor.Nature
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, ReceiveTimeout}
import akka.cluster.sharding.ShardRegion.Passivate
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import domain.SolutionDescription.SURVIVAL_LIKELIHOOD
import domain.SolutionDescription.CROSSOVER_LIKELIHOOD
import domain.SolutionDescription.MUTATION_LIKELIHOOD

object NatureEvolutionSettings {
  val numberOfShards = 10 // use 10x number of nodes in your cluster
  val numberOfEntities = 100 // 10x number of shards

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

class Evolution(port: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
      |akka.actor.provider = cluster
      |akka.remote.artery.canonical.hostname = localhost
      |akka.remote.artery.canonical.port = $port
      |""".stripMargin)
    .withFallback(ConfigFactory.load("application.conf"))

  val system = ActorSystem("EvolutionSystem", config)

  val natureShardRegionRef: ActorRef = ClusterSharding(system).start(
    typeName = "NatureEvolution",
    entityProps = Nature.props(SURVIVAL_LIKELIHOOD, CROSSOVER_LIKELIHOOD, MUTATION_LIKELIHOOD),
    settings = ClusterShardingSettings(system).withRememberEntities(true),
    extractEntityId = NatureEvolutionSettings.extractEntityId,
    extractShardId = NatureEvolutionSettings.extractShardId
  )

  system.actorSelection("akka://PopulationSystem@localhost:2551/user/populationManager") ! "HOLA DESDE EL MÄS ALLA"
  system.actorSelection("akka://PopulationSystem@localhost:2551/user/populationManager") ! natureShardRegionRef.toString
}

object Universe1 extends Evolution(2561)
object Universe2 extends Evolution(2562)
