package app

import akka.actor.*
import akka.cluster.ClusterEvent.MemberEvent
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.routing.FromConfig
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import domain.Operators.*
import domain.actors.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import domain.{BuildNewGeneration, Execute, ManagerOnline, MasterOnline}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

class EvolutionMasterNode(quantityOfWorkersPerNode: Int) extends App with SprayJsonSupport with EvolutionRequestBodyJsonProtocol {
  val configSource = ConfigFactory.load("resources/application.conf")
  val serializationConfig = configSource.getConfig(ExecutionScript.SERIALIZATION_CONFIG)
  val mainConfig = configSource.getConfig("mainConfig")
  val masterRouterConfig = configSource.getConfig("masterRouterConfig")

  val config = ConfigFactory.parseString(
    s"""
      |akka.remote.artery.canonical.port = 2551
      |akka.cluster.roles = ["$MASTER_ROLE"]
      |akka.actor.deployment./evolutionRouter.cluster.max-nr-of-instances-per-node = $quantityOfWorkersPerNode
      |""".stripMargin)
    .withFallback(serializationConfig)
    .withFallback(masterRouterConfig)
    .withFallback(mainConfig)

  implicit val system: ActorSystem = ActorSystem("GeneticAlgorithmSystem", config)
  implicit val timeout: Timeout = Timeout(3 seconds)
  implicit val ec: ExecutionContext = system.dispatcher

  val generationsManager = system.actorOf(GenerationsManager.props(), "generationManager")
  val master = system.actorOf(EvolutionMaster.props(), "evolutionMaster")
  val solutionsPrinter = system.actorOf(SolutionsPrinter.props(), "solutionsPrinter")
  val router: ActorRef = system.actorOf(FromConfig.props(EvolutionWorker.props()), "evolutionRouter")

  val routesTree: Route = MasterRouteTree(generationsManager, master, router, solutionsPrinter, quantityOfWorkersPerNode)

  Http().newServerAt("localhost", 8080).bind(routesTree)
}
