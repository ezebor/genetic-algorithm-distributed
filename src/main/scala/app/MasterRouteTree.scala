package app

import akka.actor.*
import akka.cluster.singleton.{ClusterSingletonProxy, ClusterSingletonProxySettings}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.routing.FromConfig
import akka.util.Timeout
import app.ExecutionScript.{QUANTITY_OF_NODES, QUANTITY_OF_WORKERS}
import com.typesafe.config.ConfigFactory
import domain.*
import domain.Operators.*
import domain.actors.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.util.Random

object MasterRouteTree extends Parallel with SprayJsonSupport with EvolutionRequestBodyJsonProtocol {

  implicit val timeout: Timeout = Timeout(3 seconds)

  def apply(
             generationsManager: ActorRef,
             master: ActorRef,
             router: ActorRef,
             solutionsPrinter: ActorRef,
             quantityOfWorkersPerNode: Int
           ): Route = {
    pathPrefix("api" / "evolution") {
      (post & pathEndOrSingleSlash) {
        entity(as[EvolutionRequestBody]) { case EvolutionRequestBody(
        survivalLikelihood,
        crossoverLikelihood,
        mutationLikelihood,
        maxQuantityOfGenerationsWithoutImprovements,
        solutionsPopulationsSize
        ) =>
          solutionsPrinter ? PrinterOnline
          generationsManager ? ManagerOnline(solutionsPrinter, master, solutionsPopulationsSize, maxQuantityOfGenerationsWithoutImprovements)
          master ? MasterOnline(generationsManager, router, QUANTITY_OF_WORKERS, survivalLikelihood, crossoverLikelihood, mutationLikelihood)
          this.distributeWork(
            master,
            InitialPopulation(ExecutionScript.INDIVIDUAL_TYPE_NAME)
          )
          complete(StatusCodes.Created, "Evolution has started")
        }
      }
    }
  }
}
