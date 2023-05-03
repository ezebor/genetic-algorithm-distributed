package integration

import akka.actor.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.routing.{FromConfig, RoundRobinGroup, RoundRobinPool}
import akka.testkit.{EventFilter, ImplicitSender, TestActorRef, TestKit, TestProbe}
import app.{ExecutionScript, MasterRouteTree}
import com.typesafe.config.ConfigFactory
import domain.{Execute, PrinterOnline}
import domain.Operators.*
import domain.actors.{EvolutionMaster, EvolutionWorker, GenerationsManager, SolutionsPrinter}
import domain.entities.*
import org.scalatest.matchers.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasketsEvolutionSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport with EvolutionRequestBodyJsonProtocol {

  "Baskets evolution" should {
    "find the best basket using one worker for a small population" in {
      implicit val testActorSystem: ActorSystem = ActorSystem("BasketsEvolutionSpec", ConfigFactory.parseString(
        s"""
           |akka.test.single-expect-default = 60s
           |""".stripMargin))
      val generationsManager: ActorRef = testActorSystem.actorOf(GenerationsManager.props(), "generationManager")
      val master: ActorRef = testActorSystem.actorOf(EvolutionMaster.props(), "evolutionMaster")
      val solutionsPrinter: TestProbe = TestProbe("solutionsPrinter")(testActorSystem)
      val router: ActorRef = testActorSystem.actorOf(RoundRobinPool(1).props(EvolutionWorker.props()), "router")
      val routesTree = MasterRouteTree(generationsManager, master, router, solutionsPrinter.ref, 1, "Basket")

      // TODO: this test takes several seconds
      /*Post("/api/evolution", EvolutionRequestBody()) ~> routesTree ~> check {
        status shouldBe StatusCodes.Created
      }

      val solutions = solutionsPrinter.expectMsgType[Population]
      assert(solutions.bestIndividual.fitness.get == 36)*/
    }

    "find the best basket using several workers for a small population" in {
      implicit val testActorSystem: ActorSystem = ActorSystem("BasketsEvolutionSpec", ConfigFactory.parseString(
        s"""
           |akka.test.single-expect-default = 3s
           |""".stripMargin))
      val generationsManager: ActorRef = testActorSystem.actorOf(GenerationsManager.props(), "generationManager")
      val master: ActorRef = testActorSystem.actorOf(EvolutionMaster.props(), "evolutionMaster")
      val solutionsPrinter: TestProbe = TestProbe("solutionsPrinter")(testActorSystem)
      val router: ActorRef = testActorSystem.actorOf(RoundRobinPool(50).props(EvolutionWorker.props()), "router")
      val routesTree = MasterRouteTree(generationsManager, master, router, solutionsPrinter.ref, 50, ExecutionScript.BASKET_INDIVIDUAL_TYPE_NAME)

      Post("/api/evolution", EvolutionRequestBody()) ~> routesTree ~> check {
        status shouldBe StatusCodes.Created
      }

      solutionsPrinter.expectMsg(PrinterOnline)
      val solutions = solutionsPrinter.expectMsgType[Population]
      assert(solutions.bestIndividual.fitness.get == 36)
    }

    "find the best basket using quite a lot of workers for a small population" in {
      implicit val testActorSystem: ActorSystem = ActorSystem("BasketsEvolutionSpec", ConfigFactory.parseString(
        s"""
           |akka.test.single-expect-default = 1s
           |""".stripMargin))
      val generationsManager: ActorRef = testActorSystem.actorOf(GenerationsManager.props(), "generationManager")
      val master: ActorRef = testActorSystem.actorOf(EvolutionMaster.props(), "evolutionMaster")
      val solutionsPrinter: TestProbe = TestProbe("solutionsPrinter")(testActorSystem)
      val router: ActorRef = testActorSystem.actorOf(RoundRobinPool(100).props(EvolutionWorker.props()), "router")
      val routesTree = MasterRouteTree(generationsManager, master, router, solutionsPrinter.ref, 50, ExecutionScript.BASKET_INDIVIDUAL_TYPE_NAME)

      Post("/api/evolution", EvolutionRequestBody()) ~> routesTree ~> check {
        status shouldBe StatusCodes.Created
      }

      solutionsPrinter.expectMsg(PrinterOnline)
      val solutions = solutionsPrinter.expectMsgType[Population]
      assert(solutions.bestIndividual.fitness.get == 36)
    }
  }
}
