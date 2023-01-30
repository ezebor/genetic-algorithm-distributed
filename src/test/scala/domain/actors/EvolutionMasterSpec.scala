package domain.actors

import akka.actor.ActorSystem
import akka.routing.{FromConfig, RoundRobinGroup}
import akka.testkit.{EventFilter, ImplicitSender, TestActorRef, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import domain.Execute
import domain.Operators.*
import domain.entities.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

class EvolutionMasterSpec
  extends TestKit(ActorSystem("EvolutionMasterSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll
    with should.Matchers {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  val QUANTITY_OF_WORKERS = 3
  val POPULATION_SIZE = 200
  val QUANTITY_OF_CHUNKS = 4 // (200 / 3) + 1 (because of the 20 leftover individuals)

  val population = BasketsPopulationRandomGenerator.randomPopulation(POPULATION_SIZE)
  val router = TestProbe("router")
  router.receiveWhile() {
    case Execute(_, _) => router.reply(Execute(ADD_POPULATION, population))
  }
  val master = system.actorOf(EvolutionMaster.props(QUANTITY_OF_WORKERS, router.ref), "evolutionMaster")

  "Evolution master" should {
    "start natural selection when receives 'evolution' operator" in {
      master ! Execute(EVOLUTION, population)

      router.receiveN(QUANTITY_OF_CHUNKS) foreach {
        case Execute(operatorName, _) => assert(operatorName == NATURAL_SELECTION)
      }
    }

    "start crossover when receives 'crossover' operator" in {
      master ! Execute(CROSSOVER, population)

      router.receiveN(QUANTITY_OF_CHUNKS) foreach {
        case Execute(operatorName, _) => assert(operatorName == CROSSOVER)
      }
    }
  }
}
