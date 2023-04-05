package domain.actors

import akka.actor.*
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

  val population: Population = RandomPopulation(POPULATION_SIZE, "Basket")
  val router: TestProbe = TestProbe("router")
  val master: ActorRef = system.actorOf(EvolutionMaster.props(), "evolutionMaster")

  "Evolution master" should {
    "evolve a given population" in {
      master ! Execute(EVOLUTION, population)

      router.receiveWhile() {
        case Execute(NATURAL_SELECTION, _) => router.reply(Execute(ADD_POPULATION, population))
        case Execute(CROSSOVER, _) => router.reply(Execute(ADD_POPULATION, population))
        case Execute(MUTATION, _) => router.reply(Execute(ADD_POPULATION, population))
        case Execute(UPDATE_POPULATION, _) => router.reply(Execute(ADD_POPULATION, population))
      }

      master ! HEALTH
      expectMsg[String](OK)
    }
  }
}
