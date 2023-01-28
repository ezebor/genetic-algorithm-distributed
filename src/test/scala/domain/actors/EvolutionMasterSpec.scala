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
  extends TestKit(ActorSystem("ActorsSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll
    with should.Matchers {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  val QUANTITY_OF_WORKERS = 3
  val POPULATION_SIZE = 200
  val QUANTITY_OF_CHUNKS = 4 // (200 / 3) + 1 (because of the 20 leftover individuals)

  val router = TestProbe("router")
  val master = system.actorOf(EvolutionMaster.props(QUANTITY_OF_WORKERS, router.ref), "evolutionMaster")
  val population = BasketsPopulationRandomGenerator.randomPopulation(POPULATION_SIZE)

  "Evolution master" should {
    "start natural selection when receives 'evolution' operator" in {
      master ! Execute(EVOLUTION, population)

      val chunks = population.individuals.grouped(population.individuals.size / QUANTITY_OF_WORKERS).toList
      chunks.size should be(QUANTITY_OF_CHUNKS)
      chunks.foreach { chunk =>
        router.expectMsg(Execute(NATURAL_SELECTION, Population(chunk)))
      }
    }
  }
}
