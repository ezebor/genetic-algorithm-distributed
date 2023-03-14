package domain.actors

import akka.actor.ActorSystem
import akka.routing.{FromConfig, RoundRobinGroup}
import akka.testkit.{EventFilter, ImplicitSender, TestActorRef, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

class EvolutionWorkerSpec
  extends TestKit(ActorSystem("EvolutionWorkerSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll
    with should.Matchers {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  val QUANTITY_OF_WORKERS_PER_NODE = 3
  val QUANTITY_OF_NODES = 2
  val POPULATION_SIZE = 200
  val SURVIVAL_POPULATION_SIZE: Int = POPULATION_SIZE / (QUANTITY_OF_NODES * QUANTITY_OF_WORKERS_PER_NODE)
  val CROSSOVER_LIKELIHOOD = 0.5
  val MUTATION_LIKELIHOOD = 0.03

  val worker = system.actorOf(EvolutionWorker.props(
    SURVIVAL_POPULATION_SIZE,
    CROSSOVER_LIKELIHOOD,
    MUTATION_LIKELIHOOD
  ), "evolutionWorker")

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(POPULATION_SIZE)

  "Evolution worker" should {
    "execute natural selection when receives 'natural_selection' operator" in {
      worker ! Execute(NATURAL_SELECTION, population)

      val executeMessage = expectMsgType[Execute]

      executeMessage.operatorName should be(ADD_POPULATION)
      executeMessage.population.individuals.size should be(SURVIVAL_POPULATION_SIZE)
    }

    "execute crossover when receives 'crossover' operator" in {
      worker ! Execute(CROSSOVER, population)

      val executeMessage = expectMsgType[Execute]

      executeMessage.operatorName should be(ADD_POPULATION)
      executeMessage.population.individuals.size should be(POPULATION_SIZE)
    }

    "execute mutation when receives 'mutation' operator" in {
      worker ! Execute(MUTATION, population)

      val executeMessage = expectMsgType[Execute]

      executeMessage.operatorName should be(ADD_POPULATION)
      assert(executeMessage.population.individuals.size <= (POPULATION_SIZE * (MUTATION_LIKELIHOOD + 0.03)).toInt)
    }
  }
}
