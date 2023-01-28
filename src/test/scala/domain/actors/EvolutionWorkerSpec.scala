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

class EvolutionWorkerSpec
  extends TestKit(ActorSystem("EvolutionWorkerSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll
    with should.Matchers {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  val POPULATION_SIZE = 200
  val likelihoodBias = 0.05

  val worker = system.actorOf(EvolutionWorker.props(), "evolutionWorker")
  val population = BasketsPopulationRandomGenerator.randomPopulation(POPULATION_SIZE)

  "Evolution worker" should {
    "execute natural selection when receives 'natural_selection' operator" in {
      worker ! Execute(NATURAL_SELECTION, population)

      val executeMessage = expectMsgType[Execute]
      val bottomNaturalSelectionLikelihood = EvolutionWorker.SURVIVAL_LIKELIHOOD - likelihoodBias
      val topNaturalSelectionLikelihood = EvolutionWorker.SURVIVAL_LIKELIHOOD + likelihoodBias

      executeMessage.operatorName should be(ADD_POPULATION)
      Math.max(executeMessage.population.individuals.size, POPULATION_SIZE * bottomNaturalSelectionLikelihood) should be(executeMessage.population.individuals.size)
      Math.min(executeMessage.population.individuals.size, POPULATION_SIZE * topNaturalSelectionLikelihood) should be(executeMessage.population.individuals.size)
    }
  }
}
