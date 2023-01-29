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

  val worker = system.actorOf(EvolutionWorker.props(), "evolutionWorker")
  val population = BasketsPopulationRandomGenerator.randomPopulation(POPULATION_SIZE)

  "Evolution worker" should {
    "execute natural selection when receives 'natural_selection' operator" in {
      worker ! Execute(NATURAL_SELECTION, population)

      val executeMessage = expectMsgType[Execute]

      executeMessage.operatorName should be(ADD_POPULATION)
      executeMessage.population.individuals.size should be((POPULATION_SIZE * EvolutionWorker.SURVIVAL_LIKELIHOOD).toInt)
    }
  }
}
