package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{Individual, Population}
import domain.{Execute, GenerationBuilt, MasterOnline}

import scala.util.Random

object EvolutionMaster {
  def props(quantityOfWorkers: Int, router: ActorRef, originalSender: ActorRef): Props = Props(new EvolutionMaster(quantityOfWorkers, router, originalSender))
}

class EvolutionMaster(quantityOfWorkers: Int, router: ActorRef, originalSender: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = offline

  private def offline: Receive = {
    case MasterOnline() =>
      context.become(online)
    case HEALTH => sender() ! OK
  }

  private def online: Receive = { message =>
    def waitingWorkers(nextOperatorName: String)(evolvedPopulation: Population, pendingWorkers: Int): Receive = {
      case Execute(ADD_POPULATION, newPopulation: Population) =>
        val finalPopulation = Population(evolvedPopulation.individuals ::: newPopulation.individuals)
        if (pendingWorkers == 1) {
          context.become(online)
          self ! Execute(nextOperatorName, finalPopulation)
        } else {
          context.become(waitingWorkers(nextOperatorName)(finalPopulation, pendingWorkers - 1))
        }
    }

    message match
      case Execute(EVOLUTION, population: Population) =>
        self ! Execute(NATURAL_SELECTION, population)
      case Execute(STOP, population: Population) =>
        originalSender ! GenerationBuilt(population)
      case Execute(currentOperatorName: String, population: Population) =>
        val (basePopulation: Population, nextOperatorName: String) = currentOperatorName match
          case NATURAL_SELECTION => (Population(List()), CROSSOVER)
          case CROSSOVER => (population, MUTATION)
          case MUTATION => (population, STOP)

        log.debug(s"Executing $currentOperatorName for a population with size = ${population.individuals.size}. Next operator: $nextOperatorName")

        val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
        context.become(waitingWorkers(nextOperatorName)(basePopulation, chunks.size))
        chunks.foreach(chunk => router ! Execute(currentOperatorName, chunk))
      case HEALTH => sender() ! OK
      case OFFLINE => context.become(offline)
  }
}
