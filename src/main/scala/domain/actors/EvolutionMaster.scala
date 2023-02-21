package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{Individual, Population}
import domain.{Execute, NewGenerationBuilt, Online}

import scala.util.Random

object EvolutionMaster {
  def props(quantityOfWorkers: Int, router: ActorRef): Props = Props(new EvolutionMaster(quantityOfWorkers, router))
}

class EvolutionMaster(quantityOfWorkers: Int, router: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = offline

  def offline: Receive = {
    case Online(manager: ActorRef) =>
      context.become(online(manager))
    case Execute(RETURN_SOLUTIONS, solutions: Population) =>
      log.info(s"${solutions.individuals.size} solutions were found. Fitness of each solution: ${solutions.individuals}")
    case HEALTH => sender() ! OK
  }

  def online(manager: ActorRef): Receive = {
    case Execute(EVOLUTION, population: Population) =>
      self ! Execute(NATURAL_SELECTION, population)
    case Execute(STOP, population: Population) =>
      manager ! NewGenerationBuilt(population)
    case Execute(currentOperatorName: String, population: Population) =>
      val (basePopulation: Population, nextOperatorName: String) = currentOperatorName match
        case NATURAL_SELECTION => (Population(List()), CROSSOVER)
        case CROSSOVER => (population, MUTATION)
        case MUTATION => (population, STOP)

        log.info(s"Executing $currentOperatorName for a population with size = ${population.individuals.size}. Next operator: $nextOperatorName")

      val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(manager)(nextOperatorName)(basePopulation, chunks.size))
      chunks.foreach(chunk => router ! Execute(currentOperatorName, chunk))
    case HEALTH => sender() ! OK
  }

  def waitingWorkers(manager: ActorRef)(nextOperatorName: String)(evolvedPopulation: Population, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, newPopulation: Population) =>
      val finalPopulation = Population(evolvedPopulation.individuals ::: newPopulation.individuals)
      if (pendingWorkers == 1) {
        context.become(online(manager))
        self ! Execute(nextOperatorName, finalPopulation)
      } else {
        context.become(waitingWorkers(manager)(nextOperatorName)(finalPopulation, pendingWorkers - 1))
      }
  }
}
