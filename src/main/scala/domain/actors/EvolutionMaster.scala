package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Execute
import domain.Operators.*
import domain.entities.{Individual, Population}

import scala.util.Random

object EvolutionMaster {
  def props(quantityOfWorkers: Int, router: ActorRef): Props = Props(new EvolutionMaster(quantityOfWorkers, router))
}

class EvolutionMaster(quantityOfWorkers: Int, router: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = offline

  def offline: Receive = {
    case Execute(EVOLUTION, population: Population) =>
      context.become(online)
      self ! Execute(NATURAL_SELECTION, population: Population)
    case Execute(RETURN_SOLUTIONS, solutions: Population) =>
      log.info(s"${solutions.individuals.size} solutions were found. Fitness of each solution: ${solutions.individuals}")
    case HEALTH => sender() ! OK
  }

  def online: Receive = {
    case Execute(currentOperatorName: String, population: Population) =>
      val (basePopulation: Population, nextOperatorName: String) = currentOperatorName match
        case NATURAL_SELECTION => (Population(List()), CROSSOVER)
        case CROSSOVER => (population, MUTATION)
        case MUTATION => (population, STOP)
        case STOP => (Population(List()), NATURAL_SELECTION)

        log.info(s"Executing $currentOperatorName for a population with size = ${population.individuals.size}. Next operator: $nextOperatorName")

      val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(nextOperatorName)(basePopulation, chunks.size))
      chunks.foreach(chunk => router ! Execute(currentOperatorName, chunk))
    case HEALTH => sender() ! OK
  }

  def waitingWorkers(nextOperatorName: String)(evolvedPopulation: Population, pendingWorkers: Int): Receive = {
    case Execute(currentOperatorName: String, newPopulation: Population) =>
      val (connectionStatus: Receive, individuals: List[Individual], newContext: ((Population, Int) => Receive)) = currentOperatorName match
        case ADD_POPULATION => (
          online,
          evolvedPopulation.individuals ::: newPopulation.individuals, 
          waitingWorkers(nextOperatorName)
        )
        case TAKE_BESTS_INDIVIDUALS => (
          offline,
          evolvedPopulation.bestIndividuals ::: newPopulation.bestIndividuals, 
          takingSolutions(RETURN_SOLUTIONS)
        )

      val finalPopulation = Population(individuals)
      if (pendingWorkers == 1) {
        context.become(connectionStatus)
        self ! Execute(nextOperatorName, finalPopulation)
      } else {
        context.become(newContext(finalPopulation, pendingWorkers - 1))
      }
  }

  def takingSolutions(nextOperatorName: String)(solutions: Population, pendingWorkers: Int): Receive = {
    case Execute(currentOperatorName: String, newPopulation: Population) => currentOperatorName match
      case ADD_POPULATION | TAKE_BESTS_INDIVIDUALS =>
        val newSolutions = Population(solutions.individuals ::: newPopulation.bestIndividuals)
        if (pendingWorkers == 1) {
          context.become(offline)
          self ! Execute(nextOperatorName, newSolutions)
        } else {
          context.become(takingSolutions(nextOperatorName)(newSolutions, pendingWorkers - 1))
        }

  }
}
