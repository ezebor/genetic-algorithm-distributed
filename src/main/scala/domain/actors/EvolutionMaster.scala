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
      context.become(evolving)
      self ! Execute(NATURAL_SELECTION, population: Population)
    case Execute(RETURN_SOLUTIONS, solutions: Population) =>
      log.info(s"${solutions.individuals.size} solutions were found. Fitness of each solution: ${solutions.individuals.map(_.fitness)}")
    case HEALTH => sender() ! OK
  }

  def evolving: Receive = {
    case Execute(currentOperatorName: String, population: Population) =>
      val (basePopulation: Population, nextOperatorName: String) = currentOperatorName match
        case NATURAL_SELECTION => (Population(List()), CROSSOVER)
        case CROSSOVER => (population, MUTATION)
        case MUTATION => (population, UPDATE_POPULATION)
        case UPDATE_POPULATION => (Population(List()), STOP)
        case STOP => (Population(List()), NATURAL_SELECTION)

        log.info(s"Executing $currentOperatorName for a population with size = ${population.individuals.size}. Next operator: $nextOperatorName")

      val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(basePopulation, nextOperatorName, chunks.size))
      chunks.foreach(chunk => router ! Execute(currentOperatorName, chunk))
    case HEALTH => sender() ! OK
  }

  def waitingWorkers(evolvedPopulation: Population, nextOperatorName: String, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, newPopulation: Population) =>
      log.debug(s"Adding ${newPopulation.individuals.size} new members: ${newPopulation.individuals}")
      val finalPopulation = Population(evolvedPopulation.individuals ++ newPopulation.individuals)
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${finalPopulation.individuals.size} members. Next operation is $nextOperatorName")
        context.become(evolving)
        self ! Execute(nextOperatorName, finalPopulation)
      } else {
        context.become(waitingWorkers(finalPopulation, nextOperatorName, pendingWorkers - 1))
      }
    case Execute(GO_TO_NEXT_GENERATION, newPopulation: Population) =>
      val populationForNextGeneration = Population(evolvedPopulation.individuals ++ newPopulation.individuals)
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${populationForNextGeneration.individuals.size} members. Next operation is $nextOperatorName")
        context.become(evolving)
        self ! Execute(nextOperatorName, populationForNextGeneration)
      } else {
        context.become(waitingWorkers(populationForNextGeneration, nextOperatorName, pendingWorkers - 1))
      }
    case Execute(TAKE_BESTS_INDIVIDUALS, newPopulation: Population) =>
      val finalPopulation = Population(evolvedPopulation.bestIndividuals ++ newPopulation.bestIndividuals)
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${finalPopulation.individuals.size} members. Next operation is $nextOperatorName")
        context.become(offline)
        self ! Execute(RETURN_SOLUTIONS, finalPopulation)
      } else {
        context.become(waitingWorkers(finalPopulation, nextOperatorName, pendingWorkers - 1))
      }
  }
}
