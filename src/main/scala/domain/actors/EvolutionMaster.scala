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
  override def receive: Receive = {
    case command@Execute(EVOLUTION, population: Population) =>
      val chunkSize = population.individuals.size / quantityOfWorkers
      context.become(evolving(chunkSize))
      self ! command
  }

  def evolving(chunkSize: Int): Receive = {
    case Execute(EVOLUTION, population: Population) =>
      val chunks: List[Population] = population.intoChunks(chunkSize)
      context.become(waitingWorkers(Population(List()), CROSSOVER, chunks.size))
      distribute(chunks, NATURAL_SELECTION)
    case Execute(CROSSOVER, population: Population) =>
      val populationLookingForReproduction = population.randomSubPopulation(1 + population.individuals.size / 2)
      val chunks = populationLookingForReproduction.intoChunks(populationLookingForReproduction.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(Population(List()), MUTATION, chunks.size))
      distribute(chunks, CROSSOVER)
  }

  def waitingWorkers(evolvedPopulation: Population, nextOperatorName: String, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, newPopulation: Population) =>
      log.info(s"Adding ${newPopulation.individuals.size} new members: ${newPopulation.individuals}")
      val finalPopulation = Population(evolvedPopulation.individuals ++ newPopulation.individuals)
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${finalPopulation.individuals.size} members: $finalPopulation")
        context.become(evolving(finalPopulation.individuals.size / quantityOfWorkers))
        self ! Execute(nextOperatorName, finalPopulation)
      } else {
        context.become(waitingWorkers(finalPopulation, nextOperatorName, pendingWorkers - 1))
      }
    case Execute(UPDATE_POPULATION, population: Population) => ???
  }

  def distribute(chunks: List[Population], operatorName: String) = chunks.foreach(chunk => router ! Execute(operatorName, chunk))
}
