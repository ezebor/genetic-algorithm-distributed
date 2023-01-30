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
      context.become(evolving)
      self ! command
  }

  def evolving: Receive = {
    case Execute(EVOLUTION, population: Population) =>
      val chunks: List[Population] = population.intoChunks(population.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(Population(List()), CROSSOVER, chunks.size))
      chunks.foreach(chunk => router ! Execute(NATURAL_SELECTION, chunk))
    case Execute(CROSSOVER, population: Population) =>
      val chunks = population.intoChunks(population.individuals.size / quantityOfWorkers)
      context.become(waitingWorkers(population, MUTATION, chunks.size))
      chunks.foreach(chunk => router ! Execute(CROSSOVER, chunk))
    case Execute(MUTATION, population: Population) =>
      println(s"LLEGO EL MUTATUON AL MASTER CON CANTIDAD DE INDIFIVUOS = ${population.individuals.size}")
    case HEALTH => sender() ! OK
  }

  def waitingWorkers(evolvedPopulation: Population, nextOperatorName: String, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, newPopulation: Population) =>
      log.info(s"Adding ${newPopulation.individuals.size} new members: ${newPopulation.individuals}")
      val finalPopulation = Population(evolvedPopulation.individuals ++ newPopulation.individuals)
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${finalPopulation.individuals.size} members: $finalPopulation")
        context.become(evolving)
        self ! Execute(nextOperatorName, finalPopulation)
      } else {
        context.become(waitingWorkers(finalPopulation, nextOperatorName, pendingWorkers - 1))
      }
    case Execute(UPDATE_POPULATION, population: Population) => ???
  }
}
