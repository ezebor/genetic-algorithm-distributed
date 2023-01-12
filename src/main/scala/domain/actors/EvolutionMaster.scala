package domain.actors

import akka.actor.*
import akka.cluster.ClusterEvent.*
import akka.cluster.{Cluster, Member}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config
import domain.Execute
import domain.Operators.*
import domain.individuals.{Individual, Population}

object EvolutionMaster {
  def props(quantityOfWorkers: Int, router: ActorRef): Props = Props(new EvolutionMaster(quantityOfWorkers, router))
}

class EvolutionMaster(quantityOfWorkers: Int, router: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case command@Execute(EVOLUTION, population: Population) =>
      val chunkSize = population.size / quantityOfWorkers
      context.become(evolving(chunkSize))
      self ! command
  }

  def evolving(chunkSize: Int): Receive = {
    case Execute(operatorName, population: Population) =>
      val nextOperatorName = operatorName match
        case EVOLUTION => NATURAL_SELECTION
        case STOP => ???
        case operatorName => operatorName

      context.become(waitingWorkers(List(), quantityOfWorkers))
      population
        .grouped(chunkSize)
        .foreach(populationChunk => router ! Execute(nextOperatorName, populationChunk))
  }

  def waitingWorkers(evolvedPopulation: Population, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, newPopulation: Population) =>
      log.info(s"Adding ${newPopulation.size} new members: $newPopulation")
      val finalPopulation = evolvedPopulation ++ newPopulation
      if (pendingWorkers == 1) {
        log.info(s"Population has evolved with ${finalPopulation.size} members: $finalPopulation")
        context.become(evolving(finalPopulation.size / quantityOfWorkers))
        self ! Execute(CROSSOVER, finalPopulation)
      } else {
        context.become(waitingWorkers(finalPopulation, pendingWorkers - 1))
      }
    case Execute(UPDATE_POPULATION, population: Population) => ???
  }
}
