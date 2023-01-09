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
    case command @ Execute(EVOLUTION, population: Population) =>
      val chunkSize = population.size / quantityOfWorkers
      context.become(evolving(chunkSize))
      self ! command
  }

  def evolving(chunkSize: Int): Receive = {
    case command @ Execute(EVOLUTION, population: Population) =>
      population
        .grouped(chunkSize)
        .foreach(populationChunk => router ! Execute(NATURAL_SELECTION, populationChunk))
      context.become(waitingWorkers(List(), quantityOfWorkers))
    case command @ Execute(CROSSOVER, population: Population) => ???
    case command @ Execute(MUTATION, population: Population) => ???
    case command @ Execute(STOP, population: Population) => ???
  }

  def waitingWorkers(pendingPopulation: Population, pendingWorkers: Int): Receive = {
    case Execute(ADD_POPULATION, population: Population) => ???
    case Execute(UPDATE_POPULATION, population: Population) => ???
  }
}
