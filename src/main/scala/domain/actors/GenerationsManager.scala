package domain.actors
import akka.actor.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.POPULATION_SIZE
import domain.entities.{BasketsPopulationRandomGenerator, Population}
import domain.{Execute, NewGenerationBuilt, NextGeneration, Online}

object GenerationsManager {
  def props(evolutionMaster: ActorRef): Props = Props(new GenerationsManager(evolutionMaster))
}

class GenerationsManager(evolutionMaster: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = offline

  def offline: Receive = {
    case ONLINE =>
      evolutionMaster ! Online(self)
      context.become(online(1))
  }

  def online(generationsId: Int): Receive = {
    case NextGeneration(population: Population) =>
      log.info(s"Starting generation [$generationsId]")
      evolutionMaster ! Execute(EVOLUTION, population: Population)
    case NewGenerationBuilt(population: Population) =>
      log.info(s"$population")
  }
}
