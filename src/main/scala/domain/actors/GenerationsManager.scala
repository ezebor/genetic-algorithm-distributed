package domain.actors
import akka.actor.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.POPULATION_SIZE
import domain.entities.{BasketsPopulationRandomGenerator, Population}
import domain.{BuildNewGeneration, Execute, GenerationBuilt, Online}

object GenerationsManager {
  def props: Props = Props(new GenerationsManager())
}

class GenerationsManager extends Actor with ActorLogging {
  override def receive: Receive = offline

  private def offline: Receive = {
    case Online(evolutionMaster: ActorRef) =>
      evolutionMaster ! ONLINE
      context.become(online(1, evolutionMaster))
  }

  private def online(generationsId: Int, evolutionMaster: ActorRef): Receive = {
    case BuildNewGeneration(population: Population) =>
      log.info(s"Starting generation [$generationsId]")
      evolutionMaster ! Execute(EVOLUTION, population: Population)
    case GenerationBuilt(population: Population) =>
      log.info(s"$population")
  }
}
