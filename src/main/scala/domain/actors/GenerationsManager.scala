package domain.actors
import akka.actor.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.{MAX_QUANTITY_OF_GENERATIONS_WITHOUT_IMPROVEMENTS, POPULATION_SIZE, SOLUTIONS_POPULATION_SIZE, random}
import domain.entities.{Individual, Population}
import domain.{BuildNewGeneration, Execute, GenerationBuilt, Online}

object GenerationsManager {
  def props: Props = Props(new GenerationsManager())
}

class GenerationsManager extends Actor with ActorLogging {
  override def receive: Receive = offline

  private def offline: Receive = {
    case Online(evolutionMaster: ActorRef) =>
      evolutionMaster ! ONLINE
      context.become(firstOnline(evolutionMaster))
  }

  private def firstOnline(evolutionMaster: ActorRef): Receive = {
    case BuildNewGeneration(population: Population) =>
      log.info(s"Starting generation [1]")
      evolutionMaster ! Execute(EVOLUTION, population: Population)
    case GenerationBuilt(population: Population) =>
      context.become(steadyOnline(2, 0, Population(List(population.bestIndividual)), evolutionMaster))
      self ! BuildNewGeneration(population)
  }

  private def steadyOnline(
                            generationId: Int,
                            quantityOfGenerationsWithoutImprovements: Int,
                            solutions: Population,
                            evolutionMaster: ActorRef): Receive = {
    case BuildNewGeneration(population: Population) =>
      log.info(s"Starting generation [$generationId] over a population with ${population.individuals.size} individuals")
      evolutionMaster ! Execute(EVOLUTION, population)
    case GenerationBuilt(population: Population) =>
      if(population.bestIndividual.fitness > solutions.individuals.head.fitness)
        log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
        val newSolutions = Population(population.bestIndividual :: {
          if(solutions.individuals.size == SOLUTIONS_POPULATION_SIZE) solutions.individuals.dropRight(1)
          else solutions.individuals
        })
        context.become(steadyOnline(generationId + 1, 0, newSolutions, evolutionMaster))
        self ! BuildNewGeneration(population)
      else
        if(quantityOfGenerationsWithoutImprovements <= MAX_QUANTITY_OF_GENERATIONS_WITHOUT_IMPROVEMENTS)
          context.become(steadyOnline(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions, evolutionMaster))
          self ! BuildNewGeneration(population)
        else
          evolutionMaster ! OFFLINE
          log.info(s"Found solutions: $solutions")
          context.become(offline)
  }
}
