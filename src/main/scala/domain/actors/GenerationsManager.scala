package domain.actors
import akka.actor.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{Individual, Population}
import domain.{BuildNewGeneration, Execute, GenerationBuilt, Online}

object GenerationsManager {
  def props(populationSize: Int,
            solutionsPopulationSize: Int,
            maxQuantityOfGenerationsWithoutImprovements: Int): Props = Props(new GenerationsManager(
    populationSize,
    solutionsPopulationSize,
    maxQuantityOfGenerationsWithoutImprovements
  ))
}

class GenerationsManager(
                          populationSize: Int,
                          solutionsPopulationSize: Int,
                          maxQuantityOfGenerationsWithoutImprovements: Int) extends Actor with ActorLogging {
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
      if(population.bestIndividual.fitness.getOrElse(0d) > solutions.individuals.head.fitness.getOrElse(0d))
        log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
        val newSolutions = Population(population.bestIndividual :: {
          if(solutions.individuals.size == solutionsPopulationSize) solutions.individuals.dropRight(1)
          else solutions.individuals
        })
        context.become(steadyOnline(generationId + 1, 0, newSolutions, evolutionMaster))
        self ! BuildNewGeneration(population)
      else
        if(quantityOfGenerationsWithoutImprovements <= maxQuantityOfGenerationsWithoutImprovements)
          context.become(steadyOnline(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions, evolutionMaster))
          self ! BuildNewGeneration(population)
        else
          evolutionMaster ! OFFLINE
          log.info(s"Found solutions: $solutions")
          context.become(offline)
  }
}
