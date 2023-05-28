package domain.actors
import akka.actor.*
import domain.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.random
import domain.entities.{EmptyPopulation, Individual, Population}

object GenerationsManager {
  def props(): Props = Props(new GenerationsManager())
}

class GenerationsManager() extends BaseActor {
  override def receive: Receive = offline

  private def offline: Receive = {
    case ManagerOnline(originalSender, evolutionMaster, solutionsPopulationSize, maxQuantityOfGenerationsWithoutImprovements) =>
      def buildNewGeneration(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Operator = { population =>
        log.info(s"Starting generation [$generationId]")

        this.distributeWork(
          evolutionMaster,
          population
        )

        context.become(this.waitingPopulations(
          manageBuiltGeneration(generationId, quantityOfGenerationsWithoutImprovements, solutions),
          population.empty(),
          1
        ))
      }

      def manageBuiltGeneration(generationId: Int, quantityOfGenerationsWithoutImprovements: Int, solutions: Population): Operator = { population =>
        val emptyPopulation = population.empty()
        if (solutions.individuals.isEmpty)
          buildNewGeneration(generationId + 1, 0, population.copyWith(List(population.bestIndividual)))(emptyPopulation)
        else if (population.bestIndividual.fitness.getOrElse(0d) > solutions.individuals.head.fitness.getOrElse(0d))
          log.info(s"New better individual was found with fitness = ${population.bestIndividual.fitness}")
          val newSolutions = population.copyWith(population.bestIndividual :: {
            if (solutions.individuals.size == solutionsPopulationSize) solutions.individuals.dropRight(1)
            else solutions.individuals
          })
          buildNewGeneration(generationId + 1, 0, newSolutions)(emptyPopulation)
        else if (quantityOfGenerationsWithoutImprovements <= maxQuantityOfGenerationsWithoutImprovements)
          buildNewGeneration(generationId + 1, quantityOfGenerationsWithoutImprovements + 1, solutions)(emptyPopulation)
        else printSolutions(solutions)
      }

      def printSolutions: Operator = { solutions =>
        this.distributeWork(
          originalSender,
          solutions,
          1,
          1
        )
      }

      context.become(this.waitingPopulations(
        buildNewGeneration(1, 0, EmptyPopulation),
        EmptyPopulation,
        1
      ))
  }
}
