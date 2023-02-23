package domain.actors
import akka.actor.*
import domain.Operators.*
import domain.entities.AlgorithmConfig.{POPULATION_SIZE, random}
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
      context.become(online(1, Population(List()), evolutionMaster))
  }

  private def online(generationsId: Int, solutions: Population, evolutionMaster: ActorRef): Receive = {
    case BuildNewGeneration(population: Population) =>
      log.info(s"Starting generation [$generationsId]")
      evolutionMaster ! Execute(EVOLUTION, population: Population)
    case GenerationBuilt(population: Population) =>
      // TODO 1: validar convergencia (que N seguidos den una diff menor a 1E-10
      // TODO 2: si en la nueva generación nadie es mejor que las soluciones existentes, no agregarla a la lista
      if(solutions.individuals.isEmpty)
        val newSolutions = Population(List(population.bestIndividual))
        context.become(online(generationsId + 1, newSolutions, evolutionMaster))
        self ! BuildNewGeneration(population)
      else {
        val bestIndividual: Individual = population.bestIndividual
        val previousSolution = solutions.individuals.last
        val newSolutions = Population(solutions.individuals ::: List(bestIndividual))
        if(Math.abs(bestIndividual.fitness - previousSolution.fitness) <= 1E-10)
          log.info(s"$newSolutions")
        else
          context.become(online(generationsId + 1, newSolutions, evolutionMaster))
          self ! BuildNewGeneration(population)
      }
  }
}
