package domain

import akka.actor.*
import domain.entities.{Individual, Population}

object Operators {
  val EVOLUTION = "evolution"
  val NATURAL_SELECTION = "natural-selection"
  val CROSSOVER = "crossover"
  val MUTATION = "mutation"
  val UPDATE_POPULATION = "update-population"
  val ADD_POPULATION = "add-population"
  val LAST_INDIVIDUALS = "last-individuals"
  val STOP = "stop"
  val HEALTH = "health"
  val OK = "ok"
  val OFFLINE = "offline"
}

case class ManagerOnline(solutionsPrinter: ActorRef, evolutionMaster: ActorRef, solutionsPopulationSize: Int, maxQuantityOfGenerationsWithoutImprovements: Int)
case class MasterOnline(manager: ActorRef, router: ActorRef, quantityOfWorkers: Int, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double)
case class WorkerOnline(evolutionMaster: ActorRef, survivalPopulationSize: SurvivalPopulationSize, crossoverLikelihood: CrossoverLikelihood, mutationLikelihood: MutationLikelihood)
case class SurvivalPopulationSize(size: Int)
case class CrossoverLikelihood(likelihood: Double)
case class MutationLikelihood(likelihood: Double)
case object PrinterOnline
case class BuildNewGeneration(population: Population)
case class Execute(operatorName: String, population: Population)
case class GenerationBuilt(population: Population)
