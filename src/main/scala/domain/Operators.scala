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

case class ManagerOnline(originalSender: ActorRef, evolutionMaster: ActorRef, solutionsPopulationSize: Int, maxQuantityOfGenerationsWithoutImprovements: Int)
case class MasterOnline(manager: ActorRef, survivalLikelihood: Double, crossoverLikelihood: Double, mutationLikelihood: Double)
case class WorkerOnline(evolutionMaster: ActorRef, survivalPopulationSize: Int, crossoverLikelihood: Double, mutationLikelihood: Double)
case object PrinterOnline
case class BuildNewGeneration(population: Population)
case class Execute(operatorName: String, population: Population)
case class GenerationBuilt(population: Population)
