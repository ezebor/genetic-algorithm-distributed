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
  val STOP = "stop"
  val HEALTH = "health"
  val OK = "ok"
  val OFFLINE = "offline"
}

case class ManagerOnline(evolutionMaster: ActorRef)
case class MasterOnline()
case class BuildNewGeneration(population: Population)
case class Execute(operatorName: String, population: Population)
case class GenerationBuilt(population: Population)
