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
  val RETURN_SOLUTIONS = "return_solutions"
  val ONLINE = "online"
}

case class Online(manager: ActorRef)
case class NextGeneration(population: Population)
case class Execute(operatorName: String, population: Population)
case class NewGenerationBuilt(population: Population)
