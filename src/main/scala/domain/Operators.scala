package domain

import domain.entities.{Individual, Population}

object Operators {
  val EVOLUTION = "evolution"
  val NATURAL_SELECTION = "natural-selection"
  val CROSSOVER = "crossover"
  val MUTATION = "mutation"
  val ADD_POPULATION = "add-population"
  val UPDATE_POPULATION = "update-population"
  val STOP = "stop"
  val HEALTH = "health"
  val OK = "ok"
}

case class Execute(operatorName: String, population: Population)
