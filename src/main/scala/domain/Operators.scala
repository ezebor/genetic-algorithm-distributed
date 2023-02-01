package domain

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
  val TAKE_BEST_INDIVIDUAL = "take_best_individual"
  val GO_TO_NEXT_GENERATION = "go_to_next_generation"
}

case class Execute(operatorName: String, population: Population)
