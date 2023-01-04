package domain

import SolutionDescription.*
import individuals.GenericIndividual

object Operators {
  val EVOLUTION = "evolution"
  val NATURAL_SELECTION = "natural-selection"
  val CROSSOVER = "crossover"
  val MUTATION = "mutation"
  val ADD_POPULATION = "add-population"
  val UPDATE_POPULATION = "update-population"

  case class Execute[T](operatorName: String, population: List[Individual[T]])
}
