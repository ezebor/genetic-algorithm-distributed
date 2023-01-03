package domain

import domain.SolutionDescription.*
import domain.individuals.GenericIndividual

object Operators {
  val NATURAL_SELECTION = "natural-selection"
  val CROSSOVER = "crossover"
  val MUTATION = "mutation"
  val ADD_POPULATION = "add-population"
  val UPDATE_POPULATION = "update-population"

  case class Evolve[T](populationSize: Population[T])
  case class Execute[T](operator: String, population: Population[T])
}
