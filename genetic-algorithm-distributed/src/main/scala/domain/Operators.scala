package domain

import domain.individuals.{Individual, Population}
import spray.json.*

object Operators {
  val EVOLUTION = "evolution"
  val NATURAL_SELECTION = "natural-selection"
  val CROSSOVER = "crossover"
  val MUTATION = "mutation"
  val ADD_POPULATION = "add-population"
  val UPDATE_POPULATION = "update-population"

  case class Execute(operatorName: String, population: Population)
}
