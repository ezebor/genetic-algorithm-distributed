package domain

import domain.SolutionDescription.*

object Operators {
  trait Operator
  case class NaturalSelection(candidates: Population) extends Operator
  case class EndedSelection(population: Population)
  case class FinalPopulationSelection(quantity: Int, candidates: Population) extends Operator
  case class Crossover(leftParent: Individual, candidates: Population) extends Operator
  case class EndedCrossover(leftParent: Individual, rightParent: Individual)
  case class Mutation(candidates: Population) extends Operator
  case class EndedMutation(mutatedIndividual: Individual)
}
