package domain

object SolutionDescription {
  type Population = List[Individual]
  type Genotype = List[Gen]
  type Gen = Any
  type StopCriteria = Population => Boolean
  
  val SURVIVAL_LIKELIHOOD = 0.8
  val CROSSOVER_LIKELIHOOD = 0.5
  val MUTATION_LIKELIHOOD = 0.1

  case class Individual(chromosome: Genotype, fitness: Double)
}
