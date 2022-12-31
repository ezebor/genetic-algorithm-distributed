package domain

object SolutionDescription {
  type Population = List[Individual]
  type Genotype = List[Gen]
  type Gen = Any

  case class Individual(chromosome: Genotype, fitness: Double)
}
