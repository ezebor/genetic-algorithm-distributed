package domain

object SolutionDescription {
  type Population[T] = List[Individual[T]]
  type Chromosome[T] = List[T]
  case class Individual[T](chromosome: Chromosome[T], fitness: Double)
}
