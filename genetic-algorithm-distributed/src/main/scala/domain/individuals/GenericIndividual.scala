package domain.individuals

import domain.SolutionDescription.{Chromosome, Individual, Population}

trait GenericIndividual[T] {
  def apply(chromosome: Chromosome[T]): Individual[T] = Individual(chromosome, fitness(chromosome))
  protected def fitness(chromosome: Chromosome[T]): Double
  def generateRandomPopulation(populationSize: Int): Population[T]
}
