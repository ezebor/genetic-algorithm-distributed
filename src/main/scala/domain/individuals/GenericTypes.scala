package domain.individuals

trait Chromosome
trait Gene

case class Population(individuals: List[Individual]) {
  lazy val accumulatedFitness: List[(Individual, Double)] = individuals
    .zipWithIndex
    .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
      result :+ (individual, {
        if(index == 0) individual.fitness
        else individual.fitness + result(index - 1)._2
      })
    }
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}
