package domain.individuals

trait Chromosome
trait Gene

trait Population(individuals: List[Individual]) {
  lazy val accumulatedFitness: List[(Individual, Double)] = individuals
    .zipWithIndex
    .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
      if(index == 0) result :+ (individual, individual.fitness)
      else result :+ (individual, individual.fitness + result(index - 1)._2)
    }
}

trait Individual(chromosome: Chromosome) {
  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness
}
type LeveledIndividual = (Individual, Double)
type LeveledPopulation = List[LeveledIndividual]

trait IndividualGenerator {
  def generateRandomPopulation(populationSize: Int): Population
}

trait FitnessTree
case class Node(leveledIndividual: LeveledIndividual, leftTree: FitnessTree, rightTree: FitnessTree) extends FitnessTree
case class Leaf(leveledIndividual: LeveledIndividual) extends FitnessTree
