package domain.entities

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import domain.entities.AlgorithmConfig.*
import domain.exceptions.{EmptyAccumulatedFitnessListException, IllegalChunkSizeException}
import spray.json.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}

trait Chromosome(genes: List[Gene])(implicit random: Random) {
  def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
  def copyWith(genes: List[Gene]): Chromosome
  def getGenes: List[Gene] = genes

  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness

  def crossoverWith(couple: Chromosome, crossoverLikelihood: Double): (List[Gene], List[Gene]) = {
    def addGeneAccordingToLikelihood(nextGene: Gene, genes: List[Gene]): List[Gene] =
      if(random.nextInt(100) + 1 <= crossoverLikelihood * 100) nextGene :: genes
      else genes

    (genes ::: couple.getGenes).foldLeft((List[Gene](), List[Gene]())) { (result, nextGene) =>
      (
        addGeneAccordingToLikelihood(nextGene, result._1),
        addGeneAccordingToLikelihood(nextGene, result._2)
      )
    }
  }
}
trait Gene(implicit random: Random) {
  def mutate: Gene
}

object AlgorithmConfig {
  implicit val random: Random = new Random()
}

case class Population(individuals: List[Individual])(implicit random: Random) {
  lazy val accumulatedFitness: List[(Individual, Double)] = {
    val totalFitness = individuals.foldLeft(0d)((total, individual) => total + individual.fitness.getOrElse(0d))
    val fitIndividuals = individuals.filter(_.fitness.getOrElse(0d) > 0)
    fitIndividuals
      .zipWithIndex
      .foldLeft(List[(Individual, Double)]()) { case (result, (individual, index)) =>
        result :+ (
          individual,
          if(index == 0) individual.fitness.getOrElse(0d) / totalFitness
          else if (index == fitIndividuals.size - 1) 1.0
          else individual.fitness.getOrElse(0d) / totalFitness + result(index - 1)._2
        )
      }
  }

  def findIndividualWhoseAccumulatedFitnessWindowIncludes(aFitness: Double): Individual = {
    @tailrec
    def recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness: List[(Individual, Double)]): Individual = {
      if(anAccumulatedFitness.size == 1) anAccumulatedFitness.head._1
      else {
        val middleIndex = anAccumulatedFitness.size / 2
        val middleFitness = anAccumulatedFitness(middleIndex)._2
        aFitness match
          case _ if aFitness == middleFitness => anAccumulatedFitness(middleIndex)._1
          case _ if aFitness > middleFitness => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.slice(middleIndex + 1, anAccumulatedFitness.size))
          case _ if aFitness > anAccumulatedFitness(middleIndex - 1)._2 => anAccumulatedFitness(middleIndex)._1
          case _ => recFindIndividualWhoseAccumulatedFitnessWindowIncludes(anAccumulatedFitness.slice(0, middleIndex))
      }
    }

    if(accumulatedFitness.isEmpty) Individual.emptyIndividual(EmptyAccumulatedFitnessListException(this))
    else recFindIndividualWhoseAccumulatedFitnessWindowIncludes(accumulatedFitness)
  }

  def intoChunks(chunkSize: Int): List[Population] = 
    if(chunkSize == 0) List(copyWith(List(Individual.emptyIndividual(IllegalChunkSizeException(this)))))
    else individuals
      .grouped(chunkSize)
      .map(anIndividuals => Population(anIndividuals))
      .toList

  def randomSubPopulation(size: Int): Population = {
    @tailrec
    def recRandomSubPopulation(sourcePopulation: Population, sinkPopulation: Population, aSize: Int): Population = {
      if(aSize == 0 || sourcePopulation.accumulatedFitness.isEmpty) sinkPopulation
      else {
        val foundIndividual = sourcePopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())

        recRandomSubPopulation(
          Population(sourcePopulation.individuals.filter(_ != foundIndividual)),
          Population(sinkPopulation.individuals ::: List(foundIndividual)),
          aSize - 1
        )
      }
    }

    recRandomSubPopulation(
      this,
      Population(List()),
      size
    )
  }

  def selectStrongerPopulation(size: Int): Population = randomSubPopulation(size)

  def crossoverWith(otherPopulation: Population, crossoverLikelihood: Double): Population = {
    Population(individuals.flatMap { individual =>
      val couple = otherPopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
      individual.crossoverWith(couple, crossoverLikelihood: Double)
    })
  }

  def mutate(mutationLikelihood: Double): Population = {
    val individualsToMutate = individuals.filter(_ => random.nextInt(100) + 1 <= mutationLikelihood * 100)
    Population(
      individualsToMutate.map(individual => individual.mutate)
    )
  }

  override def toString: String = {
    s"""
      |***** Population summary *****
      |* Size of the population: ${individuals.size} individuals
      |* Best individual (fitness = ${bestIndividual.fitness}): $bestIndividual
      |* Individuals: $individuals
      |******************************
      |""".stripMargin
  }

  lazy val bestIndividual: Individual = individuals.reduceLeft((firstIndividual: Individual, secondIndividual: Individual) => {
    if(firstIndividual.fitness.getOrElse(0d) >= secondIndividual.fitness.getOrElse(0d)) firstIndividual
    else secondIndividual
  })

  def copyWith(newIndividuals: List[Individual]): Population = Population(newIndividuals)(random)
}

object Individual {
  def emptyIndividual(exception: RuntimeException): Individual = new Individual(Failure(exception)) {
    override protected def copyWith(chromosome: Try[Chromosome]): Individual = emptyIndividual(exception)
  }
}

trait Individual(tryChromosome: Try[Chromosome])(implicit random: Random) {
  protected def copyWith(chromosome: Try[Chromosome]): Individual

  def getTryChromosome: Try[Chromosome] = tryChromosome

  def getTryGenes: Try[List[Gene]] = tryChromosome.map(_.getGenes)

  def fitness: Try[Double] = tryChromosome.map(_.fitness)

  def crossoverWith(couple: Individual, crossoverLikelihood: Double): List[Individual] = {
    val tryChildrenChromosomes: Try[(Chromosome, Chromosome)] = for {
      thisChromosome <- getTryChromosome
      coupleChromosome <- couple.getTryChromosome
      crossedGenes = thisChromosome.crossoverWith(coupleChromosome, crossoverLikelihood)
    } yield {
      (
        thisChromosome.copyWith(crossedGenes._1),
        thisChromosome.copyWith(crossedGenes._2)
      )
    }

    List(
      copyWith(tryChildrenChromosomes.map ((child: Chromosome, _) => child)),
      copyWith(tryChildrenChromosomes.map ((_, child: Chromosome) => child)),
    )
  }

  def mutate: Individual = copyWith(tryChromosome.map(_.mutate))
}

case class EvolutionRequestBody(
                                 populationSize: Int = 500,
                                 crossoverLikelihood: Double = 0.5,
                                 mutationLikelihood: Double = 0.03,
                                 maxQuantityOfGenerationsWithoutImprovements: Int = 50,
                                 solutionsPopulationsSize: Int = 10
                               )

trait EvolutionRequestBodyJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val evolutionRequestJsonProtocol: RootJsonFormat[EvolutionRequestBody] = jsonFormat5(EvolutionRequestBody.apply)
}

object RandomPopulation {
  def apply(populationSize: Int, individualTypeName: String)(implicit customRandom: Random = random): Population = individualTypeName match
    case "Basket" => Population(
      (1 to populationSize).map(i => Basket(
        Success(ItemsList(
          (1 to (customRandom.nextInt(5) + 1)).map(_ => Item(s"Item $i", customRandom.nextInt(10), customRandom.nextInt(10))).toList
        )(customRandom))
      )).toList)
}
