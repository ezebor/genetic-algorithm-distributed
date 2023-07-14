package domain.entities

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import app.ExecutionScript
import app.ExecutionScript.{POPULATION_SIZE, QUANTITY_OF_WORKERS_PER_NODE}
import com.sksamuel.scrimage.ImmutableImage
import domain.entities.AlgorithmConfig.*
import domain.exceptions.{EmptyAccumulatedFitnessListException, IllegalChunkSizeException}
import spray.json.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}

trait Chromosome(genes: List[Gene])(implicit random: Random) {
  def mutate: Chromosome = copyWith(genes.map(gene => gene.mutate))
  def copyWith(genes: List[Gene]): Chromosome
  def getGenes: List[Gene] = genes

  protected def calculateFitness: Double
  lazy val fitness: Double = calculateFitness

  def crossoverWith(couple: Chromosome, crossoverLikelihood: Double): (List[Gene], List[Gene]) = {
    def addGeneAccordingToLikelihood(nextGene: Gene, genes: List[Gene]): List[Gene] =
      if(nextGene.isHealthy || random.nextInt(100) + 1 <= crossoverLikelihood * 100) nextGene :: genes
      else genes

    (genes ::: couple.getGenes).foldLeft((List[Gene](), List[Gene]())) { (result, nextGene) =>
      (
        addGeneAccordingToLikelihood(nextGene, result._1),
        addGeneAccordingToLikelihood(nextGene, result._2)
      )
    }
  }
}
trait Gene(fitness: Double)(implicit random: Random) {
  def mutate: Gene
  def isHealthy: Boolean
}

object AlgorithmConfig {
  implicit val random: Random = new Random()
}

trait Population(internalIndividuals: List[Individual])(implicit random: Random) {
  def copyWith(newIndividuals: List[Individual]): Population
  def empty(): Population
  def individuals: List[Individual] = internalIndividuals
  
  def fusionWith(otherPopulation: Population): Population = this.copyWith(individuals ::: otherPopulation.individuals)

  // TODO: parametrizar el accumulatedFitness, teniendo en cuenta que los fitness vienen de los workers
  // TODO: lograr que no se necesiten todos los valores para construir la lista (el total), como hacer un nextDouble * 10. Al hacer el find individual, si ya lo tengo lo devuelvo, sino espero
  // TODO: no esperar a terminar de construir el accumulatedFitness si ya tengo el individuo que quiero (si ya me llegó del worker)
  lazy val accumulatedFitness: List[(Individual, Double)] = {
    val futureTotalFitness = individuals
      .map(anIndividual => Future(anIndividual.fitness.getOrElse(0d)))
      .foldLeft(Future(0d)) { case (futureResult, futureFitness) =>
        for {
          total <- futureResult
          fitness <- futureFitness
        } yield total + fitness
      }

    val totalFitness = Await.result(futureTotalFitness, Duration.Inf)
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

  def intoChunksOfSize(chunkSize: Int): Vector[Population] =
    if(chunkSize == 0) Vector(copyWith(List(Individual.emptyIndividual(IllegalChunkSizeException(this)))))
    else individuals
      .grouped(chunkSize)
      .map(anIndividuals => copyWith(anIndividuals))
      .toVector

  def intoNChunks(quantityOfChunks: Int): Vector[Population] = individuals
    .zipWithIndex
    .foldLeft(Map[Int, List[Individual]]()) { case (result, (nextIndividual, index)) =>
      val groupIndex = index % quantityOfChunks
      result.updated(groupIndex, nextIndividual :: result.getOrElse(groupIndex, List()))
    }
    .values
    .map(anIndividuals => copyWith(anIndividuals))
    .toVector

  def randomSubPopulation(size: Int): Population = {
    copyWith(
      (1 to size).map { _ =>
        findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
      }.toList
    )
  }
  
  def selectStrongerPopulation(size: Int): Population = randomSubPopulation(size)

  def crossoverWith(otherPopulation: Population, crossoverLikelihood: Double): Population = {
    val futureGroupedChildren = individuals.map { anIndividual =>
      Future {
        val couple = otherPopulation.findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
        anIndividual.crossoverWith(couple, crossoverLikelihood)
      }
    }

    val futureChildren = futureGroupedChildren.foldLeft(Future(List[Individual]())) { case (result, futureNextChildren) =>
      for {
        children <- result
        nextChildren <- futureNextChildren
      } yield {
        children ::: nextChildren
      }
    }

    copyWith(Await.result(futureChildren, Duration.Inf))
  }
  
  def mutate(mutationLikelihood: Double): Population = copyWith(
    (1 to (POPULATION_SIZE * mutationLikelihood).toInt).map { _ =>
      findIndividualWhoseAccumulatedFitnessWindowIncludes(random.nextDouble())
        .mutate
    }.toList
  )

  override def toString: String = {
    s"""
      |***** Population summary *****
      |* Size of the population: ${individuals.size} individuals
      |* Best individual (fitness = ${bestIndividual.fitness}): $bestIndividual
      |******************************
      |""".stripMargin
  }

  lazy val bestIndividual: Individual = individuals.foldLeft(Individual.emptyIndividual(new RuntimeException())) { (firstIndividual, secondIndividual) =>
    if (firstIndividual.fitness.getOrElse(0d) >= secondIndividual.fitness.getOrElse(0d)) firstIndividual
    else secondIndividual
  }
}

object Individual {
  def emptyIndividual(exception: RuntimeException): Individual = new Individual(Failure(exception)) {
    override protected def copyWith(chromosome: Try[Chromosome]): Individual = emptyIndividual(exception)
    override def isHealthy: Boolean = false
  }
}

trait Individual(tryChromosome: Try[Chromosome])(implicit random: Random) {
  protected def copyWith(chromosome: Try[Chromosome]): Individual

  def getTryChromosome: Try[Chromosome] = tryChromosome

  def getTryGenes: Try[List[Gene]] = tryChromosome.map(_.getGenes)

  def fitness: Try[Double] = tryChromosome.map(_.fitness)

  def isHealthy: Boolean

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
                                 survivalLikelihood: Double = 0.8,
                                 crossoverLikelihood: Double = 0.5,
                                 mutationLikelihood: Double = 0.03,
                                 maxQuantityOfGenerationsWithoutImprovements: Int = 20,
                                 solutionsPopulationsSize: Int = 10
                               )

trait EvolutionRequestBodyJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val evolutionRequestJsonProtocol: RootJsonFormat[EvolutionRequestBody] = jsonFormat5(EvolutionRequestBody.apply)
}

object InitialPopulation {
  def apply(individualTypeName: String)(implicit customRandom: Random = random): Population = individualTypeName match
    case ExecutionScript.BASKET_INDIVIDUAL_TYPE_NAME => BasketsPopulation(
      (1 to POPULATION_SIZE).map(i => Basket(
        Success(ItemsList(
          (1 to (customRandom.nextInt(5) + 1)).map(_ => Item(s"Item $i", customRandom.nextInt(10), customRandom.nextInt(10))).toList
        )(customRandom))
      )).toList)
    case ExecutionScript.IMAGES_SIMILARITIES_TYPE_NAME =>
      ImagesManager.initialPopulation()
}

case object EmptyPopulation extends Population(List()) {
  override def copyWith(newIndividuals: List[Individual]): Population = this
  override def empty(): Population = this
}
