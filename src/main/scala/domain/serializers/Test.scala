package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.*

import scala.annotation.tailrec

object Test extends App {

  val population = BasketGenerator.generateRandomPopulation(500)

  // TODO: USAR TAIL RECURSION
  val mapToFitnessTree: Population => FitnessTree = aPopulation => {
    // TODO: QUE EL MASTER AL ARMAR LA NUEVA POBLACIÓN PARA EL NUEVO OPERADOR LOS ORDENE POR FITNESS, Y LE PASE A CADA WORKER UNA POBLACIÓN, DONDE EL PRIMER INDIVIDUO ES EL QUE HAY QUE CRUZAR Y EL RESTO LA POBLACIÓN ORIGINAL
    // TODO: USAR BÚSQUEDA BINARIA EN LA LISTA PARA SABER CON CUÁL CRUZAR (NO TRANSFORMAR A TREE)
    val populationWithAccumulatedFitness = aPopulation
      .zipWithIndex
      .foldLeft(List[LeveledIndividual]()) { case (result, (individual, index)) =>
        if(index == 0) result :+ (individual, individual.fitness)
        else result :+ (individual, individual.fitness + result(index - 1)._2)
      }

    def middleElement(populationWithAccumulatedFitness: LeveledPopulation) = {
      val populationLength = populationWithAccumulatedFitness.length
      val middleIndex = populationLength / 2
      populationWithAccumulatedFitness(middleIndex)
    }

    @tailrec
    def buildFitnessTree(populationChunk: LeveledPopulation): FitnessTree = {
      if(populationChunk.length == 1) Leaf(populationChunk.head)
      else {
        val populationLength = populationWithAccumulatedFitness.length
        val middleIndex = populationLength / 2
        Node(
          middleElement(populationWithAccumulatedFitness),
          buildFitnessTree(populationWithAccumulatedFitness.drop(middleIndex)),
          buildFitnessTree(populationWithAccumulatedFitness.dropRight(populationLength - middleIndex))
        )
      }
    }

    val populationLength = populationWithAccumulatedFitness.length
    val middleIndex = populationLength / 2
    Node(
      middleElement(populationWithAccumulatedFitness),
      buildFitnessTree(populationWithAccumulatedFitness.drop(middleIndex)),
      buildFitnessTree(populationWithAccumulatedFitness.dropRight(populationLength - middleIndex))
    )
  }

  val maxFitness: FitnessTree => Double = {
    case Node(_, _, rightTree) => maxFitness(rightTree)
    case Leaf(individualWithAccumulatedFitness) => individualWithAccumulatedFitness._2
  }

  /*println(populationWithAccumulatedFitness.last._2)
  println(maxFitness(toFitnessTree(population)))*/


  /*

  val serializer = new ExecuteBasketJsonSerializer

  val message1 = Execute(EVOLUTION, population)

  val json = serializer.write(message1)

  val message2 = serializer.read(json)

  println(message1)
  println(message2)
  println(message1 == message2)
*/
}
