package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.*

import scala.annotation.tailrec

object Test extends App {

  val population = BasketGenerator.generateRandomPopulation(500)

  // TODO: USAR TAIL RECURSION
  val mapToFitnessTree: Population => FitnessTree = aPopulation => {
    // TODO: QUE EL MASTER MANDE A CALCULAR POR CHUNKS ESTO A CADA WORKER. LUEGO DE QUE TODOS LOS WORKERS RESPONDAN, EL MASTER MANDA A HACER LA CRUZA. PARA ESO, TOMA LA MITAD DE LA POBLACIÓN. DE ESA MITAD, DIVIDE POR CANTIDAD DE WORKERS. A CADA WORKER LE LLEGA UNA DE ESAS PORCIONES + LA POBLACIÓN TOTAL, Y DEVUELVE UNA NUEVA POBLACIÓN QUE SURGE DE CRUZAR A LA PORCIÓN CON LA POBLACIÓN TOTAL
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
