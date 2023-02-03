package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.annotation.tailrec
import scala.util.Random

object Test extends App {

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(130)
/*
  println(population.accumulatedFitness)
  println(population.findIndividualWhoseAccumulatedFitnessWindowIncludes(2000))
*/

println(new Random().nextDouble())
println(new Random().nextDouble())
println(new Random().nextDouble())


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
