package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.*

import scala.annotation.tailrec
import scala.util.Random

object Test extends App {

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(500)

  println(population.accumulatedFitness)
  println(population.accumulatedFitness.last._2)

  val random = new Random()
  println(random.nextDouble())
  println(random.nextDouble())
  println(random.nextDouble())

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
