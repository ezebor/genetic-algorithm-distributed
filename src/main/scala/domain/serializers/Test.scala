package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.entities.*

import scala.annotation.tailrec
import scala.util.Random

object Test extends App {

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(1000)

  println(population.accumulatedFitness)
  println(population.findIndividualWhoseAccumulatedFitnessIncludes(2000))

  val a = List(1800, 100)
  println(a.takeRight(1))

  println(s"Tamaño total de la población: ${population.individuals.size}")
  val random = new Random()

  // TODO: master calcula los random de la mitad de la población, cada worker elige un chunk de la población siguiendo esos random
  // TODO: cuando el master recibió los chunks de los workers, arma la población de los que encara, y le manda una porción a cada worker para que los cruce



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
