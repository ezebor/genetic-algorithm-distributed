package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.*

import scala.annotation.tailrec
import scala.util.Random

object Test extends App {

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(500)


/*  println(population.individuals.map(_.fitness))
  println(population.accumulatedFitness.map(_._2))*/


  println(s"Tamaño total de la población: ${population.individuals.size}")
  val random = new Random()

  // TODO: master calcula los random de la mitad de la población, cada worker elige un chunk de la población siguiendo esos random
  // TODO: cuando el master recibió los chunks de los workers, arma la población de los que encara, y le manda una porción a cada worker para que los cruce

  val lookIndividualForFitness: Int => List[(Individual, Double)] => (Individual, Double) = aRandomFitness => { accumulatedFitness =>
    if(accumulatedFitness.size == 1) accumulatedFitness.head
    else {
      val middleIndex = accumulatedFitness.size / 2
      val accumulatedFitnessAtTheMiddle = accumulatedFitness(accumulatedFitness.size / 2)
      val fitnessAtTheMiddle = accumulatedFitnessAtTheMiddle._2
      lookIndividualForFitness(aRandomFitness) {
        if(aRandomFitness <= fitnessAtTheMiddle) accumulatedFitness.take(middleIndex)
        else accumulatedFitness.takeRight(middleIndex)
      }
    }
  }

  val totalAccumulatedFitness = population.accumulatedFitness.last._2.toInt
  val randomFitness = (1 to population.individuals.size / 2).map(_ => random.nextInt(totalAccumulatedFitness) + 1).toList
  val individuals = randomFitness
    .map(random => lookIndividualForFitness(random)(population.accumulatedFitness))

  println(individuals)


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
