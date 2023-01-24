package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.*

import scala.annotation.tailrec

object Test extends App {

  val population: Population = BasketsPopulationRandomGenerator.randomPopulation(500)

  // TODO: USAR TAIL RECURSION
  // TODO: QUE EL MASTER AL ARMAR LA NUEVA POBLACIÓN PARA EL NUEVO OPERADOR LOS ORDENE POR FITNESS, Y LE PASE A CADA WORKER UNA POBLACIÓN, DONDE EL PRIMER INDIVIDUO ES EL QUE HAY QUE CRUZAR Y EL RESTO LA POBLACIÓN ORIGINAL
  // TODO: USAR BÚSQUEDA BINARIA EN LA LISTA PARA SABER CON CUÁL CRUZAR (NO TRANSFORMAR A TREE)

  println(population.individuals.map(_.fitness))
  println(population.accumulatedFitness.map(_._2))


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
