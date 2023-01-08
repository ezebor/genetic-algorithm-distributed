package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.BasketGenerator

object Test extends App {

  val population = BasketGenerator.generateRandomPopulation(10)

  val serializer = new ExecuteBasketJsonSerializer

  val message1 = Execute(EVOLUTION, population)

  val json = serializer.write(message1)

  val message2 = serializer.read(json)

  println(message1)
  println(message2)
  println(message1 == message2)

}
