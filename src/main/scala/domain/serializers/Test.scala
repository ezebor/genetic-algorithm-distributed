package domain.serializers

import domain.Execute
import domain.individuals.BasketGenerator
import domain.Operators.*

object Test extends App {

  val population = BasketGenerator.generateRandomPopulation(10)

  val serializer = new BasketJsonSerializer

  val message1 = Execute(EVOLUTION, population)

  val json = serializer.BasketJsonFormat.write(message1)

  val message2 = serializer.BasketJsonFormat.read(json)

  println(message1)
  println(message2)
  println(message1 == message2)

}
