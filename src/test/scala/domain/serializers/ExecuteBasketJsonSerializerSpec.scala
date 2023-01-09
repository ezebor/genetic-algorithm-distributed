package domain.serializers

import domain.Execute
import domain.Operators.*
import domain.individuals.BasketGenerator
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

class ExecuteBasketJsonSerializerSpec extends AnyWordSpecLike with should.Matchers {
  val serializer = new ExecuteBasketJsonSerializer

  "Serializer" should {
    "serialize and deserialize a command for the empty population" in {
      val expectedCommand = Execute(EVOLUTION, List())
      val currentCommand = serializer.read(serializer.write(expectedCommand))
      currentCommand should be(currentCommand)
    }

    "serialize and deserialize a command for a random population" in {
      val commands = (1 to 10).map{_ =>
        Execute(EVOLUTION, BasketGenerator.generateRandomPopulation(10))
      }
      commands.foreach(expectedCommand => expectedCommand should be(serializer.read(serializer.write(expectedCommand))))
    }
  }
}
