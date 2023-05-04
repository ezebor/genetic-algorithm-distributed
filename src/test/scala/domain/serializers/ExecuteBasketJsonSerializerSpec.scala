package domain.serializers

import app.ExecutionScript
import domain.Execute
import domain.Operators.*
import domain.entities.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Random

class ExecuteBasketJsonSerializerSpec extends AnyWordSpecLike with should.Matchers {
  val serializer = new ExecuteBasketJsonSerializer
  implicit val random: Random = new Random()

  "Serializer" should {
    "serialize and deserialize a command for the empty population" in {
      val expectedCommand = Execute(EVOLUTION, BasketsPopulation(List()))
      val currentCommand = serializer.read(serializer.write(expectedCommand))
      currentCommand should be(currentCommand)
    }

    "serialize and deserialize a command for a random population" in {
      val commands = (1 to 10).map{_ =>
        Execute(EVOLUTION, RandomPopulation(10, ExecutionScript.BASKET_INDIVIDUAL_TYPE_NAME))
      }
      commands.foreach(expectedCommand => expectedCommand should be(serializer.read(serializer.write(expectedCommand))))
    }
  }
}
