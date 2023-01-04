package domain.serializers

import akka.serialization.Serializer
import spray.json.*
import domain.Operators.*
import domain.SolutionDescription.*
import domain.individuals.Item

class BasketJsonSerializer /*extends Serializer with DefaultJsonProtocol */{
/*
  implicit def basketFormatter = jsonFormat2(Execute[Item])

  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case operator @ Execute(operatorName: String, population: Population[Item]) =>
      println(s"Serializing json operator $operator")
      operator.toJson.prettyPrint.getBytes()
    case _ => throw new Exception("Only Person is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val string = new String(bytes)
    val operator = string.parseJson.convertTo[Execute[Item]]
    println(s"Deserializing json operator $operator")
    operator
  }

  override def includeManifest: Boolean = false
*/
}
