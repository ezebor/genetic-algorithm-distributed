package domain.serializers

import akka.serialization.Serializer
import domain.Operators.*
import domain.individuals.{BasketJsonProtocol, ExecuteBasket, Item}
import spray.json.*

class BasketJsonSerializer extends Serializer with BasketJsonProtocol {
  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case operator: ExecuteBasket =>
      println(s"Serializing json operator $operator")
      operator.toJson.prettyPrint.getBytes()
    case _ => throw new Exception("Only Person is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val string = new String(bytes)
    val operator = string.parseJson.convertTo[ExecuteBasket]
    println(s"Deserializing json operator $operator")
    operator
  }

  override def includeManifest: Boolean = false
}
