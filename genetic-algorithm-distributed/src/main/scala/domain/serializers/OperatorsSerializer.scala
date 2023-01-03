package domain.serializers

import akka.serialization.Serializer

object OperatorsSerializer {
/*
  import spray.json._
  import domain.Operators.*

  class PersonJsonSerializer extends Serializer with DefaultJsonProtocol {
    implicit val naturalSelectionFormat = jsonFormat1(NaturalSelection)

    override def identifier: Int = 1712

    override def toBinary(o: AnyRef): Array[Byte] = o match {
      case operator: NaturalSelection =>
        operator.toJson.prettyPrint.getBytes()
      case _ => throw new Exception("Only Person is supported to be serialized")
    }

    override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
      val string = new String(bytes)
      val person = string.parseJson.convertTo[Person]
      println(s"Deserializing json person $person")
      person
    }

    override def includeManifest: Boolean = false
  }
*/
}
