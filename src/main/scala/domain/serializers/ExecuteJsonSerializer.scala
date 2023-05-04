package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.random
import spray.json.*

trait ExecuteJsonSerializer extends Serializer with DefaultJsonProtocol with RootJsonFormat[Execute] {
  protected def chromosomeOf: Individual => List[Gene]
  protected def serializeGene: Gene => JsValue
  protected def deserializeIndividual: List[JsValue] => Individual
  protected def createPopulation(individuals: List[Individual]): Population

  def write(command: Execute) = command match {
    case Execute(operatorName: String, population: Population) =>
      val formattedPopulation = JsArray(
        for {
          case individual: Individual <- population.individuals.toVector
          chromosome = chromosomeOf(individual).toVector
        } yield {
          JsObject(
            "chromosome" -> JsArray(
              for gene: Gene <- chromosome
                yield serializeGene(gene)
            )
          )
        }
      )

      JsObject(
        "operatorName" -> JsString(operatorName),
        "population" -> formattedPopulation
      )
  }

  def read(value: JsValue) = {
    value.asJsObject.getFields("operatorName", "population") match {
      case Seq(JsString(operatorName), JsArray(population)) =>
        Execute(operatorName,
          createPopulation(
            for {
              case chromosome: JsValue <- population.toList
              case JsArray(genes) <- chromosome.asJsObject.getFields("chromosome")
            } yield deserializeIndividual(genes.toList)
          )
        )
      case _ => throw new DeserializationException("Execute message expected")
    }
  }

  override def identifier: Int = 1712

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case command: Execute => command.toJson(write).prettyPrint.getBytes()
    case _ => throw new Exception("Only Basket is supported to be serialized")
  }

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    new String(bytes).parseJson.convertTo[Execute](read)
  }

  override def includeManifest: Boolean = false
}
