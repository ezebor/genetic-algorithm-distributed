package domain.serializers

import akka.serialization.Serializer
import domain.Execute
import domain.Operators.*
import domain.entities.*
import domain.entities.AlgorithmConfig.random
import spray.json.*

import scala.collection.immutable.Vector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait ExecuteJsonSerializer extends Serializer with DefaultJsonProtocol with RootJsonFormat[Execute] {
  protected def chromosomeOf: Individual => List[Gene]
  protected def serializeGenes(genes: Vector[Gene]): JsValue
  protected def createPopulation(genes: Vector[JsValue]): Population

  def write(command: Execute) = command match {
    case Execute(operatorName: String, population: Population) =>

      val futureFormattedPopulation = for {
        case individual: Individual <- population.individuals.toVector
        chromosome = chromosomeOf(individual).toVector
      } yield {
        Future(serializeGenes(chromosome))
      }

      val formattedPopulation = futureFormattedPopulation.foldLeft(Future(List[JsValue]())) { case (result, nextFutureFormattedIndividual) =>
        for{
          individuals <- result
          nextFormattedIndividual <- nextFutureFormattedIndividual
        } yield nextFormattedIndividual :: individuals
      }

      JsObject(
        "operatorName" -> JsString(operatorName),
        "population" -> JsArray(Await.result(formattedPopulation, Duration.Inf).toVector)
      )
  }

  def read(value: JsValue) = {
    value.asJsObject.getFields("operatorName", "population") match {
      case Seq(JsString(operatorName), JsArray(individuals)) =>
        Execute(
          operatorName,
          createPopulation(individuals)
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
