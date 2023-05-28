package domain.actors

import akka.actor.*
import domain.Execute
import domain.Operators.*
import domain.entities.Population

val MASTER_ROLE = "master"
val WORKER_ROLE = "worker"

trait Parallel {
  def distributeWork(receiver: ActorRef, population: Population, chunkSize: Int, quantityOfEOFMessages: Int): Unit = {
    val chunks: Vector[Population] = population.intoChunks(chunkSize)
    val dataIndexes = chunks.indices.take(chunks.size - quantityOfEOFMessages)
    val eofIndexes = chunks.indices.takeRight(quantityOfEOFMessages)

    // TODO optimización: mandar a cada receiver todo de una (hacer deep dive en vez de round robin, para que los workers ya vayan trabajando)
    // TODO: el quantityOfEOFMessages indica la cantidad de cortes que hay que hacer para decirle a cada worker que empiece a trabajar
    dataIndexes.foreach(index => receiver ! Execute(ADD_POPULATION, chunks(index)))
    eofIndexes.foreach(index => receiver ! Execute(LAST_INDIVIDUALS, chunks(index)))
  }
}

trait BaseActor extends Parallel with Actor with ActorLogging {

  type Operator = Population => Unit

  def waitingPopulations(operator: Operator, accumulatedPopulation: Population, pendingActorsResponses: Int): Receive = {
    case Execute(ADD_POPULATION, incomingPopulation) =>
      context.become(
        waitingPopulations(
          operator,
          incomingPopulation.fusionWith(accumulatedPopulation),
          pendingActorsResponses
        )
      )
    case Execute(LAST_INDIVIDUALS, incomingPopulation) =>
      if (pendingActorsResponses <= 1) {
        operator(incomingPopulation.fusionWith(accumulatedPopulation))
      } else {
        context.become(
          waitingPopulations(
            operator,
            incomingPopulation.fusionWith(accumulatedPopulation),
            pendingActorsResponses - 1
          )
        )
      }
  }
}
