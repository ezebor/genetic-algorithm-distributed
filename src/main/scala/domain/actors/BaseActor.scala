package domain.actors

import domain.Execute
import domain.Operators.*
import domain.entities.Population
import akka.actor.*

trait BaseActor extends Actor with ActorLogging {
  def waitingPopulations(nextOperatorName: String, online: Receive, accumulatedPopulation: Population, pendingActorsResponses: Int): Receive = {
    case Execute(ADD_POPULATION, incomingPopulation) =>
      context.become(
        waitingPopulations(
          nextOperatorName,
          online,
          accumulatedPopulation.fusionWith(incomingPopulation),
          pendingActorsResponses
        )
      )
    case Execute(LAST_INDIVIDUALS, incomingPopulation) =>
      if (pendingActorsResponses == 1) {
        context.become(online)
        self ! Execute(nextOperatorName, accumulatedPopulation.fusionWith(incomingPopulation))
      } else {
        context.become(
          waitingPopulations(
            nextOperatorName,
            online,
            accumulatedPopulation.fusionWith(incomingPopulation),
            pendingActorsResponses - 1
          )
        )
      }
  }
}
