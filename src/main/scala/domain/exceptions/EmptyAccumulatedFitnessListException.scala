package domain.exceptions

import domain.entities.Population

case class EmptyAccumulatedFitnessListException(population: Population) extends RuntimeException {
  override def getMessage: String = s"The accumulatedFitness list is empty. Population: $population"
}
