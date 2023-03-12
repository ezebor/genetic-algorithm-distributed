package domain.exceptions

import domain.entities.Population

case class IllegalChunkSizeException(population: Population) extends RuntimeException {
  override def getMessage: String = s"The chunk size is zero, therefore the population can't be splitted up. Population: $population"
}
