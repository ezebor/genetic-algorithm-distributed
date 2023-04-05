package integration

import akka.actor.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.routing.{FromConfig, RoundRobinGroup}
import akka.testkit.{EventFilter, ImplicitSender, TestActorRef, TestKit, TestProbe}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.typesafe.config.ConfigFactory
import domain.Execute
import domain.Operators.*
import domain.entities.*
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.server._
import Directives._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BasketsEvolutionSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  "Baskets evolution" should {
    "find the best basket using one worker for a small population" in {
      
    }
  }
}
