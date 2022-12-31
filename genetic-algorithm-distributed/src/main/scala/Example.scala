import akka.actor.ActorSystem
import akka.actor._

class Hola() extends Actor with ActorLogging {
  override def receive: Receive = {
    case _ => println("¡HOLUUUUS!!!!")
  }
}

object Example extends App {

  val system = ActorSystem("hola")

  val a = system.actorOf(Props(new Hola()), "miActor")

  a ! "mensaje"
}
