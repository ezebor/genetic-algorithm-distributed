package app

import akka.actor.ActorSystem
import akka.cluster.ClusterEvent.MemberEvent
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config

class PriorityMailbox(settings: ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    PriorityGenerator {
      case _: MemberEvent => 0
      case _ => 4
    }
  )
