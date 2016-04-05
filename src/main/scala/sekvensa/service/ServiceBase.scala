package sekvensa.service

import akka.actor.ActorSystem

trait ServiceBase  {

  val system: ActorSystem

}
