package sekvensa

import akka.actor.ActorSystem
import akka.event.Logging

object Main extends App {

  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  val logger = Logging(system, "SimpleService")

  val transformActor = system.actorOf(sekvensa.service.Transformer.props)
  transformActor ! "connect"

  val transformListener = system.actorOf(sekvensa.service.TransformerListener.props)
  transformListener ! "connect"

  scala.io.StdIn.readLine("Press ENTER to exit application.\n") match {
    case x => system.terminate()
  }


  // Start a rest API - example
  //val (interface, port) = (config.getString("http.interface"), config.getInt("http.port"))
  //logger.info(s"Starting sekvensa.service on port $port")
  //Http().bindAndHandle(routes, interface, port)
}
