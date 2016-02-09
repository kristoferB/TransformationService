package sekvensa

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import sekvensa.service.healthcheck.HealthCheckService

object Main extends App with HealthCheckService {

  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val logger = Logging(system, "SimpleService")

  val transformActor = system.actorOf(sekvensa.service.Transformer.props)
  transformActor ! "connect"

  // Start a rest API - example
  //val (interface, port) = (config.getString("http.interface"), config.getInt("http.port"))
  //logger.info(s"Starting service on port $port")
  //Http().bindAndHandle(routes, interface, port)
}
