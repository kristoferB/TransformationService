package sekvensa.service

import akka.actor._
import com.codemettle.reactivemq._
import com.codemettle.reactivemq.ReActiveMQMessages._
import com.codemettle.reactivemq.model._
import com.typesafe.config.ConfigFactory
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import com.github.nscala_time.time.Imports._

import scala.util.Try

/**
  * Created by kristofer on 2016-02-08.
  * An example map transformation that listens on elvis patient and returns a diff
  */
class Transformer extends Actor with DummyOptimizer with EnergyOptimizer {
  implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all // for json serialization

  // reading from config file
  val config = ConfigFactory.load()
  val address = config.getString("sp.activemq.address")
  val user = config.getString("sp.activemq.user")
  val pass = config.getString("sp.activemq.pass")
  val readFrom = config.getString("sp.simpleservice.readFromTopic")
  val writeTo = config.getString("sp.simpleservice.writeToTopic")

  // The state
  var theBus: Option[ActorRef] = None



  def receive = {
    case "connect" => {
      ReActiveMQExtension(context.system).manager ! GetAuthenticatedConnection(s"nio://$address:61616", user, pass)
    }
    case ConnectionEstablished(request, c) => {
      println("connected:"+request)
      c ! ConsumeFromTopic(readFrom)
      theBus = Some(c)
    }
    case ConnectionFailed(request, reason) => {
      println("failed:"+reason)
    }
    case mess @ AMQMessage(body, prop, headers) => {
      val topic = prop.destination.map(_.name).getOrElse("")
      val traj  = Try{read[Trajectory](body.toString)}.toOption

      val optType = traj.map(_.optimization.optType).getOrElse("jointer")

      val res = traj.map{createNewTraj}.getOrElse(List())


      val optTrajName = traj.map(_.info.name).getOrElse("noName") +"_opt"
      val optPara = traj.map(_.optimization).getOrElse(OptimizationParameters("Can not convert"))
      val resTray = Trajectory(Info(optTrajName, DateTime.now), optPara, res)
      sendToLisa(write(resTray))
    }
  }

  override def postStop() = {
    theBus.map(_ ! CloseConnection)
  }


  def sendToLisa(json: String) = {
    theBus.foreach{bus => bus ! SendMessage(Topic(writeTo), AMQMessage(json))}
  }

}

object Transformer {
  def props = Props[Transformer]

}




trait DummyOptimizer {
  def createNewTraj(t: Trajectory) = {
    implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all
    println("The optimizer got: "+write(t))
    t.optimization.optType match {
      case "slowDown" =>
        t.trajectory.foldLeft(List[Pose]())((a, b) => {
          a.headOption match {
            case None => List(b)
            case Some(p) =>
              val zip = p.joints zip b.joints
              val newT = (p.time + b.time*2) / 2
              val newB = b.copy(time = b.time*2)
              newB :: Pose(newT, zip.map(kv => (kv._1+kv._2)/2)) :: a
          }
        }).reverse
      case "jointer" if t.trajectory.nonEmpty =>
        val j1 = Try{t.trajectory.head.joints(0)}.getOrElse(0.0)
        val j2 = Try{t.trajectory.head.joints(1)}.getOrElse(0.0)
        t.trajectory.map{p => p.copy(joints = List(j1, j2) ++ p.joints.drop(2))}
      case _ => List()
    }
  }
}



class TransformerListener extends Actor {
  implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all // for json serialization

  // reading from config file
  val config = ConfigFactory.load()
  val address = config.getString("sp.activemq.address")
  val user = config.getString("sp.activemq.user")
  val pass = config.getString("sp.activemq.pass")
  val readFrom = config.getString("sp.simpleservice.readFromTopic")
  val writeTo = config.getString("sp.simpleservice.writeToTopic")

  // The state
  var theBus: Option[ActorRef] = None



  def receive = {
    case "connect" => {
      ReActiveMQExtension(context.system).manager ! GetAuthenticatedConnection(s"nio://$address:61616", user, pass)
    }
    case ConnectionEstablished(request, c) => {
      println("connected:"+request)
      c ! ConsumeFromTopic(writeTo)
      theBus = Some(c)
    }
    case ConnectionFailed(request, reason) => {
      println("failed:"+reason)
    }
    case mess @ AMQMessage(body, prop, headers) => {
      println(s"The optimized json: $body")
    }
  }

  override def postStop() = {
    theBus.map(_ ! CloseConnection)
  }


}

object TransformerListener {
  def props = Props[TransformerListener]

}