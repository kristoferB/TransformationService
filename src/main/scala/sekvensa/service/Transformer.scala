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

/**
  * Created by kristofer on 2016-02-08.
  * An example map transformation that listens on elvis patient and returns a diff
  */
class Transformer extends Actor {
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
  var currentState: List[ElvisPatient] = List()



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
      val patientMap: Map[String, List[ElvisPatient]]  = read[Map[String, List[ElvisPatient]]](body.toString)
      val patients = patientMap("patients") // will fail if wrong structure
      println(s"No of patients: ${patients.size}")
      checkTheDiff(patients)
    }
  }


  def checkTheDiff(ps: List[ElvisPatient]) = {
    if (currentState.isEmpty) {
      currentState = ps
      ps.foreach{p =>
        val newP = NewPatient(getNow, p)
        val json = write(Map("data"->newP, "isa"->"newLoad"))
        sendToEvah(json)
      }
    }
    else if (currentState != ps)  {
      val changes = ps.filterNot(currentState.contains)
      val removed = currentState.filterNot(p => ps.exists(_.CareContactId == p.CareContactId))

      changes.map{ p =>
        val diffP = diffPat(p, currentState.find(_.CareContactId == p.CareContactId))
        diffP match {
          case None => {
            val newP = NewPatient(getNow, p)
            val json = write(Map("data"->newP, "isa"->"new"))
            sendToEvah(json)
          }
          case Some(d) => {
            val diffPatient = PatientDiff(d._1, d._2, d._3)
            val json = write(Map("data"->diffPatient, "isa"->"diff"))
            sendToEvah(json)
            }
          }
        }

      removed.map{p =>
        val removedPat = RemovedPatient(getNow, p)
        val json = write(Map("data"->removedPat, "isa"->"removed"))
        sendToEvah(json)
      }
      currentState = ps
    }
  }

  def sendToEvah(json: String) = {
    theBus.foreach{bus => bus ! SendMessage(Topic(writeTo), AMQMessage(json))}
  }



  def toNewPat(p: ElvisPatient)= {
    val t = p.CareContactRegistrationTime
    NewPatient(t,p)
  }


  def diffPat(curr: ElvisPatient, old: Option[ElvisPatient])={
    old.map {
      case prev: ElvisPatient => {
        (Map(
          "CareContactId" -> Some(Extraction.decompose(curr.CareContactId)),
          "CareContactRegistrationTime" -> diffThem(prev.CareContactRegistrationTime, curr.CareContactRegistrationTime),
          "DepartmentComment" -> diffThem(prev.DepartmentComment, curr.DepartmentComment),
          "Location" -> diffThem(prev.Location, curr.Location),
          "PatientId" -> Some(Extraction.decompose(curr.PatientId)),
          "ReasonForVisit" -> diffThem(prev.ReasonForVisit, curr.ReasonForVisit),
          "Team" -> diffThem(prev.Team, curr.Team),
          "VisitId" -> diffThem(prev.VisitId, curr.VisitId),
          "VisitRegistrationTime" -> diffThem(prev.VisitRegistrationTime, curr.VisitRegistrationTime),
          "timestamp" -> Some(Extraction.decompose(getNow))
        ).filter(kv=> kv._2 != None).map(kv=> kv._1 -> kv._2.get),
          curr.Events.filterNot(prev.Events.contains),
          prev.Events.filterNot(curr.Events.contains))
      }
    }

  }

  def diffThem[T](prev: T, current: T): Option[JValue]= {
    if (prev == current) None
    else Some(Extraction.decompose(current))
  }

  def getNow = {
    DateTime.now(DateTimeZone.forID("Europe/Stockholm"))
  }
}

object Transformer {
  def props = Props[Transformer]

}
