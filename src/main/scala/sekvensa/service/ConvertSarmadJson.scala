package sekvensa.service

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import org.json4s.native.Serialization._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Created by kristofer on 2016-05-02.
  */
object ConvertSarmadJson extends App with FileHandling with TrajectoryConverter with EnergyOptimizer {

  val folder = "/Users/kristofer/SW/PatientDiffService/"

  //
  val sarmadJsonIn = ""
  val emiIn = "sunriseTest.json_EMI.txt"
  val friIn = "sunriseTest.json"

  val inSarmad = Try(readFromFile(folder +  sarmadJsonIn)).toOption.flatMap(x => readSarmadResult(x.mkString(" ")))
  val inEMI = Try(readFromFile(folder + emiIn)).toOption.map(x => extractJVsEMILog(x))
  val inFRI = Try(readFromFile(folder + friIn)).toOption.flatMap(x => readFRIJson(x.mkString(" ")))


  inEMI.foreach{x =>
    val traj = makeTrajectory(makePoses(x), "fromEMI")
    writeToFile(folder, emiIn + "_FRI.json", writePretty(traj))

    val sarmad = makeSarmadJson(x)
    writeToFile(folder, emiIn + "_Sarmad.json", writePretty(sarmad))
  }

  inFRI.foreach{x =>
    val jsV = makeJointValues(x.trajectory)
    writeLinesToFile(folder, friIn + "_EMI.txt", makeEMIFile(jsV))

    val sarmad = makeSarmadJson(jsV)
    writeToFile(folder, friIn + "_Sarmad.json", writePretty(sarmad))
  }

  inSarmad.foreach { x =>
    val trajs = sarmadResultsToTrajectories(x)

    trajs.zipWithIndex.foreach{case (t, i) =>
      writeToFile(folder, sarmadJsonIn + s"_FRI_$i.json", writePretty(t))

      val jsV = makeJointValues(t.trajectory)
      writeLinesToFile(folder, friIn + s"_EMI_$i.txt", makeEMIFile(jsV))
    }
  }





}

case class JointValues(t: Double, j1: Double, j2: Double, j3: Double, j4: Double, j5: Double, j6: Double) {
  def -(jv: JointValues) = {
    val dt = t - jv.t
    JointValues(t, (j1-jv.j1)/(dt), (j2-jv.j2)/(dt), (j3-jv.j3)/(dt), (j4-jv.j4)/(dt), (j5-jv.j5)/(dt), (j6-jv.j6)/(dt))
  }
  def getJoints = List(j1, j2, j3, j4, j5, j6)
  override def toString = {
    round(t,3).toString + "  " +
      j1.toString  + "  " +
      j2.toString  + "  " +
      j3.toString  + "  " +
      j4.toString  + "  " +
      j5.toString  + "  " +
      j6.toString
  }
  def round(n: Double, p: Int): Double = {
    BigDecimal(n).setScale(p, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}

trait FileHandling {

  def writeToFile(path: String, filename: String, txt: String): Unit = {
    import java.nio.file.{Paths, Files}; import java.nio.charset.StandardCharsets
    Files.write(Paths.get(path+filename), txt.getBytes(StandardCharsets.UTF_8))
  }
  def writeLinesToFile(path: String, filename: String, lines: List[String]): Unit = {
    writeToFile(path, filename, lines.mkString("\n"))
  }
  import java.nio.charset.CodingErrorAction
  import scala.io.Codec

  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
  def readFromFile(path: String) =
    scala.io.Source.fromFile(path).getLines.toList
}



trait TrajectoryConverter {
  implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all // for json serialization

  def makePoses(xs: List[JointValues]) = {
    xs.map(js => Pose(js.t, List(js.j1, js.j2, js.j3, js.j4, js.j5, js.j6)))
  }

  def makeJointValues(xs: List[Pose]) = {
    xs.flatMap(p => Try(JointValues(p.time, p.joints(0), p.joints(1), p.joints(2), p.joints(3), p.joints(4), p.joints(5))).toOption)
  }

  def makeTrajectory(poses: List[Pose], name: String, about: String = "converted") = {
    val info = Info(s"$name", DateTime.now())
    Trajectory(info, OptimizationParameters(about), poses)
  }

  def makeSarmadJson(xs: List[JointValues]) = {
    val times = xs.map(_.t)
    val totalTime = times.reverse.headOption.getOrElse(0.0)
    val traj = xs.map(_.getJoints)
    val robot = SarmadJsonRobot(times, traj, totalTime)
    SarmadJson(List(robot))
  }


  def readFRIJson(json: String) = {
    Try(read[Trajectory](json)).toOption
  }


  def zipTheLog(lines: List[String]) = {
    println(s"no of lines" + lines.size)
    val head = lines.head.split("\t").toList
    val logLines = lines.tail.map(_.split("\t"))
    logLines.map(x => (head zip x).toMap)
  }

  def extractJVsEMILog(lines: List[String]) = {
    for {
      l <- lines
      js = {val kalle = l.split("\\s+").map(_.trim).filter(_.nonEmpty); //println(kalle.toList);
        kalle}
      if js.size == 7
      j <- Try{js.map(_.toDouble)}.toOption
    } yield {
      JointValues(j(0), j(1), j(2), j(3), j(4), j(5), j(6))
    }
  }

  def extractJVs(map: Map[String, String]) = {
    for {
      ts <- map.get("Zeit [s]")
      t <- Try(ts.toDouble).toOption
      j1s <- map.get("AxisPos_CmdIpo1 [°]")
      j1 <- Try(j1s.toDouble).toOption
      j2s <- map.get("AxisPos_CmdIpo2 [°]")
      j2 <- Try(j2s.toDouble).toOption
      j3s <- map.get("AxisPos_CmdIpo3 [°]")
      j3 <- Try(j3s.toDouble).toOption
      j4s <- map.get("AxisPos_CmdIpo4 [°]")
      j4 <- Try(j4s.toDouble).toOption
      j5s <- map.get("AxisPos_CmdIpo5 [°]")
      j5 <- Try(j5s.toDouble).toOption
      j6s <- map.get("AxisPos_CmdIpo6 [°]")
      j6 <- Try(j6s.toDouble).toOption
    } yield {JointValues(t, j1, j2, j3, j4, j5, j6)}
  }

  @tailrec
  final def derJVs(xs: List[JointValues], res: List[JointValues]): List[JointValues] = xs match {
    case Nil => res.reverse
    case x :: Nil => res.reverse
    case x :: y :: xs =>
      val dy = y - x
      derJVs(y :: xs, dy :: res)
  }

  def makeTheProgStruct(lines: List[String]) = {
    val init: (List[Map[String, String]], Option[Map[String, String]]) = (List(), None)
    val fold = lines.foldLeft(init){case ((infos, temp), line) =>
      line match {
        case "#BEGINMOTIONINFO" =>
          (infos, Some(Map()))
        case "#ENDMOTIONINFO" if temp.nonEmpty =>
          (temp.get :: infos, None)
        case x if temp.nonEmpty =>
          val split = x.split(":").toList
          val addToTemp = temp.map(tempMap => if (split.size == 2) tempMap + (split.head -> split.tail.head) else tempMap)
          (infos, addToTemp)
        case _ => (infos, temp)

      }
    }
    fold._1.reverse
  }

  def filter(xs: List[Map[String, String]]) = xs.map(x => getTheTime(x) -> x).filter(_._1.nonEmpty).map(x => x._1.get -> x._2)
  def getTheTime(m: Map[String, String]) = {
    val value = m.get("Zeit [s]").orElse(m.get("TIME"))
    value.flatMap(v => Try(v.toDouble).toOption)
  }

  def jointSpeedAccAndJerk(xs: List[JointValues]) = {
    val start = JointValues(0,0,0,0,0,0,0)
    val speed = derJVs(xs, List(start))
    val acc = derJVs(speed, List(start))
    val jerk = derJVs(acc, List(start))
    (xs, speed, acc, jerk)
  }

  def mergeJoints(xs: List[JointValues]) = {
    xs.map{x =>
      val join = x.j1*x.j1 + x.j2*x.j2+ x.j3*x.j3+ x.j4*x.j4+ x.j5*x.j5+ x.j6*x.j6
      (x.t, math sqrt(join / 6))
    }
  }

  def makeEMIFile(jVs: List[JointValues]) = {
    val init = (-1.0, List[JointValues]())
    val fixedTime = jVs.foldLeft(init)((a,b)=>
      if (a._1 < 0){
        val t = b.t
        (t, List(b.copy(t = 0.0)))
      } else {
        val newV = b.t - a._1
        (a._1, b.copy(t = b.t - a._1) :: a._2)
      }
    )._2.reverse

    val header = List("[HEADER]", " GEAR_NOMINAL_VEL = 1.000000", "  CRC = 2339249579", "[RECORDS]")
    val lastLine = "[END]"
    val body = fixedTime.map(_.toString)
    (header ++ body) :+ lastLine
  }

  def allStandStills(xs: List[JointValues]) = {
    val jsaj = jointSpeedAccAndJerk(xs)
    val s = mergeJoints(jsaj._2).filter(_._2 <= 0.001).map(_._1)
    val a = mergeJoints(jsaj._3).filter(_._2 <= 0.001).map(_._1)
    val j = mergeJoints(jsaj._4).filter(_._2 <= 0.001).map(_._1)
    s.filter(x => a.contains(x) && j.contains(x)).sortWith(_ < _)
  }
}