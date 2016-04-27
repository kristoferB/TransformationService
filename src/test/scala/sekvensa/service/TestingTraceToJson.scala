package sekvensa.service

import org.scalatest.{FreeSpec, Matchers}
import org.json4s.native.Serialization._

import scala.annotation.tailrec
import scala.util.Try

trait FilesNStuff {
  // Script settings
  val folder = "/Users/kristofer/Dropbox/Sarmad - Kristofer/AREUS DAI/toRemove/Emily_030RB_100/"
  val logFile = "TraceORIG_KRCIpo.asc"
  val emiLogFile = "opt_traj.txt"
  val sarmadJsonFile = "orig_traj3.json"
  val progFile = "TraceORIG_PROG.TXT"
  val jsonFile = "TraceORIG.json"
  val start = 64.068
  val end = 66.996
  val emiFile = "emiToHome.txt"
  val pretty = true

  lazy val fileLines = readFromFile(folder + logFile)
  lazy val markingLines = readFromFile(folder + progFile)
  lazy val emiLines = readFromFile(folder + emiLogFile)

  def writeToFile(path: String, filename: String, txt: String): Unit = {
    import java.nio.file.{Paths, Files}; import java.nio.charset.StandardCharsets
    Files.write(Paths.get(path+filename), txt.getBytes(StandardCharsets.UTF_8))
  }
  def writeLinesToFile(path: String, filename: String, lines: List[String]): Unit = {
    writeToFile(path, filename, lines.mkString("\n"))
  }

  def readFromFile(path: String) =
    scala.io.Source.fromFile(path, "UTF-8").getLines.toList
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


case class Mark(robot: Int, entersAtSample: Int, exitsAtSample: Int)
case class SarmadJsonRobot(time: List[Double],
                      trajectory: List[List[Double]],
                      makespan: Double,
                      samplingRate: Double = 0.012,
                      timeToleranceMax: Double = 0.012,
                      timeToleranceMin: Double = 0.012,
                      epsilonT: Double = 0.012,
                      costScaleFactor: Double = 0.012,
                      velocityLimit: List[Double] = (1 to 6).map(x => 200.0).toList,
                      accelerationLimit: List[Double]  = (1 to 6).map(x => 2000.0).toList,
                      jerkLimit: List[Double] = (1 to 6).map(x => 15000.0).toList,
                      weights: List[List[Double]] = List(List(20, 20, 20, 10, 7, 5))
                     )
case class SarmadJson(robots: List[SarmadJsonRobot],
                      sharedZones: List[List[Mark]] = List(),
                      preservedZones: List[Mark] = List()
                     )

trait SarmadJsonTest {
  implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all // for json serialization
  def makeSarmadJson(xs: List[JointValues]) = {
    val times = xs.map(_.t)
    val totalTime = times.reverse.headOption.getOrElse(0.0)
    val traj = xs.map(_.getJoints)
    val robot = SarmadJsonRobot(times, traj, totalTime)
    SarmadJson(List(robot))
  }


//  val times = List[Double]()
//  val trajectory = List[List[Double]]()
//  val robot = SPAttributes(
//    "makespan" -> 8.0,
//    "samplingRate" -> 0.012,
//    "timeToleranceMax" -> 0.1,
//    "timeToleranceMin" -> 0.001,
//    "epsilonT" -> 0.001,
//    "costScaleFactor" -> 100,
//    "velocityLimit" -> (1 to 6).map(x => 200),
//    "accelerationLimit" -> (1 to 6).map(x => 2000),
//    "jerkLimit" -> (1 to 6).map(x => 15000),
//    "weights" -> List(List(20, 20, 20, 10, 7, 5)),
//    "time" -> times,
//    "trajectory" -> trajectory
//  )
//
//  val r1Z = Mark(0, 100, 300)
//  val r2Z = Mark(1, 301, 400)
//
//  val request = SPAttributes(
//    "robots" -> List(robot, robot),
//    "sharedZones" -> List(List()),
//    "preservedZones" -> List()
//  )
}


trait TraceNProgEater {
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




  //case class MotionInfo(time: Double, module: String, function: String, motionType: String, signal: String, line: Int, point: String, coord: String, blend: String, vel: String, acc: String, base: String, tool: String, ipo: String)
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

  def allStandStills(xs: List[JointValues]) = {
    val jsaj = jointSpeedAccAndJerk(xs)
    val s = mergeJoints(jsaj._2).filter(_._2 <= 0.001).map(_._1)
    val a = mergeJoints(jsaj._3).filter(_._2 <= 0.001).map(_._1)
    val j = mergeJoints(jsaj._4).filter(_._2 <= 0.001).map(_._1)
    s.filter(x => a.contains(x) && j.contains(x)).sortWith(_ < _)
  }
}

/**
  * Created by kristofer on 2016-04-05.
  */
class TestingTraceToJson extends FreeSpec with Matchers with DummyOptimizer with FilesNStuff with TraceNProgEater with SarmadJsonTest {

//  "Parse the trace file" in {
//    val lines = fileLines // file.lines.toList
//    println(s"no of lines" + lines.size)
//    val zip = zipTheLog(lines)
//    println("a log line")
//    zip.head.foreach(println)
//    println(s"testing the time: "+ getTheTime(zip.head))
//  }
//
//  "parse the emi file" in {
//    val lines = emiLines
//    println(s"no of lines" + lines.size)
//    val zip = extractJVsEMILog(lines)
//    println("a log line emi:")
//    println(zip.head)
//  }
//
//  "parse the prog file" in {
//    val lines = markingLines
//    val info = makeTheProgStruct(lines)
//    println()
//    println("a info struct")
//    info.head.map(println)
//    println(s"testing the time: "+ getTheTime(info.head))
//
//  }
//
//  "merge trace and prog" in {
//    val logWT = filter(zipTheLog(fileLines))
//    val marksWT = filter(makeTheProgStruct(markingLines))
//    val sorted = (logWT ++ marksWT).sortWith(_._1 < _._1)
//    val json = if (pretty) writePretty(sorted) else write(sorted)
//    //writeToFile(folder, jsonFile, json)
//  }

//  "find standstill" in {
//    val logWT = zipTheLog(fileLines)
//    val jVs = logWT.flatMap(extractJVs)
//    val still = allStandStills(jVs)
//    println(s"All stills: $still")
//  }

  "emi to sarmad" in {
    val emi = extractJVsEMILog(emiLines)
    val sarmad = makeSarmadJson(emi)
    println("sarmad")
    writeToFile(folder, sarmadJsonFile, (writePretty(sarmad)))
  }

  "traceToEMI" in {
//
//    val completeLog = zipTheLog(fileLines)
//    val complLogWT = filter(completeLog)
//    val log = complLogWT.filter(x => x._1 >= start && (end < 0 || x._1 <= end)).map(_._2)
////
//    val jVs = log.flatMap(extractJVs)
//    val init = (-1.0, List[JointValues]())
//    val fixedTime = jVs.foldLeft(init)((a,b)=>
//      if (a._1 < 0){
//        val t = b.t
//        (t, List(b.copy(t = 0.0)))
//      } else {
//        val newV = b.t - a._1
//        (a._1, b.copy(t = b.t - a._1) :: a._2)
//      }
//    )._2.reverse
//
//    val header = List("[HEADER]", " GEAR_NOMINAL_VEL = 1.000000", "  CRC = 2339249579", "[RECORDS]")
//    val lastLine = "[END]"
//
//    val body = jVs.map(_.toString)
//    header.map(println)
//    body.map(println)
//    println(lastLine)
//
//    val body2 = fixedTime.map(_.toString)
//    header.map(println)
//    body2.map(println)
//    println(lastLine)
//
//    val res = (header ++ body2) :+ lastLine
//
//    writeLinesToFile(folder, emiFile, res)
  }


  "emiToEMI" in {

    val emi = extractJVsEMILog(emiLines)
    val jVs = emi.filter(x => x.t >= start && (end < 0 || x.t <= end))

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

    val body = jVs.map(_.toString)
    header.map(println)
    body.map(println)
    println(lastLine)

    val body2 = fixedTime.map(_.toString)
    header.map(println)
    body2.map(println)
    println(lastLine)

    val res = (header ++ body2) :+ lastLine

    writeLinesToFile(folder, emiFile, res)
  }

}



/*
  #BEGINMOTIONINFO
  TIME: 0.660000
  MODULE: LOADTEST
  FUNCTION/PROCEDURE: LOADTEST
  TYPE: PTP
  SIGNAL: END
  LINE: 61
  POINT NAME: Xdown1
  POINT COORDINATES: { x 10.943561, y 524.660217, z 892.697205, a 88.804993, b 89.937164, c -0.000082, s 2, t 3 }
  BLENDING: NONE
  VELOCITIES: { a1 100.000000, a2 100.000000, a3 100.000000, a4 100.000000, a5 100.000000, a6 100.000000 }
  ACCELERATIONS: { a1 100.000000, a2 100.000000, a3 100.000000, a4 100.000000, a5 100.000000, a6 100.000000 }
  BASE: { x 0.000000, y 0.000000, z 0.000000, a 0.000000, b 0.000000, c 0.000000 }
  TOOL: { x 0.000000, y 0.000000, z 0.000000, a 0.000000, b 0.000000, c 0.000000 }
  IPO MODE: BASE
    #ENDMOTIONINFO
 */