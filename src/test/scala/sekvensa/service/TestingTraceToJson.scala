package sekvensa.service

import org.scalatest.{FreeSpec, Matchers}
import org.json4s.native.Serialization._

import scala.annotation.tailrec
import scala.util.Try

trait FilesNStuff {
  // Script settings
  val folder = "/Users/kristofer/Dropbox/Sarmad - Kristofer/Experiments KUKA Nordic/traces and emi/160421/multiplePaus/"
  val logFile = "TraceORIG_KRCIpo.asc"
  val progFile = "TraceORIG_PROG.TXT"
  val jsonFile = "TraceORIG.json"
  val start = 5.328
  val end = 6.924
  val emiFile = "emiMiddle.txt"
  val pretty = true

  val fileLines = readFromFile(folder + logFile)
  val markingLines = readFromFile(folder + progFile)

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

trait TraceNProgEater {
  def zipTheLog(lines: List[String]) = {
    println(s"no of lines" + lines.size)
    val head = lines.head.split("\t").toList
    val logLines = lines.tail.map(_.split("\t"))
    logLines.map(x => (head zip x).toMap)
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

  def round(n: Double, p: Int): Double = {
    val s = math pow(10,p)
    (math floor n*s)/s
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
class TestingTraceToJson extends FreeSpec with Matchers with DummyOptimizer with FilesNStuff with TraceNProgEater {
  implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all // for json serialization

  "Parse the trace file" - {
    val lines = fileLines // file.lines.toList
    println(s"no of lines" + lines.size)
    val zip = zipTheLog(lines)
    println("a log line")
    zip.head.foreach(println)
    println(s"testing the time: "+ getTheTime(zip.head))
  }

  "parse the prog file" - {
    val lines = markingLines
    val info = makeTheProgStruct(lines)
    println()
    println("a info struct")
    info.head.map(println)
    println(s"testing the time: "+ getTheTime(info.head))

  }

  "merge trace and prog" - {
    val logWT = filter(zipTheLog(fileLines))
    val marksWT = filter(makeTheProgStruct(markingLines))
    val sorted = (logWT ++ marksWT).sortWith(_._1 < _._1)
    val json = if (pretty) writePretty(sorted) else write(sorted)
    //writeToFile(folder, jsonFile, json)
  }

  "find standstill" - {
    val logWT = zipTheLog(fileLines)
    val jVs = logWT.flatMap(extractJVs)
    val still = allStandStills(jVs)
    println(s"All stills: $still")
  }

  "traceToEMI" - {
//    val completeLog = zipTheLog(fileLines)
//    val complLogWT = filter(completeLog)
//    val log = complLogWT.filter(x => x._1 >= start && (end < 0 || x._1 <= end)).map(_._2)
//
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

    //writeLinesToFile(folder, emiFile, res)
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