package sekvensa.service

import org.scalatest.{FreeSpec, Matchers}
import org.json4s.native.Serialization._

import scala.util.Try

trait FilesNStuff {

  // Script settings
  val folder = "/Users/kristofer/Dropbox/Sarmad - Kristofer/KB-TRACE/"
  val logFile = "TraceORIG_KRCIpo.asc"
  val progFile = "TraceORIG_PROG.TXT"
  val jsonFile = "TraceORIG.json"
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

trait TraceNProgEater {
  def zipTheLog(lines: List[String]) = {
    println(s"no of lines" + lines.size)
    val head = lines.head.split("\t").toList
    val logLines = lines.tail.map(_.split("\t"))
    logLines.map(x => (head zip x).toMap)
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
    writeToFile(folder, jsonFile, json)
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