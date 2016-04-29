package sekvensa.service

import org.scalatest.{FreeSpec, Matchers}
import org.json4s.native.Serialization._

import scala.annotation.tailrec
import scala.util.Try


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

  "find standstill in trace" in {
    val logWT = zipTheLog(fileLines)
    val jVs = logWT.flatMap(extractJVs)
    val still = allStandStills(jVs)
    val fold = still.foldLeft((List[Double](),0.0))((tuple,jv) =>
      tuple._1 match {
        case Nil => (List(jv), jv)
        case x :: xs if (jv - tuple._2) - 0.0002 <= 0.012 =>
          (tuple._1, jv)
        case x :: xs if x != tuple._2 => (jv :: tuple._2 :: tuple._1, jv)
        case x :: xs => (jv :: tuple._1, jv)
      }
    )

    println(s"All stills:")
    fold._1.reverse.map(println)
    println(still)
  }

  "find standstill in emi" in {
    val still = allStandStills(extractJVsEMILog(emiLines))

    val fold = still.foldLeft((List[Double](),0.0))((tuple,jv) =>
      tuple._1 match {
        case Nil => (List(jv), jv)
        case x :: xs if (jv - tuple._2) - 0.0002 <= 0.012 =>
          (tuple._1, jv)
        case x :: xs if x != tuple._2 => (jv :: tuple._2 :: tuple._1, jv)
        case x :: xs => (jv :: tuple._1, jv)
      }
    )

    println(s"All stills:")
    fold._1.reverse.map(println)
    println(still)
  }

  "emi to sarmad" in {
    val emi = extractJVsEMILog(emiLines)
    val sarmad = makeSarmadJson(emi)
    println("sarmad")
    writeToFile(folder, sarmadJsonFile, (writePretty(sarmad)))
  }

  "traceToEMI" in {
    val completeLog = zipTheLog(fileLines)
    val complLogWT = filter(completeLog)
    val log = complLogWT.filter(x => x._1 >= start && (end < 0 || x._1 <= end)).map(_._2)
    val jVs = log.flatMap(extractJVs)
    val res = makeEMIFile(jVs)

    writeLinesToFile(folder, emiFile, res)
  }

  "emiToEMI" in {
    val emi = extractJVsEMILog(emiLines)
    val jVs = emi.filter(x => x.t >= start && (end < 0 || x.t <= end))
    val res = makeEMIFile(jVs)

    writeLinesToFile(folder, emiFile, res)
  }

  case class RobOp(name: String, beforeStart: Double, beforeEnd: Double, start: Double, end: Double)
  "Fix times and create emi operations" in {
    val folder = "C:\\Users\\krist\\Dropbox\\Sarmad - Kristofer\\AREUS DAI\\Optimization\\160428\\R30Case1/"
    val opt = readFromFile(folder + "sol.txt")
    val optEMI = readFromFile(folder + "opt_traj.txt")
    val emi = extractJVsEMILog(optEMI)


    val pairs: List[(Double, Double)] = opt.flatMap { l =>
      val tuple = l.split("\\s+").flatMap(d => Try(d.toDouble).toOption)
      if (tuple.size == 2) Some((tuple(0), tuple(1))) else None
    }



    val ops = List(
      RobOp("ToPU20",	    1,	214  ,	0.0,  0.0     ),
      RobOp("FromPU20",	  335,	499  ,	0.0,	0.0     ),
      RobOp("ToPrag",	    527,	676  ,	0.0,	0.0     )  ,
      RobOp("FromPrag",	  824,	992  ,	0.0,	0.0     )  ,
      RobOp("ToPlace_1",	    1027	,1154  ,	0.0,	0.0     )    ,
      RobOp("FromPlace_1",	    1261	,1346  ,	0.0,	0.0     )  ,
      RobOp("ToPU30_1",	  2015	,2095  ,	0.0,	0.0     )  ,
      RobOp("FromPU30_1",	  2202	,2287  ,	0.0,	0.0     )  ,
      RobOp("ToLeave_2",	  2313	,2597  ,	0.0,	0.0     )  ,
      RobOp("FromLeave_2",	3114	,3289  ,	0.0,	0.0     )
    )
    val map = pairs.toMap
    val upd = ops.map{o =>
      val tS = map(o.beforeStart)
      val tE = map(o.beforeEnd)
      o.copy(start = tS, end = tE)
    }




    var diff = 0.0
    upd.foreach {o =>
      val oldTime = o.beforeEnd*0.012-o.beforeStart*0.012
      val newTime = o.end-o.start
      diff = diff + (newTime-oldTime)
      println(s"${o.name}: oldtime: ${oldTime} newTime: ${newTime}")
      val jVs = emi.filter(x => x.t >= o.start-0.006 && (x.t <= o.end+0.006))
      val res = makeEMIFile(jVs)
      writeLinesToFile(folder, o.name+".txt", res)
    }
    println(diff)


    // Check do not touch:
    val donots = List(
      RobOp("PU2",	      175	,334 ,	0.0,  0.0     ),
      RobOp("PU2-PU1",	  500	,526 ,	0.0,	0.0     ),
      RobOp("PU1",	      669	,823  ,	0.0,	0.0     )  ,
      RobOp("PU1-PU5",	  993	,1026  ,	0.0,	0.0     )  ,
      RobOp("PU5",	      1155	,1260  ,	0.0,	0.0     )    ,
      RobOp("glue",	      1347	,2014  ,	0.0,	0.0     )  ,
      RobOp("short",	    2096	,2201  ,	0.0,	0.0     )  ,
      RobOp("pl1",	      2288	,2312  ,	0.0,	0.0     )  ,
      RobOp("pl2",	      2598	,3113  ,	0.0,	0.0     )
    )
    val don = donots.map{o =>
      val tS = map(o.beforeStart)
      val tE = map(o.beforeEnd)
      o.copy(start = tS, end = tE)
    }
    diff = 0.0
    don.foreach {o =>
      val oldTime = o.beforeEnd*0.012-o.beforeStart*0.012
      val newTime = o.end-o.start
      diff = diff + (newTime-oldTime)
      println(s"${o.name}: oldtime: ${oldTime} newTime: ${newTime}")
    }
    println(diff)


  }

  "optEmiToEMI" in {
    val folder = "C:\\Users\\krist\\Dropbox\\Sarmad - Kristofer\\AREUS DAI\\Optimization\\160428\\R10/"
    val opt = readFromFile(folder + "opt_traj.txt")
    val emi = extractJVsEMILog(opt)
    val ops = List(RobOp("ToPU2",	0,	184,	0,	2.59187),
      RobOp("FromPU2",	795,	917,	9.30294,	10.8399)     ,
      RobOp("ToPU1",	920,	1024,	10.8529,	12.5252)       ,
      RobOp("FromPU1",	1512,	1587,	17.8832,	19.36)       ,
      RobOp("ToPU5",	1591,	1741,	19.384,	21.0926)         ,
      RobOp("ToGlue",	2583,	2779,	30.3447,	32.8895)       ,
      RobOp("FromGlue",	3823,	3982,	44.4418,	46.3051)     ,
      RobOp("ToPlaceS",	4058,	4319,	47.1211,	51.0897)     ,
      RobOp("ToPlaceB",	4438,	4554,	52.3887,	53.8596)     ,
      RobOp("FromPlace1",	4746,	4903,	55.9577,	58.1903)   ,
      RobOp("FromPlace2",	4906,	5002,	58.2133,	60.024)
    )

    ops.foreach {o =>
      val jVs = emi.filter(x => x.t >= o.start-0.006 && (x.t <= o.end+0.006))
      val res = makeEMIFile(jVs)
      writeLinesToFile(folder, o.name, res)
    }

  }

}


trait FilesNStuff {
  // Script settings
  val folder = "C:\\Users\\krist\\Dropbox\\Sarmad - Kristofer\\AREUS DAI\\trace\\160428\\Trace_und_Emily_030RB_100/"
  val logFile = "R30FULL2_KRCIpo.asc"
  val emiLogFile = "opt_traj.txt"
  val sarmadJsonFile = "orig_traj3.json"
  val progFile = "R30FULL2_PROG.TXT"
  val jsonFile = "TraceORIG.json"
  val start = 46.944
  val end = 47.904
  val emiFile = "OrgToPU30_2"
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