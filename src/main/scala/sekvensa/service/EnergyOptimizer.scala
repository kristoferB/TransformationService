package sekvensa.service

import org.joda.time.DateTime
import org.json4s.native.Serialization._

import scala.util.Try

/**
  * Created by kristofer on 2016-04-26.
  */
trait EnergyOptimizer {
  def fixSamples(traj: Trajectory, sampleFactor: Int = 4) = {
    if (traj.trajectory.isEmpty) traj
    else {
      val t = traj.trajectory
      val first = t.head
      val last = t.last

      val downSample = first :: t.zipWithIndex.collect{case (e, i) if ((i+1) % sampleFactor) == 0 => e}

      val dropFirst  = downSample.dropWhile(_.joints == first.joints)
      val dropLast = dropFirst.reverse.dropWhile(_.joints == last.joints)
      val drop = dropLast.reverse

      val diff = timeBetweenSamples(drop)

      val b1 = first.copy(time = drop.head.time-diff)
      val e1 = last.copy(time = dropLast.head.time+diff)
      val e2 = last.copy(time = dropLast.head.time+diff*2)
      val e3 = last.copy(time = dropLast.head.time+diff*3)

      val resWrongTime = (b1 :: drop) ++ List(e1, e2, e3)



      val res = resWrongTime.map(x => x.copy(time = round(x.time-resWrongTime.head.time, 5)))




//      var tempT = t.head.time
//      println("org:")
//      t.foreach{p =>
//        println(p.time - tempT)
//        tempT = p.time
//      }
//
//      println("")
//      println("New:")
//      tempT = res.head.time
//      res.foreach{p =>
//        println(p.time - tempT)
//        tempT = p.time
//      }

      //val downSample = t.zipWithIndex.collect{case (e,i) if ((i+1) % sampleFactor) == 0 => e}

      /*
      val drop = traj.trajectory.dropWhile(_.joints == traj.trajectory.head.joints)
      val first = traj.trajectory.takeWhile(_.joints == traj.trajectory.head.joints).reverse.head
      val temp = (first :: drop).map(x => x.copy(time = round(x.time-first.time,3)))

      val filter = temp.foldLeft(List[Pose]()) { (list, p) =>
        list match {
          case Nil => List(p)
          case x :: xs if p.time - x.time < sampleFactor => list
          case x :: xs => p :: list
        }
      }

      val filterDropEnd = filter.dropWhile(_.joints == filter.head.joints)

      val t = filterDropEnd.head.time
      val ending = (filterDropEnd.head.copy(time = round(t+sampleFactor*2,3)) :: filterDropEnd.head.copy(time = round(t+sampleFactor, 3)) :: filterDropEnd).reverse
     */

      traj.copy(trajectory = res)
    }
  }

  def timeBetweenSamples(t: List[Pose]) = {
    t match {
      case Nil => 0.0
      case x :: Nil => 0.0
      case x :: y :: xs => math.abs(y.time - x.time)
    }
  }


  // Must be correct sampling before called
  def convertTrajectory(traj: Trajectory) = {
   implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all
    val times = traj.trajectory.map(_.time)
    val joints = traj.trajectory.map(_.joints)
    val makespan = times.reverse.headOption.getOrElse(0.0)
    writePretty(SarmadJson(robots = List(SarmadJsonRobot(times, joints, makespan))))
  }

  def readSarmadJson(json: String) = {
    implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all
    Try(read[SarmadJson](json)).toOption
  }

  def readSarmadResult(json: String) = {
    implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all
    Try{read[SarmadResult](json)}.toOption
  }

  def sarmadResultsToTrajectories(xs: SarmadResult) = {
    val zip = xs.result.map(x => x.optimizedTime zip x.interpolatedTrajectory)
    val poses = zip.map(x => x.map(j => Pose(j._1, j._2)))
    poses.map(jv => Trajectory(Info("result", DateTime.now), OptimizationParameters("optimization"), jv))
  }

  def round(n: Double, p: Int): Double = {
    BigDecimal(n).setScale(p, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}

case class Mark(robot: Int, entersAtSample: Int, exitsAtSample: Int)
case class SarmadJsonRobot(time: List[Double],
                           trajectory: List[List[Double]],
                           makespan: Double,
                           samplingRate: Double = 0.020,
                           targetSamplingRate: Double = 0.005,
                           timeToleranceMax: Double = 0.1,
                           timeToleranceMin: Double = 0.001,
                           epsilonT: Double = 0.00001,
                           costScaleFactor: Double = 0.001,
                           velocityLimit: List[Double] = List(
                             300.0,
                             225.0,
                             225.0,
                             380.0,
                             310.0,
                             490.0),
                           accelerationLimit: List[Double]  = List(
                             4482.791667,
                             3206.041667,
                             5353.125000,
                             1970.798611,
                             5091.215278,
                             1670.270833),
                           jerkLimit: List[Double] = List(
                             264820.023148,
                             79008.101852,
                             142232.638889,
                             10192.12963,
                             131814.814815,
                             10549.189815
                           ),
                           weights: List[List[Double]] = List(List(
                             0.460000,
                             1.000000,
                             0.120000,
                             0.004000,
                             0.013000,
                             0.015000))
                          )
case class SarmadJson(robots: List[SarmadJsonRobot],
                      sharedZones: List[List[Mark]] = List(),
                      preservedZones: List[Mark] = List()
                     )

case class SarmadResultTrajectory(optimizedTime: List[Double], interpolatedTrajectory: List[List[Double]])
case class SarmadResult(result: List[SarmadResultTrajectory])