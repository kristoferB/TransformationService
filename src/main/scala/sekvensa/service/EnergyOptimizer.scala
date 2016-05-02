package sekvensa.service

import org.json4s.native.Serialization._

import scala.util.Try

/**
  * Created by kristofer on 2016-04-26.
  */
trait EnergyOptimizer {
  def fixSamples(traj: Trajectory) = {
    if (traj.trajectory.isEmpty) traj
    else {
      val drop = traj.trajectory.dropWhile(_.joints == traj.trajectory.head.joints)
      val first = traj.trajectory.takeWhile(_.joints == traj.trajectory.head.joints).reverse.head
      val temp = (first :: drop).map(x => x.copy(time = round(x.time-first.time,3)))

      val filter = temp.foldLeft(List[Pose]()) { (list, p) =>
        list match {
          case Nil => List(p)
          case x :: xs if p.time - x.time < 0.012 => list
          case x :: xs => p :: list
        }
      }

      val t = filter.head.time
      val ending = (filter.head.copy(time = round(t+0.024,3)) :: filter.head.copy(time = round(t+0.012, 3)) :: filter.head :: filter.dropWhile(_.joints == filter.head.joints)).reverse

      traj.copy(trajectory = ending)
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