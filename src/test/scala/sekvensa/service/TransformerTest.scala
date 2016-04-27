package sekvensa.service

import org.joda.time.DateTime
import org.scalatest.{FreeSpec, Matchers}
import org.json4s._
import org.json4s.native.Serialization._
import com.github.nscala_time.time.Imports._
import scala.util.Try

/**
  * Created by kristofer on 2016-04-05.
  */
class TransformerTest extends FreeSpec with Matchers with DummyOptimizer {


  "The real optimizer" - {
    "should reduce samples" in {
      val traj = Trajectory(Info("t1", DateTime.now()), OptimizationParameters("energy"),
        List(
          Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(0.1, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(0.2, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(0.210, List(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)),
          Pose(0.211, List(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)),
          Pose(0.212, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
          Pose(0.214, List(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)),
          Pose(0.224, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
        )
      )
      val filtered = List(
        Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
        Pose(0.012, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
        Pose(0.024, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0)),
        Pose(0.036, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0)),
        Pose(0.048, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
      )

      val sarmad = new EnergyOptimizer {}
      val res = sarmad.fixSamples(traj)
      assert(filtered == res.trajectory)
    }
    "should convert to json" in {
      val traj = Trajectory(Info("t1", DateTime.now()), OptimizationParameters("energy"),
        List(
          Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(0.012, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
          Pose(0.024, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
        )
      )

      val sarmad = new EnergyOptimizer {}
      val res = sarmad.convertTrajectory(traj)
      println(res)
      //assert(filtered == res.trajectory)
    }
    "should fix the trace" in {

      implicit val formats = org.json4s.DefaultFormats ++ org.json4s.ext.JodaTimeSerializers.all
      val f = scala.io.Source.fromFile("/Users/kristofer/SW/PatientDiffService/sunriseTest.json", "UTF-8").mkString("")
      val trajO  = Try{read[Trajectory](f)}.foreach{ traj =>
        println(traj.info)
        val sarmad = new EnergyOptimizer {}
        val res = sarmad.fixSamples(traj)
        println(s"no of samples: ${res.trajectory.size} and before ${traj.trajectory.size}")
        val newJson = sarmad.convertTrajectory(res)
        println("json:")
        println(newJson)
      }

    }
  }
  "The Dummy optimizer" - {
    "when making new trajectories" - {
      "should hold still j1 and j2" in {
        val traj = Trajectory(Info("t1", DateTime.now()), OptimizationParameters("energy"),
          List(
            Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
            Pose(1.0, List(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)),
            Pose(2.0, List(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)),
            Pose(3.0, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
            Pose(4.0, List(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)),
            Pose(5.0, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
          )
        )

        val opt = createNewTraj(traj)

        var res = List(
          Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(1.0, List(1.0, 2.0, 4.0, 5.0, 6.0, 7.0)),
          Pose(2.0, List(1.0, 2.0, 5.0, 6.0, 7.0, 8.0)),
          Pose(3.0, List(1.0, 2.0, 6.0, 7.0, 8.0, 9.0)),
          Pose(4.0, List(1.0, 2.0, 7.0, 8.0, 9.0, 10.0)),
          Pose(5.0, List(1.0, 2.0, 8.0, 9.0, 10.0, 11.0))
        )

        println(s"jointer: $opt")
        assert(res == opt)

      }
      "should add extra joints" in {
        val traj = Trajectory(Info("t1", DateTime.now()), OptimizationParameters("slowDown"),
          List(
            Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
            Pose(1.0, List(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)),
            Pose(2.0, List(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)),
            Pose(3.0, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
            Pose(4.0, List(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)),
            Pose(5.0, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
          )
        )

        val opt = createNewTraj(traj)

        var res = List(
          Pose(0.0, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Pose(1.0, List(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)),
          Pose(2.0, List(2.0, 3.0, 4.0, 5.0, 6.0, 7.0)),
          Pose(3.0, List(2.5, 3.5, 4.5, 5.5, 6.5, 7.5)),
          Pose(4.0, List(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)),
          Pose(5.0, List(3.5, 4.5, 5.5, 6.5, 7.5, 8.5)),
          Pose(6.0, List(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)),
          Pose(7.0, List(4.5, 5.5, 6.5, 7.5, 8.5, 9.5)),
          Pose(8.0, List(5.0, 6.0, 7.0, 8.0, 9.0, 10.0)),
          Pose(9.0, List(5.5, 6.5, 7.5, 8.5, 9.5, 10.5)),
          Pose(10.0, List(6.0, 7.0, 8.0, 9.0, 10.0, 11.0))
        )

        println(s"slowDown: $opt")
        assert(res == opt)

      }
    }
  }
}
