package sekvensa.service

import org.joda.time.DateTime
import org.scalatest.{FreeSpec, Matchers}

/**
  * Created by kristofer on 2016-04-05.
  */
class TransformerTest extends FreeSpec with Matchers with DummyOptimizer {
  "The Dummy optimizer" - {
    "when making new trajectories" - {
      "should hold still j1 and j2" in {
        val traj = Trajectory(Info("t1", DateTime.now()), OptimizationParameters("jointer"),
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
