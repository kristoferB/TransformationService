package sekvensa.service

import org.joda.time.DateTime

/**
  * Created by kristofer on 2016-04-04.
  */


case class Info(name: String, time: DateTime, freq:Int)
case class OptimizationParameters(optType: String)
case class Trajectory(info: Info, optimization: OptimizationParameters, trajectory: List[List[Double]])
