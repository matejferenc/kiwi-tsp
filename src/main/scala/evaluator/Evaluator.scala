package evaluator

import java.io.FileInputStream

// COMMENT / UNCOMMENT TO SWITCH SOLUTIONS
//import nearest-neighbor.{Main, Flight, Problem}

import greedyman.{Main, Flight, Problem}

//import superman.{Main, Flight, Problem}

//import randomman.{Main, Flight, Problem}

//import splitter.{Main, Flight, Problem}

//import genes.{Main, Flight, Problem}

object Evaluator {

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 4) {
      runTestCase(i)
    }
  }

  def runTestCase(number: Int): Unit = {
    println(s"evaluating dataset $number")
    val inputStream = new FileInputStream(getClass.getResource(s"/$number.in").getPath)
    val start = System.currentTimeMillis
    val problem = Main.processInput(inputStream)
    val readingPhase = System.currentTimeMillis
    println("reading phase: " + (readingPhase - start) + " ms")
    val solution = Main.solve(problem)
    val calculationPhase = System.currentTimeMillis
    println("calculation phase: " + (calculationPhase - readingPhase) + " ms")
    println("total time: " + (calculationPhase - start) + " ms")
    println(Validator.check(problem, solution))
    println(s"Price: ${solution.map(_.price).sum}")
    println()
//    Main.writeSolution(solution)
  }

  def evaluateTestCase(number: Int): Unit = {
    val inputStream = new FileInputStream(getClass.getResource(s"/$number.in").getPath)
    val problem = Main.processInput(inputStream)
    val solution = Main.solve(problem)
    if (Validator.check(problem, solution).nonEmpty) throw new IllegalStateException("validation did not pass")
    println(solution.map(_.price).sum)
  }
}

object Validator {
  def check(problem: Problem, solution: List[Flight]): List[String] = {

    def firstAirport = if (solution.head.from != problem.start) Some("Solution should start at given airport") else None
    def lastAirPort = if (!problem.areas(problem.start).contains(solution.last.to)) Some("Solution should end at given area") else None
    def numberOfFlights = if (solution.size != problem.areaCount) Some("Wrong number of flights") else None
    def continuity = if (!solution.map(_.to).zip(solution.tail.map(_.from)).forall { case (to, from) => to == from })
      Some("Arrival and departure should be always from same city")
    else None
    def areas = {
      val visitedAreas = solution.map(_.to).map(problem.areas).map(_.mkString(" ")).groupBy(identity).mapValues(_.size)
      val visitedAreasSet = visitedAreas.keys.toSet
      val totalAreasSet = problem.areas.values.map(_.mkString(" ")).toSet
      if (visitedAreas.values.exists(_ != 1) || visitedAreasSet != totalAreasSet)
        Some(s"Each area should be visited just once. Offending areas: visited multiple times ${visitedAreas.filter { case (k, v) => v != 1}}, not visited at all: ${totalAreasSet -- visitedAreasSet}")
      else None
    }

    List(firstAirport, lastAirPort, numberOfFlights, continuity, areas).flatten
  }
}