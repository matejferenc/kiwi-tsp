package evaluator

import java.io.FileInputStream

import scala.collection.mutable.ArrayBuffer

// COMMENT / UNCOMMENT TO SWITCH SOLUTIONS
//import nearest_neighbor.{Main, Flight, Problem}

//import greedyman.{Flight, Main, Problem}

//import superman.{Main, Flight, Problem}

//import randomman.{Main, Flight, Problem}

//import splitter.{Main, Flight, Problem}

import genes.{Main, Flight, Problem}

object Evaluator {

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 4) {
      optimizeTestCase(i)
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

  def optimizeTestCase(number: Int): Unit = {
    val inputStream = new FileInputStream(getClass.getResource(s"/$number.in").getPath)
    val problem = Main.processInput(inputStream)
    val subOptimalSolution = Main.solve(problem)
    val solution = Optimizer.optimize(problem, subOptimalSolution.to[collection.mutable.ArrayBuffer]).toList
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
        Some(s"Each area should be visited just once. Offending areas: visited multiple times ${visitedAreas.filter { case (k, v) => v != 1 }}, not visited at all: ${totalAreasSet -- visitedAreasSet}")
      else None
    }

    List(firstAirport, lastAirPort, numberOfFlights, continuity, areas).flatten
  }
}

object Optimizer {
  def optimize(problem: Problem, solution: ArrayBuffer[Flight]): ArrayBuffer[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    while (optimized) {
      val (o, s) = optimizeOnce(problem, optimizedSolution)
      optimized = o
      optimizedSolution = s
    }
    optimizedSolution
  }
  
  def optimizeOnce(problem: Problem, solution: ArrayBuffer[Flight]): (Boolean, ArrayBuffer[Flight]) = {
    val solutionPrice = solution.map(_.price).sum
    var betterSolution = solution.clone()
    var betterPrice = solutionPrice
    var optimizedDay = 1
    while (betterPrice >= solutionPrice && optimizedDay < problem.areaCount) {
      val firstFlightToOptimize = solution(optimizedDay - 1)
      val secondFlightToOptimize = solution(optimizedDay)
      val current2FlightsPrice = firstFlightToOptimize.price + secondFlightToOptimize.price
      val midDestination = firstFlightToOptimize.to
      val finalDestination = secondFlightToOptimize.to
      val airportsInArea = problem.areas(midDestination)
      problem.flights((optimizedDay, firstFlightToOptimize.from))
        .filter(flight => airportsInArea.contains(flight.to))
        .filterNot(_.to == midDestination)
        .flatMap(flight => {
          problem.flights((optimizedDay + 1, flight.to))
            .filter(flight => airportsInArea.contains(flight.from))
            .filter(_.to == finalDestination)
            .map(secondFlight => (flight, secondFlight))
        })
        .find(betterFlights => current2FlightsPrice > (betterFlights._1.price + betterFlights._2.price))
        .foreach(betterFlights => {
          betterSolution(optimizedDay - 1) = betterFlights._1
          betterSolution(optimizedDay) = betterFlights._2
          betterPrice = solutionPrice - current2FlightsPrice + betterFlights._1.price + betterFlights._2.price
        })
      optimizedDay = optimizedDay + 1
    }
    (betterPrice < solutionPrice, betterSolution)
  }
}