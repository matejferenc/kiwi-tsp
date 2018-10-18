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
    val subOptimalSolution = Main.solve(problem)
    val calculationPhase = System.currentTimeMillis
    println("solving phase: " + (calculationPhase - readingPhase) + " ms")
    val solution = Main.optimize(problem, subOptimalSolution)
    val optimizationPhase = System.currentTimeMillis
    println("optimization phase: " + (optimizationPhase - calculationPhase) + " ms")
    println("total time: " + (optimizationPhase - start) + " ms")
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
    val start = System.currentTimeMillis
    val solution = TwoAreasSwitchOptimizer.optimize(problem, subOptimalSolution)
    val end = System.currentTimeMillis
    println("optimization time: " + (end - start) + " ms")
    if (Validator.check(problem, solution).nonEmpty) throw new IllegalStateException(Validator.check(problem, solution) mkString "\n")
    println(s"${subOptimalSolution.map(_.price).sum} : ${solution.map(_.price).sum}")
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

object InAreaOptimizer {
  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    while (optimized) {
      val (o, s) = optimizeOnce(problem, optimizedSolution)
      optimized = o
      optimizedSolution = s
    }
    optimizedSolution
  }
  
  def optimizeOnce(problem: Problem, solution: List[Flight]): (Boolean, List[Flight]) = {
    val solutionPrice = solution.map(_.price).sum
    val betterSolution = solution.to[collection.mutable.ArrayBuffer]
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
    (betterPrice < solutionPrice, betterSolution.toList)
  }
}

object TotalOptimizer {
  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    var previousPrice = solution.map(_.price).sum
    while (optimized) {
      val (_, s1) = TwoAreasSwitchOptimizer.optimizeOnce(problem, optimizedSolution)
      val (_, s2) = InAreaOptimizer.optimizeOnce(problem, s1)
      val (_, s3) = NeighborAreaSwitchOptimizer.optimizeOnce(problem, s2)
      val sol = s3
      val currentPrice = sol.map(_.price).sum
      optimized = currentPrice < previousPrice
      previousPrice = currentPrice
      optimizedSolution = sol
    }
    optimizedSolution
  }
}

object NeighborAreaSwitchOptimizer {
  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    while (optimized) {
      val (o, s) = optimizeOnce(problem, optimizedSolution)
      optimized = o
      optimizedSolution = s
    }
    optimizedSolution
  }
  
  def optimizeOnce(problem: Problem, solution: List[Flight]): (Boolean, List[Flight]) = {
    var day = 1
    val currentPrice = solution.map(_.price).sum
    var optimizedPrice = currentPrice
    val betterSolution = solution.to[collection.mutable.ArrayBuffer]
    while (day < problem.areaCount - 1 && optimizedPrice >= currentPrice) {
      val firstFlightIndex = day - 1
      val secondFlightIndex = day
      val thirdFlightIndex = day + 1
      val firstFlight = solution(firstFlightIndex)
      val secondFlight = solution(secondFlightIndex)
      val thirdFlight = solution(thirdFlightIndex)
      val pricePer3Flights = firstFlight.price + secondFlight.price + thirdFlight.price
      val from = firstFlight.from
      val mid1Area = problem.areas(secondFlight.to)
      val mid2Area = problem.areas(secondFlight.from)
      val to = solution(thirdFlightIndex).to
      problem.flights((day, from))
        .filter(f => mid1Area.contains(f.to))
        .flatMap(f1 => {
          problem.flights((day + 1, f1.to))
            .filter(f => mid2Area.contains(f.to))
            .flatMap(f2 => {
              problem.flights(day + 2, f2.to)
                .filter(_.to == to)
                .map(f3 => {
                  (f1, f2, f3)
                })
            })
        })
        .find(betterFlights => pricePer3Flights > (betterFlights._1.price + betterFlights._2.price + betterFlights._3.price))
        .foreach(betterFlights => {
          betterSolution(day - 1) = betterFlights._1
          betterSolution(day) = betterFlights._2
          betterSolution(day + 1) = betterFlights._3
          optimizedPrice = currentPrice - pricePer3Flights + betterFlights._1.price + betterFlights._2.price + betterFlights._3.price
        })
      day = day + 1
    }
    (optimizedPrice < currentPrice, betterSolution.toList)
  }
}

object TwoAreasSwitchOptimizer {
  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    while (optimized) {
      val (o, s) = optimizeOnce(problem, optimizedSolution)
      optimized = o
      optimizedSolution = s
    }
    optimizedSolution
  }
  
  def optimizeOnce(problem: Problem, solution: List[Flight]): (Boolean, List[Flight]) = {
    val currentPrice = solution.map(_.price).sum
    var optimizedPrice = currentPrice
    val betterSolution = solution.to[collection.mutable.ArrayBuffer]
    for {
      dayA1 <- 1 to (problem.areaCount - 3)
      dayA2 = dayA1 + 1
      dayB3 <- (dayA2 + 1) to (problem.areaCount - 1)
      dayB4 = dayB3 + 1
      flight1 = solution(dayA1 - 1)
      flight2 = solution(dayA2 - 1)
      flight3 = solution(dayB3 - 1)
      flight4 = solution(dayB4 - 1)
      pricePer4Flights = flight1.price + flight2.price + flight3.price + flight4.price
      substitute1 <- problem.flights((dayA1, flight1.from)).filter(s => s.to == flight3.to)
      substitute4 <- problem.flights((dayA2, flight4.from)).filter(s => s.to == flight2.to)
      substitute3 <- problem.flights((dayB3, flight3.from)).filter(s => s.to == flight1.to)
      substitute2 <- problem.flights((dayB4, flight2.from)).filter(s => s.to == flight4.to)
      if currentPrice <= optimizedPrice
      pricePer4SubstituteFlights = substitute1.price + substitute2.price + substitute3.price + substitute4.price
      if pricePer4SubstituteFlights < pricePer4Flights
      _ = { optimizedPrice = currentPrice - pricePer4Flights + pricePer4SubstituteFlights }
      _ = {
        betterSolution(dayA1 - 1) = substitute1
        betterSolution(dayA2 - 1) = substitute2
        betterSolution(dayB3 - 1) = substitute3
        betterSolution(dayB4 - 1) = substitute4
      }
    } yield null
    (optimizedPrice < currentPrice, betterSolution.toList.sortBy(_.day))
  }
}