package multithreaded_random

import java.io.{FileInputStream, InputStream}

import Flights.FlightMap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.Random

case class Problem(areaCount: Int, start: String, areas: mutable.Map[String, Seq[String]], flights: FlightMap) {
  lazy val startArea = areas(start).toSet
  def lastFlight(day: Int) = day == areaCount
  def finalDay(day: Int) = day > areaCount
}

case class Flight(from: String, to: String, day: Int, price: Int) {
  def toOutputString = List(from, to, day, price).mkString(" ")
}

object Flights {
  type FlightMap = Map[(Int, String), IndexedSeq[Flight]]
  def fromList(flights: Seq[Flight]): FlightMap = {
    flights
      .groupBy(f => (f.day, f.from))
      .mapValues(_.toIndexedSeq.sortBy(_.price))
      .withDefaultValue(IndexedSeq.empty)
  }
}

object Problem {
  def fromInputStream(in: InputStream): Problem = {
    import java.io.{BufferedReader, InputStreamReader}
    val buffer = new BufferedReader(new InputStreamReader(in))

    val l = buffer.readLine().split(" ")
    val areaCount = l(0).toInt
    val start = l(1).trim()

    val areas = mutable.Map[String, Seq[String]]()
    for (i <- 0 until areaCount) {
      val areaName = buffer.readLine() // not really necessary for anything
      val airports = buffer.readLine().split(" ").toSeq.map(_.intern())
      for (airport <- airports) areas(airport) = airports
    }

    val flights = mutable.ArrayBuffer[Flight]()
    var line = buffer.readLine()
    while (line != null) {
      val trimmed = line.trim()
      if (trimmed.nonEmpty) {
        val split = trimmed.split(" ")
        val from = split(0).intern()
        val to = split(1).intern()
        val day = split(2).toInt
        val price = split(3).toInt

        if (day == 0) (1 to areaCount).foreach(day => flights.append(Flight(from, to, day, price)))
        else flights.append(Flight(from, to, day, price))
      }
      line = buffer.readLine()
    }

    Problem(areaCount, start, areas, Flights.fromList(flights))
  }
}

object Main extends App {

  class TimeoutException extends Exception

  implicit class RichPath(path: List[Flight]) {
    def price = path.map(_.price).sum
  }

  def processInput(in: InputStream): Problem = {
    Problem.fromInputStream(in)
  }

  def solve(problem: Problem): Unit = {

    def eligibleFlight(day: Int, visitedAirports: Set[String])(flight: Flight): Boolean = {
      val flightBackHome = problem.lastFlight(day) && problem.startArea.contains(flight.to)
      val notYetVisited = !visitedAirports.contains(flight.to)
      flightBackHome || notYetVisited
    }

    val random = new Random()

    def findSolution(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[Flight] = {
      if (problem.finalDay(day)) path
      else {
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        val outboundFlights = problem.flights((day, currentAirport))
          .filter(eligibleFlight(day, newVisitedAirports))

        if (outboundFlights.nonEmpty) {
          // occasionally be more adventurous
          val coeff = if (random.nextInt(300) == 0) 5 else 50
          val randomIndex = math.min((math.abs(random.nextGaussian() / coeff) * outboundFlights.size).toInt, outboundFlights.size - 1)
          val flight = outboundFlights(randomIndex)
          findSolution(day + 1, newVisitedAirports, flight.to, flight :: path)
        } else Nil
      }
    }

    var bestPrice = Integer.MAX_VALUE
    while (true) {
      val solution = optimize(problem, (1 to 1000).par
        .map(_ => findSolution(1, Set(), problem.start, Nil).reverse)
        .filter(_.nonEmpty)
        .minBy(_.price)
      )

      if (solution.nonEmpty && solution.price < bestPrice) {
        println(s"Price: ${solution.price} Solution: $solution")
        bestPrice = solution.price
      }
    }

  }

  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    TotalOptimizer.optimize(problem, solution)
  }

  val inputStream = new FileInputStream(getClass.getResource(s"/4.in").getPath)
  solve(processInput(inputStream))
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
      val sol = NeighborAreaSwitchOptimizer.optimize(problem,
        InAreaOptimizer.optimize(problem,
          TwoAreasSwitchOptimizer.optimize(problem, optimizedSolution)
        )
      )
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