package genes

import java.io.InputStream

import Flights.FlightMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

//Submission details #6737
//ID	Result	Score	Time	Memory
//#1	accepted	93.5	2.85s	324096KB
//#6	accepted	115.89	4.86s	323776KB
//#11	accepted	104.98	14.84s	323776KB
//#14	accepted	104.93	14.86s	323520KB

case class Problem(areaCount: Int, start: String, areas: mutable.Map[String, Seq[String]], flights: FlightMap) {
  lazy val startArea = areas(start).toSet
  def lastFlight(day: Int) = day == areaCount
  def finalDay(day: Int) = day > areaCount
}

case class Flight(from: String, to: String, day: Int, price: Int) {
  def key = Flights.key(day, from)
  def toOutputString = List(from, to, day, price).mkString(" ")
}

object Flights {
  type FlightMap = Map[(Int, String), Seq[Flight]]
  def key(day: Int, from: String) = (day, from)
  def fromList(flights: Seq[Flight]): FlightMap = {
    flights
      .groupBy(_.key)
      .mapValues(_.sortBy(_.price))
      .withDefaultValue(Seq.empty)
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

case class Timeout(startTs: Long, timeoutMs: Long) {
  def shouldTerminate(): Boolean = (System.currentTimeMillis() - startTs) >= timeoutMs
}
object Timeout {
  def solution(startTs: Long, airports: Int, areas: Int): Timeout = {

    val timeoutMs = if (areas <= 20 && airports < 50) 3000 - 500
    else if (areas <= 100 && areas < 100) 5000 - 700
    else 15000 - 1000
    
    Timeout(startTs, timeoutMs)
  }
  def optimization(startTs: Long, airports: Int, areas: Int): Timeout = {
    val Gap = 300

    val timeoutMs = (if (areas <= 20 && airports < 50) 3000
    else if (areas <= 100 && areas < 100) 5000
    else 15000) - Gap

    Timeout(startTs, timeoutMs)
  }
}

class TimeoutException extends Exception

object Main extends App {


  implicit class RichPath(path: List[Flight]) {
    def price = path.map(_.price).sum
  }

  var startTs: Long = _

  def processInput(in: InputStream): Problem = {
    startTs = System.currentTimeMillis()
    Problem.fromInputStream(in)
  }

  def solve(problem: Problem): List[Flight] = {
    val timeout = Timeout.solution(startTs, problem.areas.keys.size, problem.areaCount)

    def eligibleFlight(day: Int, visitedAirports: Set[String])(flight: Flight): Boolean = {
      val flightBackHome = problem.lastFlight(day) && problem.startArea.contains(flight.to)
      val notYetVisited = !visitedAirports.contains(flight.to)
      flightBackHome || notYetVisited
    }

    var counter = 0
    var skipped = 0
    val MaxCounter = problem.areas.size * 2

    def findSolution(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight], skipMap: Map[String, Int]): List[Flight] = {
      counter += 1
      if (counter > MaxCounter) {
        skipped += 1
        throw new TimeoutException
      } else if (problem.finalDay(day)) path
      else {
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        val outboundFlights = problem.flights(Flights.key(day, currentAirport))
        outboundFlights
          .filter(eligibleFlight(day, newVisitedAirports))
          .drop(skipMap(currentAirport))
          .toStream
          .map(flight => findSolution(day + 1, newVisitedAirports, flight.to, flight :: path, skipMap))
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    val r = new Random(problem.hashCode)

    val GenerationSize = 200
    val GenerationWinners = 15
    val MutationChance = 0.05

    def generateSkip = {
      val n = r.nextInt(500)
      if (n == 0) 2
      else if (n < 10) 1
      else 0
    }

    def shuffle(a: Map[String, Int], b: Map[String, Int]): Map[String, Int] = {
      def merge(vA: Int, vB: Int) =
        if (r.nextDouble < MutationChance) generateSkip
        else if (r.nextBoolean()) vA else vB

      for ((key, valueA) <- a)
        yield (key, merge(valueA, b(key)))
    }

    var generation = for (i <- 0 to GenerationSize) yield {
      problem.areas.keys
        .map(k => (k, generateSkip))
        .toMap
    } ++ Map().withDefaultValue(0)

    var allWinners = ArrayBuffer[List[Flight]]()

    try {
      while (!timeout.shouldTerminate()) {

        val solutions = generation.map { instance =>
          if (timeout.shouldTerminate()) throw new TimeoutException
          counter = 0
          val solution = try {
            findSolution(1, Set(), problem.start, Nil, instance).reverse
          } catch {
            case e: TimeoutException => Nil
          }
          (instance, solution)
        }.filter(_._2.nonEmpty)

        val winners = solutions.sortBy(_._2.price).take(GenerationWinners)
        val winnerInstances = winners.map(_._1)

        allWinners.append(winners.map(_._2): _*)

        val shuffles = for {
          a <- winnerInstances
          b <- winnerInstances
          if a != b
        } yield shuffle(a, b)

        generation = shuffles
      }
    } catch {
      case e: TimeoutException => Nil
    }

    //println(s"$skipped paths skipped")

    allWinners.sortBy(_.price).head
  }

  def optimize(problem: Problem, solution: List[Flight]): List[Flight] = {
    val timeout = Timeout.optimization(startTs, problem.areas.keys.size, problem.areaCount)
    TotalOptimizer.optimize(problem, solution, timeout)
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.price)
    solution.foreach(flight => println(flight.toOutputString))
  }

  val problem = processInput(System.in)
  writeSolution(optimize(problem, solve(problem)))
}

object InAreaOptimizer {
  def optimize(problem: Problem, solution: List[Flight], timeout: Timeout): List[Flight] = {
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
  def optimize(problem: Problem, solution: List[Flight], timeout: Timeout): List[Flight] = {
    var optimized = true
    var optimizedSolution = solution
    var previousPrice = solution.map(_.price).sum
    while (optimized && !timeout.shouldTerminate()) {
      try {
        val (_, s1) = TwoAreasSwitchOptimizer.optimizeOnce(problem, optimizedSolution)
        if (timeout.shouldTerminate()) throw new TimeoutException
        val (_, s2) = InAreaOptimizer.optimizeOnce(problem, s1)
        if (timeout.shouldTerminate()) throw new TimeoutException
        val (_, s3) = NeighborAreaSwitchOptimizer.optimizeOnce(problem, s2)
        val sol = s3
        val currentPrice = sol.map(_.price).sum
        optimized = currentPrice < previousPrice
        previousPrice = currentPrice
        optimizedSolution = sol
      } catch {
        case e: TimeoutException => {}
      }
    }
    optimizedSolution
  }
}

object NeighborAreaSwitchOptimizer {
  def optimize(problem: Problem, solution: List[Flight], timeout: Timeout): List[Flight] = {
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
  def optimize(problem: Problem, solution: List[Flight], timeout: Timeout): List[Flight] = {
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
    var dayA1 = 1
    while (dayA1 < problem.areaCount - 2) {
      val flight1 = solution(dayA1 - 1)
      val dayA2 = dayA1 + 1
      val flight2 = solution(dayA2 - 1)
      var dayB3 = dayA2 + 1
      while (optimizedPrice >= currentPrice && dayB3 < problem.areaCount) {
        val dayB4 = dayB3 + 1
        val flight3 = solution(dayB3 - 1)
        val flight4 = solution(dayB4 - 1)
        val pricePer4Flights = flight1.price + flight2.price + flight3.price + flight4.price
        for {
          substitute1 <- problem.flights((dayA1, flight1.from)).filter(s => s.to == flight3.to)
          substitute4 <- problem.flights((dayA2, flight4.from)).filter(s => s.to == flight2.to)
          substitute3 <- problem.flights((dayB3, flight3.from)).filter(s => s.to == flight1.to)
          substitute2 <- problem.flights((dayB4, flight2.from)).filter(s => s.to == flight4.to)
          if currentPrice <= optimizedPrice
          pricePer4SubstituteFlights = substitute1.price + substitute2.price + substitute3.price + substitute4.price
          if pricePer4SubstituteFlights < pricePer4Flights
        } {
          optimizedPrice = currentPrice - pricePer4Flights + pricePer4SubstituteFlights
          betterSolution(dayA1 - 1) = substitute1
          betterSolution(dayA2 - 1) = substitute2
          betterSolution(dayB3 - 1) = substitute3
          betterSolution(dayB4 - 1) = substitute4
        }
        dayB3 = dayB3 + 1
      }
      dayA1 = dayA1 + 1
    }
    (optimizedPrice < currentPrice, betterSolution.toList.sortBy(_.day))
  }
}