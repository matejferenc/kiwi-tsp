package genes_mf

import java.io.{BufferedReader, InputStream, InputStreamReader}

import Main.FlightMap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Flight(from: String, to: String, day: Int, price: Int) {
  def key = Flights.key(day, from)
  def key3 = (day, from, to)
  def toOutputString: String = List(from, to, day, price).mkString(" ")
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

case class Problem(areaCount: Int, start: String, areas: Map[String, Seq[String]], flights: FlightMap) {
  lazy val startArea = areas(start).toSet
  def lastFlight(day: Int) = day == areaCount
  def finalDay(day: Int) = day > areaCount
}

class Timeout(startTs: Long, airports: Int, areas: Int) {
  val Gap = 300

  val timeoutMs = (if (areas <= 20 && airports < 50) 3000
    else if (areas <= 100 && areas < 100) 5000
    else 15000) - Gap

  def shouldTerminate() = (System.currentTimeMillis() - startTs) >= timeoutMs
}

object Main {
  type FlightMap = Map[(Int, String), Seq[Flight]]

  implicit class RichPath(path: List[Flight]) {
    def price: Int = path.map(_.price).sum
  }

  class TimeoutException extends Exception
  
  var startTs: Long = _

  var reader: BufferedReader = _
  
  def main(args: Array[String]): Unit = {
    val problem = processInput(System.in)
    writeSolution(optimize(problem, solve(problem)))
  }

  def readHeader(): (Int, String) = {
    val firstLine = reader.readLine()
    val splittedLine = firstLine.split(" ")
    val n = splittedLine(0).toInt
    val source = splittedLine(1).intern()
    (n, source)
  }

  def readAreasWithCities(n: Int): Map[String, List[String]] = {
    var cityToCitiesInArea = Map[String, List[String]]()
    for (i <- 1 to n) {
      val areaName = reader.readLine()
      val airports = reader.readLine().split(" ").toList.map(airport => airport.intern())
      for (airport <- airports) cityToCitiesInArea = cityToCitiesInArea + (airport -> airports)
    }
    cityToCitiesInArea
  }

  def readFlights(totalDays: Int): Seq[Flight] = {
    var flightDefinitions = ArrayBuffer[Flight]()
    var line = ""
    while ( {
      line = reader.readLine()
      line != null && line != ""
    }) {
      val flightLine = line.split(" ")
      val from = flightLine(0).intern()
      val to = flightLine(1).intern()
      val price = flightLine(3).toInt
      val day = flightLine(2).toInt
      if (day == 0) {
        for (day <- 1 to totalDays) {
          flightDefinitions.append(Flight(from, to, day, price))
        }
      } else {
        val flight = Flight(from, to, day, price)
        flightDefinitions.append(flight)
      }
    }
    flightDefinitions
  }

  def processInput(inputStream: InputStream): Problem = {
    startTs = System.currentTimeMillis()
    reader = new BufferedReader(new InputStreamReader(inputStream))
    val (numberOfAreas, sourceAirport) = readHeader()
    val sameAreaAirportsByAirport = readAreasWithCities(numberOfAreas)
    val flightDefinitions = readFlights(numberOfAreas)
    val flightsPerDayAndAirport = flightDefinitions
      .groupBy(flight => (flight.day, flight.from))
      .mapValues(flights => flights.groupBy(flight => flight.to).map(element => element._2.minBy(_.price)).toList)
      .map(element => element._1 -> element._2.sortBy(_.price))
      .withDefaultValue(Nil)
      .asInstanceOf[FlightMap]
    Problem(numberOfAreas, sourceAirport, sameAreaAirportsByAirport, flightsPerDayAndAirport)
  }

  def solve(problem: Problem): List[Flight] = {

    def eligibleFlight(day: Int, visitedAirports: Set[String])(flight: Flight): Boolean = {
      val flightBackHome = problem.lastFlight(day) && problem.startArea.contains(flight.to)
      val notYetVisited = !visitedAirports.contains(flight.to)
      flightBackHome || notYetVisited
    }

    val timeout = new Timeout(startTs, problem.areas.keys.size, problem.areaCount)

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
          if (a != b)
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
    solution
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.price)
    solution.foreach(flight => println(flight.toOutputString))
  }

}
