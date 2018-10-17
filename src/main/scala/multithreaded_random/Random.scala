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
  def key = Flights.key(day, from)
  def toOutputString = List(from, to, day, price).mkString(" ")
}

object Flights {
  type FlightMap = Map[String, IndexedSeq[Flight]]
  def key(day: Int, from: String) = day + from
  def fromList(flights: Seq[Flight]): FlightMap = {
    flights
      .groupBy(_.key)
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

        val outboundFlights = problem.flights(Flights.key(day, currentAirport))
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
      val solution = (1 to 1000).par
        .map(_ => findSolution(1, Set(), problem.start, Nil).reverse)
        .filter(_.nonEmpty)
        .minBy(_.price)

      if (solution.nonEmpty && solution.price < bestPrice) {
        println(s"Price: ${solution.price} Solution: ${solution}")
        bestPrice = solution.price
      }
    }


  }

  val inputStream = new FileInputStream(getClass.getResource(s"/4.in").getPath)
  solve(processInput(inputStream))
}
