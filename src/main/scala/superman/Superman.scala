package superman

import java.io.InputStream

import superman.Flights.FlightMap

import scala.collection.mutable

case class Problem(areaCount: Int, start: String, areas: mutable.Map[String, Seq[String]], flights: FlightMap)

case class Flight(from: String, to: String, day: Int, price: Int) {
  lazy val key = (day, from)
  def toOutputString = List(from, to, day, price).mkString(" ")
}

object Flights {
  type FlightMap = Map[(Int, String), Seq[Flight]]
  def fromList(maxDays: Int, flights: Seq[Flight]): FlightMap = {
    val (specific, general) = flights.partition(_.day != 0)

    val generalPerDay = for {
      flight <- general
      day <- 1 to maxDays
    } yield flight.copy(day = day)

    (generalPerDay ++ specific)
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
      val airports = buffer.readLine().split(" ").toSeq
      for (airport <- airports) areas(airport) = airports
    }

    val flights = mutable.ArrayBuffer[Flight]()
    var line = buffer.readLine()
    while (line != null) {
      val trimmed = line.trim()
      if (trimmed.nonEmpty) {
        val split = trimmed.split(" ")
        val flight = Flight(split(0).intern(), split(1).intern(), split(2).toInt, split(3).toInt)
        flights.append(flight)
      }
      line = buffer.readLine()
    }

    Problem(areaCount, start, areas, Flights.fromList(areaCount, flights))
  }
}

object Main extends App {
  def processInput(in: InputStream): Problem = Problem.fromInputStream(in)

  def solve(problem: Problem): List[Flight] = {
    // finds first acceptable solution, searching lowest cost flights first
    def bruteForce(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[Flight] = {
      val lastFlight = day == problem.areaCount
      val lastDay = day > problem.areaCount

      if (lastDay && currentAirport == problem.start) path
      else {
        val newDay = day + 1
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        def eligibleFlight(flight: Flight): Boolean = (lastFlight && flight.to == problem.start) || !newVisitedAirports.contains(flight.to)

        val outboundFlights = problem.flights((day, currentAirport))
        outboundFlights.toStream
          .filter(eligibleFlight)
          .map(flight => bruteForce(newDay, newVisitedAirports, flight.to, flight :: path))
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    bruteForce(1, Set(), problem.start, Nil).reverse
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.price))
    solution.foreach(flight => println(flight.from, flight.to, flight.price, flight.day))
  }

}
