import java.util.Scanner

import Main.timer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import Flights.FlightMap

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

class Timer {
  var lastMeasure = System.currentTimeMillis()
  def measure(name: String) = {
    val time = System.currentTimeMillis()
//    println(s"$name: ${time - lastMeasure}")
    lastMeasure = time
  }
}

case class Problem(areaCount: Int, start: String, areas: mutable.Map[String, Seq[String]], flights: FlightMap)

object Problem {
  def fromStdIn: Problem = {
    val sc = new Scanner(System.in)

    val areaCount = sc.nextInt()
    val start = sc.nextLine().trim()

    val areas = mutable.Map[String, Seq[String]]()
    for (i <- 0 until areaCount) {
      val areaName = sc.nextLine() // not really necessary for anything
      val airports = sc.nextLine().split(" ").toSeq
      for (airport <- airports) areas(airport) = airports
    }

    val flights = mutable.ArrayBuffer[Flight]()
    sc.useDelimiter("\\Z")
    val rest = sc.next.split("\n")
    for (line <- rest) {
      val trimmed = line.trim
      if (trimmed.nonEmpty) {
        val split = trimmed.split(" ")
        val flight = Flight(split(0).intern(), split(1).intern(), split(2).toInt, split(3).toInt)
        flights.append(flight)
      }
    }

    Problem(areaCount, start, areas, Flights.fromList(areaCount, flights))
  }
}

object Validator {
  def check(problem: Problem, solution: List[Flight]): List[String] = {

    def firstAirport = if (solution.head.from != problem.start) Some("Solution should start at given airport") else None
    def lastAirPort = if (solution.last.to != problem.start) Some("Solution should end at given airport") else None
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

object Main extends App {
  val timer = new Timer()

  val problem = Problem.fromStdIn

  timer.measure("Finished reading input")

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

  val solution = bruteForce(1, Set(), problem.start, Nil).reverse
  println(solution.map(_.price).sum)
  println(solution.map(_.toOutputString).mkString("\n"))

  //Validator.check(problem, solution).mkString("\n").foreach(println)

  timer.measure("Finished computing")
}
