import java.util.Scanner

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Flight(from: String, to: String, day: Int, price: Int) {
  def toOutputString = List(from, to, day, price).mkString(" ")
}

class Flights(days: Int) {
  val flightsMap = mutable.Map[(Int, String), ArrayBuffer[Flight]]().withDefault(_ => ArrayBuffer.empty)

  private def append(day: Int, from: String, flight: Flight): Unit = {
    val key = (day, from)
    if (!flightsMap.contains(key)) flightsMap.put(key, ArrayBuffer())
    flightsMap(key).append(flight)
  }

  private def addToAllDays(flight: Flight) =
    for (day <- 1 to days) append(day, flight.from, flight)

  def add(flight: Flight) =
    if (flight.day == 0) addToAllDays(flight)
    else append(flight.day, flight.from, flight)

  def sort() = for (key <- flightsMap.keys) flightsMap(key) = flightsMap(key).sortBy(_.price)

  def find(day: Int, from: String): Seq[Flight] = flightsMap((day, from))
}

class Timer {
  var lastMeasure = System.currentTimeMillis()
  def measure(name: String) = {
    val time = System.currentTimeMillis()
//    println(s"$name: ${time - lastMeasure}")
    lastMeasure = time
  }
}

object Main extends App {
  val timer = new Timer()
  val sc = new Scanner(System.in)

  val areaCount = sc.nextInt()
  val start = sc.nextLine().trim()

  val areas = mutable.Map[String, Seq[String]]()
  for (i <- 0 until areaCount) {
    val areaName = sc.nextLine() // not really necessary for anything
    val airports = sc.nextLine().split(" ").toSeq
    for (airport <- airports) areas(airport) = airports
  }

  timer.measure("Read areas")

  val flights = new Flights(areaCount)
  sc.useDelimiter("\\Z")
  val rest = sc.next.split("\n")
  for (line <- rest) {
    val trimmed = line.trim
    if (trimmed.nonEmpty) {
      val split = trimmed.split(" ")
      val flight = Flight(split(0), split(1), split(2).toInt, split(3).toInt)
      flights.add(flight)
    }
  }

  timer.measure("Read flights")

  flights.sort

  timer.measure("Finished reading input")

  // finds first acceptable solution, searching lowest cost flights first
  def bruteForce(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[Flight] = {
    val lastFlight = day == areaCount
    val lastDay = day > areaCount
    def eligibleFlight(flight: Flight): Boolean = (lastFlight && flight.to == start) || !visitedAirports.contains(flight.to)

    if (lastDay && currentAirport == start) path
    else {
      val newDay = day + 1
      val newVisitedAirports = visitedAirports ++ areas(currentAirport)

      val outboundFlights = flights.find(day, currentAirport)
      outboundFlights.toStream
        .filter(eligibleFlight)
        .map(flight => bruteForce(newDay, newVisitedAirports, flight.to, flight :: path))
        .find(_.nonEmpty)
        .getOrElse(Nil)
    }
  }

  val solution = bruteForce(1, Set(), start, Nil).reverse
  println(solution.map(_.price).sum)
  println(solution.map(_.toOutputString).mkString("\n"))

  timer.measure("Finished computing")
}
