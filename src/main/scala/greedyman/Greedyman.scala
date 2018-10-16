package greedyman

import java.io.{BufferedReader, InputStream, InputStreamReader}

import scala.collection.mutable.ArrayBuffer

case class Flight(from: String, to: String, day: Int, price: Int) {
  def toOutputString: String = List(from, to, day, price).mkString(" ")
}

case class Problem(areaCount: Int, start: String, areas: Map[String, Seq[String]], flights: Seq[Flight])

object Main {
  type FlightMap = Map[(Int, String), Seq[Flight]]

  var reader: BufferedReader = _

  def main(args: Array[String]): Unit = {
    writeSolution(solve(processInput(System.in)))
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
    reader = new BufferedReader(new InputStreamReader(inputStream))
    val (numberOfAreas, sourceAirport) = readHeader()
    val areas = readAreasWithCities(numberOfAreas)
    val flightDefinitions = readFlights(numberOfAreas)
    val flights = flightDefinitions
      .filterNot(flight => flight.day == 1 && flight.from != sourceAirport)
      .sortBy(_.price)
    Problem(numberOfAreas, sourceAirport, areas, flights)
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.price))
    solution.foreach(flight => println(flight.toOutputString))
  }

  def solve(problem: Problem): List[Flight] = {
    wrapper(problem.start, problem.areaCount, problem.areas, problem.flights)
  }

  def wrapper(startAirport: String,
              numberOfAreas: Int,
              areas: Map[String, Seq[String]],
              flights: Seq[Flight]
             ): List[Flight] = {
    def calculate(flewFrom: Map[Int, String], arrivedTo: Map[Int, String], path: List[Flight], flewFromSet: Set[String], arrivedToSet: Set[String]): List[Flight] = {
      val lastDay = path.length == numberOfAreas - 1
      val destinationAirportsFilter: Flight => Boolean = {
        flight: Flight => if (flight.day == numberOfAreas) {
          areas(startAirport).contains(flight.to)
        } else {
          !arrivedToSet.contains(flight.to)
        }
      }
      val sourceAirportsFilter: Flight => Boolean = {
        flight: Flight => !flewFromSet.contains(flight.from)
      }
      val filterForTomorrowFlightContinuity: Flight => Boolean = {
        flight: Flight =>
          if (flewFrom.isDefinedAt(flight.day + 1)) {
            flewFrom(flight.day + 1) == flight.to
          } else true
      }
      val filterForYesterdayFlightContinuity: Flight => Boolean = {
        flight: Flight =>
          if (arrivedTo.isDefinedAt(flight.day - 1)) {
            arrivedTo(flight.day - 1) == flight.from
          } else true
      }
      val availableFlights = flights
        .filter(destinationAirportsFilter)
        .filter(sourceAirportsFilter)
        .filter(flight => !flewFrom.keys.exists(_ == flight.day)) // no flight on this day
        .filter(filterForYesterdayFlightContinuity) // flight to destination where I fly from tomorrow
        .filter(filterForTomorrowFlightContinuity) // flight from destination where I flew from yesterday

      println("0: " + path)
//      println("1: " + flights.filter(flight => !flewFrom.keys.exists(_ == flight.day)))
//      println("2: " + flights.filter(filterForYesterdayFlightContinuity))
//      println("3: " + flights.filter(filterForTomorrowFlightContinuity))
//      println("4: " + flights.filter(destinationAirportsFilter))
//      println("5: " + flights.filter(sourceAirportsFilter))
//      println("6: " + availableFlights)
//      println("-------")

      if (lastDay) {
        if (availableFlights.nonEmpty) {
          availableFlights.head :: path
        } else {
          Nil
        }
      } else {
        availableFlights.toStream
          .map(flight => calculate(flewFrom + (flight.day -> flight.from), arrivedTo + (flight.day -> flight.to), flight :: path, flewFromSet ++ areas(flight.from), arrivedToSet ++ areas(flight.to)))
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    calculate(Map(), Map(), List(), Set(), Set()).sortBy(_.day)
  }

}
