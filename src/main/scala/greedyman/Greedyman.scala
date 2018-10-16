package greedyman

import java.io.{BufferedReader, InputStream, InputStreamReader}

import scala.collection.mutable.ArrayBuffer
import Main.FlightMap

case class Flight(from: String, to: String, day: Int, price: Int) {
  def toOutputString: String = List(from, to, day, price).mkString(" ")
}
case class Problem(areaCount: Int, start: String, areas: Map[String, Seq[String]], flights: FlightMap)

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
    val flightsPerDayAndAirport = flightDefinitions
      .groupBy(flight => (flight.day, flight.from))
      .map(element => element._1 -> element._2.sortBy(_.price))
      .withDefaultValue(Nil)
      .asInstanceOf[FlightMap]
    Problem(numberOfAreas, sourceAirport, areas, flightsPerDayAndAirport)
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.price))
    solution.foreach(flight => println(flight.toOutputString))
  }

  def solve(problem: Problem): List[Flight] = {
    val orderedFlights = problem.flights.flatMap(record => record._2)
      .toList
      .filterNot(flight => flight.day == 1 && flight.from != problem.start)
      .sortBy(_.price)
    val flightsBook = orderedFlights.groupBy(flight => (flight.day, flight.from, flight.to)).withDefaultValue(Nil)
    wrapper(problem.start, problem.areaCount, problem.areas, orderedFlights, flightsBook)
  }

  def wrapper(startAirport: String,
              numberOfAreas: Int,
              areas: Map[String, Seq[String]],
              flights: Seq[Flight],
              flightsBook: Map[(Int, String, String), Seq[Flight]]
             ): List[Flight] = {
    def calculate(flewFrom: Map[Int, String], arrivedTo: Map[Int, String], path: List[Flight], flewFromSet: Set[String], arrivedToSet: Set[String]): List[Flight] = {
      val lastDay = path.length == numberOfAreas - 1
      val destinationAirportsFilter: Flight => Boolean = {
        flight: Flight =>
          if (flight.day == numberOfAreas) {
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

      //      println("0: " + path)
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
          .map(flight => {
            val newFlewFrom = flewFrom + (flight.day -> flight.from)
            val newArrivedTo = arrivedTo + (flight.day -> flight.to)
            val newPath = flight :: path
            val newFlewFromSet = flewFromSet ++ areas(flight.from)
            val newArrivedToSet = arrivedToSet ++ areas(flight.to)
            
            if (flewFrom.isDefinedAt(flight.day + 2) && !flewFrom.isDefinedAt(flight.day + 1)) {
              flightsBook((flight.day + 1, flight.to, flewFrom(flight.day + 2)))
                .sortBy(_.price)
                .headOption
                .map(betweenFlight => calculate(newFlewFrom + (betweenFlight.day -> betweenFlight.from),
                  newArrivedTo + (betweenFlight.day -> betweenFlight.to),
                  betweenFlight :: newPath,
                  newFlewFromSet ++ areas(betweenFlight.from),
                  newArrivedToSet ++ areas(betweenFlight.to)
                ))
                .getOrElse(Nil)
            } else if (arrivedTo.isDefinedAt(flight.day - 2) && !arrivedTo.isDefinedAt(flight.day - 1)) {
              flightsBook((flight.day - 1, arrivedTo(flight.day - 2), flight.from))
                .sortBy(_.price)
                .headOption
                .map(betweenFlight => calculate(newFlewFrom + (betweenFlight.day -> betweenFlight.from),
                  newArrivedTo + (betweenFlight.day -> betweenFlight.to),
                  betweenFlight :: newPath,
                  newFlewFromSet ++ areas(betweenFlight.from),
                  newArrivedToSet ++ areas(betweenFlight.to)
                ))
                .getOrElse(Nil)
            } else {
              calculate(newFlewFrom, newArrivedTo, newPath, newFlewFromSet, newArrivedToSet)
            }
          })
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    calculate(Map(), Map(), List(), Set(), Set()).sortBy(_.day)
  }

}
