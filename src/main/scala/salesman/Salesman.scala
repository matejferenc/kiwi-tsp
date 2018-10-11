package salesman

import scala.io.StdIn

object Salesman {

  case class Flight(source: String, destination: String, day: Int, cost: Int)
  
  var numberOfAreas: Int = _
  var sourceAirport: String = _
  var sameAreaAirportsByAirport: Map[String, Seq[String]] = _
  var flightsPerDayAndAirport: Map[(Int, String), List[Flight]] = _

  def main(args: Array[String]): Unit = {
    processInput()
    solve()
  }
  
  def processInput(): Unit = {
    val (numberOfAreas, source) = readHeader()
    this.numberOfAreas = numberOfAreas
    this.sourceAirport = source

    sameAreaAirportsByAirport = readAreasWithCities(numberOfAreas)

    val flightDefinitions: List[Flight] = readFlights(numberOfAreas)

    flightsPerDayAndAirport = flightDefinitions
      .groupBy(flight => (flight.day, flight.source))
      .map(element => element._1 -> element._2.sortBy(_.cost))
      .withDefaultValue(Nil)
  }
  
  def solve(): Unit = {
    val solution = nearestNeighborWrapper(sourceAirport, numberOfAreas, sameAreaAirportsByAirport, flightsPerDayAndAirport)
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.cost))
    solution.foreach(flight => println(flight.source, flight.destination, flight.cost, flight.day))
  }

  def nearestNeighborWrapper(startAirport: String,
                             numberOfAreas: Int,
                             sameAreaAirportsByAirport: Map[String, Seq[String]],
                             flightsPerDayAndAirport: Map[(Int, String), List[Flight]]
                            ): List[Flight] = {
    def nearestNeighbor(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[Flight] = {
      val lastDay = day == numberOfAreas
      val destinationAirportsFilter: Flight => Boolean = if (lastDay) {
        flight: Flight => flight.destination == startAirport
      } else {
        flight: Flight => !visitedAirports.contains(flight.destination)
      }
      val availableFlights = flightsPerDayAndAirport((day, currentAirport))
        .filter(destinationAirportsFilter)

      if (lastDay) {
        if (availableFlights.nonEmpty) {
          availableFlights.head :: path
        } else {
          Nil
        }
      } else {
        val airportsVisitedAfterThisFlight = visitedAirports ++ sameAreaAirportsByAirport(currentAirport)

        availableFlights.toStream
          .map(flight => nearestNeighbor(day + 1, airportsVisitedAfterThisFlight, flight.destination, flight :: path))
          .find(_.nonEmpty)
          .getOrElse(Nil)
        
      }
    }

    nearestNeighbor(1, Set(), startAirport, List()).reverse
  }

  def readHeader(): (Int, String) = {
    val firstLine = StdIn.readLine()
    val splittedLine = firstLine.split(" ")
    val n = splittedLine(0).toInt
    val source = splittedLine(1)
    (n, source)
  }

  def readAreasWithCities(n: Int): Map[String, List[String]] = {
    var cityToCitiesInArea = Map[String, List[String]]()
    for (i <- 1 to n) {
      val areaName = StdIn.readLine()
      val airports = StdIn.readLine().split(" ").toList
      for (airport <- airports) cityToCitiesInArea = cityToCitiesInArea + (airport -> airports)
    }
    cityToCitiesInArea
  }

  def readFlights(totalDays: Int): List[Flight] = {
    var flightDefinitions = List[Flight]()
    var line = ""
    while ( {
      line = StdIn.readLine()
      line != null && line != ""
    }) {
      val flightLine = line.split(" ")
      val source = flightLine(0)
      val destination = flightLine(1)
      val cost = flightLine(3).toInt
      val day = flightLine(2).toInt
      if (day == 0) {
        val flightForEveryDay = for (day <- List.range(1, totalDays)) yield Flight(source, destination, day, cost)
        flightDefinitions = flightForEveryDay ::: flightDefinitions
      } else {
        val flight = Flight(source, destination, day, cost)
        flightDefinitions = flight :: flightDefinitions
      }
    }
    flightDefinitions
  }

}
