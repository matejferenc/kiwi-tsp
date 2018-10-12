package salesman

import java.io.{BufferedReader, InputStream, InputStreamReader}

object Main {

  case class Flight(from: String, to: String, day: Int, price: Int)
  type FlightMap = Map[(Int, String), Seq[Flight]]
  case class Problem(areaCount: Int, start: String, areas: Map[String, Seq[String]], flights: FlightMap)
  
  var reader: BufferedReader = _

  def main(args: Array[String]): Unit = {
    writeSolution(solve(processInput(System.in)))
  }

  def readHeader(): (Int, String) = {
    val firstLine = reader.readLine()
    val splittedLine = firstLine.split(" ")
    val n = splittedLine(0).toInt
    val source = splittedLine(1)
    (n, source)
  }

  def readAreasWithCities(n: Int): Map[String, List[String]] = {
    var cityToCitiesInArea = Map[String, List[String]]()
    for (i <- 1 to n) {
      val areaName = reader.readLine()
      val airports = reader.readLine().split(" ").toList
      for (airport <- airports) cityToCitiesInArea = cityToCitiesInArea + (airport -> airports)
    }
    cityToCitiesInArea
  }

  def readFlights(totalDays: Int): List[Flight] = {
    var flightDefinitions = List[Flight]()
    var line = ""
    while ( {
      line = reader.readLine()
      line != null && line != ""
    }) {
      val flightLine = line.split(" ")
      val from = flightLine(0)
      val to = flightLine(1)
      val price = flightLine(3).toInt
      val day = flightLine(2).toInt
      if (day == 0) {
        val flightForEveryDay = for (day <- List.range(1, totalDays + 1)) yield Flight(from, to, day, price)
        flightDefinitions = flightForEveryDay ::: flightDefinitions
      } else {
        val flight = Flight(from, to, day, price)
        flightDefinitions = flight :: flightDefinitions
      }
    }
    flightDefinitions
  }
  
  def processInput(inputStream: InputStream): Problem = {
    reader = new BufferedReader(new InputStreamReader(inputStream))
    val (numberOfAreas, sourceAirport) = readHeader()
    val sameAreaAirportsByAirport = readAreasWithCities(numberOfAreas)
    val flightDefinitions = readFlights(numberOfAreas)
    val flightsPerDayAndAirport = flightDefinitions
      .groupBy(flight => (flight.day, flight.from))
      .map(element => element._1 -> element._2.sortBy(_.price))
      .withDefaultValue(Nil)
      .asInstanceOf[FlightMap]
    Problem(numberOfAreas, sourceAirport, sameAreaAirportsByAirport, flightsPerDayAndAirport)
  }
  
  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.price))
    solution.foreach(flight => println(flight.from, flight.to, flight.price, flight.day))
  }
  
  def solve(problem: Problem): List[Flight] = {
    nearestNeighborWrapper(problem.start, problem.areaCount, problem.areas, problem.flights)
  }

  def nearestNeighborWrapper(startAirport: String,
                             numberOfAreas: Int,
                             sameAreaAirportsByAirport: Map[String, Seq[String]],
                             flightsPerDayAndAirport: Map[(Int, String), Seq[Flight]]
                            ): List[Flight] = {
    def nearestNeighbor(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[Flight] = {
      val lastDay = day == numberOfAreas
      val destinationAirportsFilter: Flight => Boolean = if (lastDay) {
        flight: Flight => flight.to == startAirport
      } else {
        flight: Flight => !visitedAirports.contains(flight.to)
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
          .map(flight => nearestNeighbor(day + 1, airportsVisitedAfterThisFlight, flight.to, flight :: path))
          .find(_.nonEmpty)
          .getOrElse(Nil)
        
      }
    }

    nearestNeighbor(1, Set(), startAirport, List()).reverse
  }

}
