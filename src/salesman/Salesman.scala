package salesman

import java.io.FileInputStream

import scala.io.StdIn

object Salesman {

  case class Flight(source: String, destination: String, day: Int, cost: Int)

  def main(args: Array[String]): Unit = {
//    val a = getClass.getResource("/2.in.txt").getPath
//    System.setIn(new FileInputStream(a))
    val start = System.currentTimeMillis

    val (numberOfAreas, source) = readHeader()

    val sameAreaAirportsByAirport: Map[String, Seq[String]] = readAreasWithCities(numberOfAreas)

    val flightDefinitions: List[Flight] = readFlights(numberOfAreas)

    val flightsPerDayAndAirport: Map[(Int, String), List[Flight]] = flightDefinitions
      .groupBy(flight => (flight.day, flight.source))
      .map(element => element._1 -> element._2.sortBy(_.cost))

    val readingPhase = System.currentTimeMillis
    println("reading phase: " + (readingPhase - start) + " ms")
    println(numberOfAreas)
    println(sameAreaAirportsByAirport.values.flatten.toSet.size)
    println(flightDefinitions.size)
    println()
    
    val solution = nearestNeighborWrapper(source, numberOfAreas, sameAreaAirportsByAirport, flightsPerDayAndAirport)
    println(solution.foldLeft(0)((subTotalCost, flight) => subTotalCost + flight.cost))
    solution.foreach(flight => println(flight.source, flight.destination, flight.cost, flight.day))
    val calculationPhase = System.currentTimeMillis
    println("calculation phase: " + (calculationPhase - readingPhase) + " ms")
    println()
    println("total time: " + (calculationPhase - start) + " ms")
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
    val firstLine = StdIn.readLine().split(" ")
    val n = firstLine(0).toInt
    val source = firstLine(1)
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
