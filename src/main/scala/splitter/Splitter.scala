package splitter

import java.io.InputStream

import Flights.FlightMap

import scala.collection.mutable

// Tries to solve the problem by separating it into chunks of smaller problems and combine solutions together.
// Does not work because of dead-end airports (like ZYL in 2.in).

case class Problem(areaCount: Int, start: String, areas: mutable.Map[String, Seq[String]], flights: FlightMap) {
  lazy val startArea = areas(start).toSet
  def lastFlight(day: Int) = day == areaCount
  def finalDay(day: Int) = day > areaCount
}

case class Flight(from: String, to: String, day: Int, price: Int) {
  def key = (day, from)
  def toOutputString = List(from, to, day, price).mkString(" ")
}

object Flights {
  type FlightMap = Map[(Int, String), Seq[Flight]]
  def fromList(flights: Seq[Flight]): FlightMap = {
    flights
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
  def processInput(in: InputStream): Problem = Problem.fromInputStream(in)

  // class for debugging collection operations
  implicit class RichSeq[T](seq: Seq[T]) {
    def peek(f: T => Unit) = seq.map { x => f(x); x }
  }

  implicit class RichPath(path: List[Flight]) {
    def price = path.map(_.price).sum
  }

  implicit class RichList[A](list: List[A]) {
    def minByOrElse[B](f: A => B, default : => A)(implicit cmp: scala.Ordering[B]): A = list match {
      case Nil => default
      case x => x.minBy(f)
    }
  }

  def solve(problem: Problem): List[Flight] = {

    // solution to problem will take areaCount days
    // 1. day => start - nekam1
    // 2. day => nekam1 - nekam2
    // 3. day => nekam2 - start
    // 4. day => the end (stopday)

    val BranchingFactor = 2

    def findPaths(day: Int, stopDay: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight]): List[List[Flight]] = {
      println(s"findPaths ${path.reverse} PRICE ${path.price}")
      if (day == stopDay) {
        println(s"findPaths END")
        List(path)
      }
      else {
        val newDay = day + 1
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        def eligibleFlight(day: Int, visitedAirports: Set[String])(flight: Flight): Boolean = {
          val flightBackHome = problem.lastFlight(day) && problem.startArea.contains(flight.to)
          val notYetVisited = !visitedAirports.contains(flight.to)
          flightBackHome || notYetVisited
        }

        problem.flights(day, currentAirport)
          .sortBy(_.price)
          .toStream
          .filter(eligibleFlight(day, newVisitedAirports))
          .map(flight => findPaths(newDay, stopDay, newVisitedAirports, flight.to, flight :: path).minByOrElse(_.price, Nil))
          .filter(_.nonEmpty)
          .take(BranchingFactor)
          .toList
      }
    }


    val Step = 3
    val ranges = (1 to problem.areaCount by Step).map(n => (n, List(n + Step, problem.areaCount + 1).min))
    println(ranges)

    val result = ranges.foldLeft(List[Flight]()) { case (result, (dayFrom, dayTo)) =>
      val visitedAirports = result.flatMap(f => List(f.from, f.to)).flatMap(problem.areas).toSet
      val start = result.headOption.map(_.to).getOrElse(problem.start)

      println(s"Chunk $dayFrom $dayTo")
      println(result.reverse)
      println(visitedAirports)
      println(start)
      findPaths(dayFrom, dayTo, visitedAirports, start, result).minBy(_.price)
    }
    println("Solution len: " + result.size)
    println("Solution: " + result.reverse)
    result.reverse
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.map(_.price).sum)
    solution.foreach(flight => println(flight.toOutputString))
  }

  writeSolution(solve(processInput(System.in)))
}
