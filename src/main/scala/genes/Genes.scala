package genes

import java.io.InputStream

import Flights.FlightMap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.Random

//Submission details #6737
//ID	Result	Score	Time	Memory
//#1	accepted	93.5	2.85s	324096KB
//#6	accepted	115.89	4.86s	323776KB
//#11	accepted	104.98	14.84s	323776KB
//#14	accepted	104.93	14.86s	323520KB

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
  type FlightMap = Map[(Int, String), Seq[Flight]]
  def key(day: Int, from: String) = (day, from)
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

  class Timeout(startTs: Long, airports: Int, areas: Int) {
    val Gap = 300

    val timeoutMs = (if (areas <= 20 && airports < 50) 3000
      else if (areas <= 100 && areas < 100) 5000
      else 15000) - Gap

    def shouldTerminate() = (System.currentTimeMillis() - startTs) >= timeoutMs
  }

  class TimeoutException extends Exception

  implicit class RichPath(path: List[Flight]) {
    def price = path.map(_.price).sum
  }

  var startTs: Long = _

  def processInput(in: InputStream): Problem = {
    startTs = System.currentTimeMillis()
    Problem.fromInputStream(in)
  }

  def solve(problem: Problem): List[Flight] = {

    def eligibleFlight(day: Int, visitedAirports: Set[String])(flight: Flight): Boolean = {
      val flightBackHome = problem.lastFlight(day) && problem.startArea.contains(flight.to)
      val notYetVisited = !visitedAirports.contains(flight.to)
      flightBackHome || notYetVisited
    }

    val timeout = new Timeout(startTs, problem.areas.keys.size, problem.areaCount)

    var counter = 0
    var skipped = 0
    val MaxCounter = problem.areas.size * 2

    def findSolution(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight], skipMap: Map[String, Int]): List[Flight] = {
      counter += 1
      if (counter > MaxCounter) {
        skipped += 1
        throw new TimeoutException
      } else if (problem.finalDay(day)) path
      else {
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        val outboundFlights = problem.flights(Flights.key(day, currentAirport))
        outboundFlights
          .filter(eligibleFlight(day, newVisitedAirports))
          .drop(skipMap(currentAirport))
          .toStream
          .map(flight => findSolution(day + 1, newVisitedAirports, flight.to, flight :: path, skipMap))
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    val r = new Random(problem.hashCode)

    val GenerationSize = 200
    val GenerationWinners = 15
    val MutationChance = 0.05

    def generateSkip = {
      val n = r.nextInt(500)
      if (n == 0) 2
      else if (n < 10) 1
      else 0
    }

    def shuffle(a: Map[String, Int], b: Map[String, Int]): Map[String, Int] = {
      def merge(vA: Int, vB: Int) =
        if (r.nextDouble < MutationChance) generateSkip
        else if (r.nextBoolean()) vA else vB

      for ((key, valueA) <- a)
        yield (key, merge(valueA, b(key)))
    }

    var generation = for (i <- 0 to GenerationSize) yield {
      problem.areas.keys
        .map(k => (k, generateSkip))
        .toMap
    } ++ Map().withDefaultValue(0)

    var allWinners = ArrayBuffer[List[Flight]]()

    try {
      while (!timeout.shouldTerminate()) {

        val solutions = generation.map { instance =>
          if (timeout.shouldTerminate()) throw new TimeoutException
          counter = 0
          val solution = try {
            findSolution(1, Set(), problem.start, Nil, instance).reverse
          } catch {
            case e: TimeoutException => Nil
          }
          (instance, solution)
        }.filter(_._2.nonEmpty)

        val winners = solutions.sortBy(_._2.price).take(GenerationWinners)
        val winnerInstances = winners.map(_._1)

        allWinners.append(winners.map(_._2): _*)

        val shuffles = for {
          a <- winnerInstances
          b <- winnerInstances
          if (a != b)
        } yield shuffle(a, b)

        generation = shuffles
      }
    } catch {
      case e: TimeoutException => Nil
    }

    //println(s"$skipped paths skipped")

    allWinners.sortBy(_.price).head
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.price)
    solution.foreach(flight => println(flight.toOutputString))
  }

  writeSolution(solve(processInput(System.in)))
}
