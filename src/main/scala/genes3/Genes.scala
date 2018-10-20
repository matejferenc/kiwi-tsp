package genes3

import java.io.InputStream

import Flights.FlightMap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.Random

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
    def price = if (path.isEmpty) Int.MaxValue else path.map(_.price).sum
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
        var drop = skipMap(currentAirport)

        var i = 0
        var solution: List[Flight] = Nil
        while (i < outboundFlights.size && solution.isEmpty) {
          val flight = outboundFlights(i)
          if (eligibleFlight(day, newVisitedAirports)(flight)) {
            if (drop > 0) drop -= 1
            else solution = findSolution(day + 1, newVisitedAirports, flight.to, flight :: path, skipMap)
          }
          i += 1
        }

        solution
      }


//
//        val outboundFlights = problem.flights(Flights.key(day, currentAirport))
//        outboundFlights
//          .filter(eligibleFlight(day, newVisitedAirports))
//          .drop(skipMap(currentAirport))
//          .toStream
//          .map(flight => findSolution(day + 1, newVisitedAirports, flight.to, flight :: path, skipMap))
//          .find(_.nonEmpty)
//          .getOrElse(Nil)
//      }
    }

    val r = new Random(/*problem.hashCode*/)

    val GenerationSize = problem.areaCount / 5 + 10 // problem.areaCount / 2
    val WinnerSelectionCoefficient = GenerationSize.toDouble / 25 // 2
    val MutationChance = 0.1
    val RestartThreshold = 3

    def generateSkip = {
//      math.abs(r.nextGaussian() / 2).toInt
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

    def initialGeneration = for (i <- 0 to GenerationSize) yield {
      problem.areas.keys
        .map(k => (k, generateSkip))
        .toMap
    } ++ Map().withDefaultValue(0)

    var generation = initialGeneration

    var winner = (Map[String, Int]().withDefaultValue(0), List[Flight]())

    var noImprovementCounter = 0

    var generationNumber = 1

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

        val winners = solutions.sortBy(_._2.price)

        if (winners.size <= 2 || noImprovementCounter == RestartThreshold) {
          println("restarting...")
          generation = (1 to 1).map(_ => winner._1) ++ initialGeneration
          noImprovementCounter = 0
        } else {

          if (winner._2.price > winners.head._2.price) {
            winner = winners.head
            noImprovementCounter = 0
          } else noImprovementCounter += 1

          val winnerInstances = winners.map(_._1) :+ winner._1

          def pickMate = math.min((math.abs(r.nextGaussian()) * WinnerSelectionCoefficient).toInt, winnerInstances.size - 1)
          def pickWinnerPair = {
            val mate1 = pickMate
            val mate2 = {
              val tmp = pickMate
              if (mate1 == tmp) math.min(tmp + 1, winnerInstances.size - 1) else tmp
            }
            (winnerInstances(mate1), winnerInstances(mate2))
          }

          generation = (1 to GenerationSize)
            .map(_ => pickWinnerPair)
            .map((shuffle _).tupled)

        }

        println(s"Generation #$generationNumber: ${winner._2.price}")
        generationNumber += 1
      }
    } catch {
      case e: TimeoutException => Nil
    }

    winner._2
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.price)
    solution.foreach(flight => println(flight.toOutputString))
  }

  writeSolution(solve(processInput(System.in)))
}
