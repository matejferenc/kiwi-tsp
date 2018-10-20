package genes2

import java.io.InputStream
import java.util.concurrent.TimeoutException

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

    def findSolution(day: Int, visitedAirports: Set[String], currentAirport: String, path: List[Flight], forcedMap: Map[Int, String]): List[Flight] = {
      counter += 1
      if (counter > MaxCounter) {
        skipped += 1
        throw new TimeoutException
      } else if (problem.finalDay(day)) path
      else {
        val newVisitedAirports = visitedAirports ++ problem.areas(currentAirport)

        val outboundFlights = problem.flights(Flights.key(day, currentAirport))

        def forcedFlight = for {
          airport <- forcedMap.get(day)
          flight <- outboundFlights.find(_.to == airport)
        } yield flight

        def nearestFlights = outboundFlights
          .filter(eligibleFlight(day, newVisitedAirports))
          .toStream

        (forcedFlight.toStream ++ nearestFlights)
          .map(flight => findSolution(day + 1, newVisitedAirports, flight.to, flight :: path, forcedMap))
          .find(_.nonEmpty)
          .getOrElse(Nil)
      }
    }

    val r = new Random(problem.hashCode)

    val GenerationSize = 200
    val GenerationWinners = 15
    val MutationChance = 0.05

    var allWinners = ArrayBuffer[List[Flight]]()

    val areas: Seq[Seq[String]] = problem.areas.values.toSet.toSeq
    val areaNumbers: Seq[Int] = (2 until areas.size)

    def newGenome: Map[Int, String] = {
      val size = (problem.areaCount / 100) + 1
      val airports = r.shuffle(areas).filter(_.toSet != problem.startArea).take(size).map(area => area(r.nextInt(area.size)))
      val days = r.shuffle(areaNumbers).take(size)
      days.zip(airports).toMap
    }

    def breed(mate1: Map[Int, String], mate2: Map[Int, String]): Map[Int, String] = {
      val split = r.nextInt(problem.areaCount)
      val airports = mutable.Set[String]()
      val merged = mutable.Map[Int, String]()

      for ((k, v) <- mate1) if (k <= split) {
        airports ++= problem.areas(v)
        merged.put(k, v)
      }

      for ((k, v) <- mate1) if (k > split && !airports.contains(v)) {
        airports ++= problem.areas(v)
        merged.put(k, v)
      }

      merged.toMap
    }


    def initialGeneration(size: Int): Seq[Map[Int, String]] = (1 to size).map(_ => newGenome)

    var bestSolution = List[Flight]()
    var bestPrice = Int.MaxValue

    var generation = initialGeneration(GenerationSize)

    while (!timeout.shouldTerminate()) {

      def evaluate(genome: Map[Int, String]) = {
        counter = 0
        try findSolution(1, genome.values.toSet, problem.start, Nil, genome)
        catch {
          case _: TimeoutException => Nil
        }
      }

      val results = generation
        .map(m => (m, evaluate(m)))
        .filter(_._2.nonEmpty)
        .sortBy(_._2.price)

      results.headOption.foreach { case (winner, solution) =>
        if (solution.price < bestPrice) {
          bestSolution = solution
          bestPrice = bestSolution.price
        }
      }

      def pickMate = results((math.abs(r.nextGaussian()) * 10 / problem.areaCount).toInt)._1

      generation = (1 to GenerationSize / 2).map(_ => breed(pickMate, pickMate)) ++ initialGeneration(GenerationSize / 2)
    }

    println(bestSolution.reverse)
    bestSolution.reverse
  }

  def writeSolution(solution: List[Flight]): Unit = {
    println(solution.price)
    solution.foreach(flight => println(flight.toOutputString))
  }

  writeSolution(solve(processInput(System.in)))
}
