package analyzer

import java.io.{FileInputStream, InputStream}

import analyzer.Flights.FlightMap

import scala.collection.mutable

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

object ProblemAnalyzer {

  def detectDuplicates(problem: Problem): Unit = {
    val set = mutable.Set[(Int, String, String)]()
    var duplicates = 0
    for {
      (from, flights) <- problem.flights
      flight <- flights
    } {
      val key = (from._1, from._2, flight.to)
      if (set.contains(key)) {
        //println(s"Detected duplicate flight $from -> ${flight.to}")
        duplicates += 1
      } else set.add(key)
    }
    println(s"Total flights: ${set.size + duplicates}, duplicates: ${duplicates}")
  }

  def cheapestFlights(problem: Problem): Unit = {

    val flights = for {
      (from, flights) <- problem.flights
      flight <- flights
    } yield (from._2, flight.to, flight.price)

    val prices = flights
      .groupBy { case (from, to, price) => (from, to) }

    val stats = prices
      .mapValues(_.map(_._3))
      .mapValues(prices => (prices.max, prices.sum / prices.size, prices.min))
      .toSeq
      .sortBy(_._2._2)

    for (((from, to), (max, avg, min)) <- stats) {
      println(s"Flights: $from -> $to, max: $max, avg: $avg, min: $min")
    }
  }


  def analyze(problem: Problem) = {
    detectDuplicates(problem)
    cheapestFlights(problem)
  }

  implicit class RichSeq[A](seq:Seq[A]) {
    def peek(f: A => Unit): Seq[A] = seq.map { a => f(a); a }
  }

  def main(args: Array[String]) = {
    (3 to 3).toStream
      .peek(i => println(s"Analyzing problem $i"))
      .map(n => new FileInputStream(getClass.getResource(s"/$n.in").getPath))
      .map(Problem.fromInputStream)
      .foreach(analyze)
  }

}
