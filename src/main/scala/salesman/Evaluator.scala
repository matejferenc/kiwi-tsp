package salesman

import java.io.FileInputStream

object Evaluator {

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 4) {
      evaluateTestCase(i)
    }
  }

  def evaluateTestCase(number: Int): Unit = {
//    println(s"evaluating dataset $number")
    val inputStream = new FileInputStream(getClass.getResource(s"/$number.in").getPath)
    System.setIn(inputStream)
    val start = System.currentTimeMillis
    Salesman.processInput()
    val readingPhase = System.currentTimeMillis
    println("reading phase: " + (readingPhase - start) + " ms")
    println()
    Salesman.solve()
    val calculationPhase = System.currentTimeMillis
    println("calculation phase: " + (calculationPhase - readingPhase) + " ms")
    println()
    println("total time: " + (calculationPhase - start) + " ms")
//    inputStream.close()
  }
}
