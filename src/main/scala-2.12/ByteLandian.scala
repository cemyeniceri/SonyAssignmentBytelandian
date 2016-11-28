/**
  * Created by cemyeniceri on 27/11/16.
  */
object ByteLandian extends App{

  val rawInput = readUserInput()

  rawInput.map { t =>

    val cityGraph = InputParser.createCityStructure(t)
    val numberOfSteps = Solver.evaluateStepCount(cityGraph)

    println(s"Number of Steps : $numberOfSteps")
  }

  def readUserInput(): List[(Int, List[String])] = {

    println("Please enter the number of test cases:")
    val numberOfTestCases = scala.io.StdIn.readInt

    (0 until numberOfTestCases).map{ x=>

      println("Please enter the number of cities:")
      val numberOfCities = scala.io.StdIn.readInt
      require(2 <= numberOfCities && numberOfCities <= 600, "number of cities should be between [2, 600]")

      println(s"Please enter the route connections")
      val roads = scala.io.StdIn.readLine().split(' ').toList
      require(roads.size == numberOfCities - 1, "number of routes should be 1 less than number of cities")
      require(roads.forall((0 until numberOfCities).map(_.toString).contains), "routes should be between the cities")

      (numberOfCities, roads)
    }
  }.toList

}
