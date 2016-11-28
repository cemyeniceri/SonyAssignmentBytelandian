import org.scalatest._

/**
  * Created by cemyeniceri on 28/11/16.
  */
class SolverSpec extends FunSuite{

  test("evaluateStepCount with valid inputs") {

    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2"))) === 2)
    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "2", "0", "0", "3", "3"))) === 4)
    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(9, List("0", "1", "1", "1", "1", "0", "2", "2"))) === 5)

    info("evaluateStepCount seems to work ")
  }

  test("evaluateStepCount with invalid inputs") {

    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2"))) !== 5)
    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "2", "0", "0", "3", "3"))) !== 3)
    assert(Solver.evaluateStepCount(InputParser.createCityStructureWithCrossCheck(9, List("0", "1", "1", "1", "1", "0", "2", "2"))) !== 6)

    info("evaluateStepCount seems to work ")
  }

  test("createConnectedCities with valid inputs") {

    // Test them for initial mapStates
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2"))) === List(Pair("0", "1"), Pair("2", "3")))
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "2", "0", "0", "3", "3"))) === List(Pair("4", "0"), Pair("6", "3"), Pair("1","2")))
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(9, List("0", "1", "1", "1", "1", "0", "2", "2"))) === List(Pair("8", "2"), Pair("4", "1"), Pair("6","0")))

    info("createConnectedCities seems to work ")
  }

  test("createConnectedCities with invalid inputs") {

    // Test them for initial mapStates
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(3, List("0", "2"))) !== List(Pair("0", "1"), Pair("2", "3")))
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(7, List("0", "1", "2", "0", "3", "3"))) !== List(Pair("4", "0"), Pair("6", "3"), Pair("1","2")))
    assert(Solver.createConnectedCities(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "5", "1", "1", "0", "2"))) !== List(Pair("8", "2"), Pair("4", "1"), Pair("6","0")))

    info("createConnectedCities seems to work ")
  }

  test("getSmallestConnection with valid inputs") {


    val cityGraph = InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2"))

    // Test them for initial mapStates
    assert(Solver.getSmallestConnection(cityGraph, Set("1","3")) === "3")
    info("getSmallestConnection seems to work ")
  }

  test("connectCitiesAndUpdateMap with valid inputs") {


    val cityGraph = InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2"))

    val connectedCities = Solver.createConnectedCities(cityGraph)
    assert(Solver.connectCitiesAndUpdateMap(cityGraph, connectedCities) === Map("0" -> Set("2"), "2" -> Set("0")))
    info("connectCitiesAndUpdateMap seems to work ")
  }

  test("mergeConnecitons with valid inputs") {

    val cityGraph = Map("2" -> Set("1", "3"), "3"-> Set("2"), "0"-> Set("2"))

    val mergedConnections = Solver.mergeConnecitons(cityGraph, Pair("0", "1"))
    assert(mergedConnections === Map("3" -> Set("2"), "0" -> Set("2"), "2" -> Set("3", "0")))
    info("mergeConnecitons seems to work ")
  }


}