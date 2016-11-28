import org.scalatest._

/**
  * Created by cemyeniceri on 28/11/16.
  */
class InputParserSpec extends FunSuite{

  test("createCityStructureWithCrossCheck successfull test") {

    val input1 = Map("0" -> Set("1"), "1" -> Set("0","2"), "2" -> Set("1","3"), "3" -> Set("2"))
    val input2 = Map("0" -> Set("1","4", "5"), "1" -> Set("0","2"), "2" -> Set("1","3"), "3" -> Set("7","6","2"), "4" -> Set("0"), "5" -> Set("0"), "6" -> Set("3"), "7" -> Set("3"))
    val input3 = Map("0" -> Set("1","6"), "1" -> Set("0","2", "3", "4", "5"), "2" -> Set("1","7","8"), "3" -> Set("1"), "4" -> Set("1"), "5" -> Set("1"), "6" -> Set("0"), "7" -> Set("2"), "8" -> Set("2"))

    assert(InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2")) === input1)
    assert(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "2", "0", "0", "3", "3")) === input2)
    assert(InputParser.createCityStructureWithCrossCheck(9, List("0", "1", "1", "1", "1", "0", "2", "2")) === input3)

    info("createCityStructure seems to work")
  }

  test("createCityStructure failure test") {

    val input1 = Map("0" -> Set("1"), "1" -> Set("0"), "2" -> Set("1","3"), "3" -> Set("2"))
    val input2 = Map("0" -> Set("1","4", "5"), "1" -> Set("0"), "2" -> Set("1","3"), "3" -> Set("7","6","2"), "4" -> Set("0"), "5" -> Set("0"), "6" -> Set("3"), "7" -> Set("3"))
    val input3 = Map("0" -> Set("1","6"), "1" -> Set("0","2", "4", "5"), "2" -> Set("1","7","8"), "3" -> Set("1"), "4" -> Set("1"), "5" -> Set("1"), "6" -> Set("0"), "7" -> Set("2"), "8" -> Set("2"))

    assert(InputParser.createCityStructureWithCrossCheck(4, List("0", "1", "2")) !== input1)
    assert(InputParser.createCityStructureWithCrossCheck(8, List("0", "1", "2", "0", "0", "3", "3")) !== input2)
    assert(InputParser.createCityStructureWithCrossCheck(9, List("0", "1", "1", "1", "1", "0", "2", "2")) !== input3)

    info("createCityStructure seems to work")
  }

}