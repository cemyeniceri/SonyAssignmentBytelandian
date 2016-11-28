import scala.annotation.tailrec

/**
  * Created by cemyeniceri on 27/11/16.
  */
object InputParser {

  // Each roads have been connected
  def createCityStructureWithCrossCheck(input: (Int, List[String])) : Map[String, Set[String]] = {

    // Initial map are created with input size, which will be updated by helper method
    val initialMap :Map[String, Set[String]] = (0 until input._1).map(i=> (i.toString -> Set[String]())).toMap
    createCityStructureWithCrossCheckRec(1, initialMap, input._2)
  }

  @tailrec
  def createCityStructureWithCrossCheckRec(index : Int, accMap: Map[String, Set[String]], roads: List[String]) : Map[String, Set[String]] = {
    if(roads.isEmpty)
      accMap
    else{
      // E.g. (0---1) update map element with key 0, by adding connection to set
      val existentElem = accMap.find(_._1 == roads.head).get
      val newMap = accMap.filterNot(_ == existentElem) + (existentElem._1 -> (existentElem._2 ++ Set(index.toString)))

      // E.g. (0---1) update map element with key 1, by adding connection to set
      val indexedExistentElem = newMap.find(_._1 == index.toString).get
      val finalMap = newMap.filterNot(_ == indexedExistentElem) + (index.toString -> (indexedExistentElem._2 ++ Set(roads.head)))

      // call helper method with updated map, remaining roads and the next index
      createCityStructureWithCrossCheckRec(index + 1, finalMap, roads.tail)
    }
  }

}
