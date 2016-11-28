import scala.annotation.tailrec

/**
  * Created by cemyeniceri on 27/11/16.
  */
object InputParser {

  def createCityStructureWithCrossCheck(input: (Int, List[String])) : Map[String, Set[String]] = {

    val initialMap :Map[String, Set[String]] = (0 until input._1).map(i=> (i.toString -> Set[String]())).toMap
    createCityStructureWithCrossCheckRec(1, initialMap, input._2)
  }

  @tailrec
  def createCityStructureWithCrossCheckRec(index : Int, accMap: Map[String, Set[String]], roads: List[String]) : Map[String, Set[String]] = {
    if(roads.isEmpty)
      accMap
    else{
      val existentElem = accMap.find(_._1 == roads.head).get
      val newMap = accMap.filterNot(_ == existentElem) + (existentElem._1 -> (existentElem._2 ++ Set(index.toString)))
      val indexedExistentElem = newMap.find(_._1 == index.toString).get
      val finalMap = newMap.filterNot(_ == indexedExistentElem) + (index.toString -> (indexedExistentElem._2 ++ Set(roads.head)))
      createCityStructureWithCrossCheckRec(index + 1, finalMap, roads.tail)
    }
  }

}
