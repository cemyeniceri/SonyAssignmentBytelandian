import scala.annotation.tailrec

/**
  * Created by cemyeniceri on 28/11/16.
  */
object Solver {

  //
  def evaluateStepCount(cityGraph: Map[String, Set[String]]): Int = {
    evaluateStepCountRec(0, cityGraph)
  }

  @tailrec
  def evaluateStepCountRec(numberOfSteps: Int, cityGraph: Map[String, Set[String]]): Int = {
    if(cityGraph.size == 1)
      numberOfSteps
    else{

      val connectedCities = createConnectedCities(cityGraph)
      val newCityGraph = connectCitiesAndUpdateMap(cityGraph, connectedCities)
      evaluateStepCountRec(numberOfSteps + 1, newCityGraph)
    }
  }


  def createConnectedCities(cityGraph: Map[String, Set[String]]): List[Pair] = {
    createConnectedCitiesRec(cityGraph, cityGraph, List())
  }

  //consCityGraph inputundan daolyı tailRec değil
  def createConnectedCitiesRec(consCityGraph: Map[String, Set[String]], cityGraph: Map[String, Set[String]], acc: List[Pair]): List[Pair] = {
    if(cityGraph.isEmpty)
      acc
    else{
      val (key, values) = cityGraph.head

      // already in use check
      if(acc.exists(x=> x.left == key || x.right == key))
        createConnectedCitiesRec(consCityGraph, cityGraph.tail, acc)
      else{
        val theBestCityToConnect = getSmallestConnection(consCityGraph, values)
        val newAcc = if(!acc.exists(x=> x.left == theBestCityToConnect || x.right == theBestCityToConnect)){
          acc :+ Pair(key, theBestCityToConnect)
        }else{
          acc
        }
        createConnectedCitiesRec(consCityGraph, cityGraph.tail, newAcc)
      }
    }
  }

  def getSmallestConnection(cityGraph: Map[String, Set[String]], values: Set[String]): String = {

    val t = values.map{x=>
      val size = cityGraph(x).size
      (x, size)
    }
    t.minBy(_._2)._1
  }

  def connectCitiesAndUpdateMap(cityGraph: Map[String, Set[String]], pair: List[Pair]) : Map[String, Set[String]] = {
    connectCitiesAndUpdateMapRec(cityGraph, pair)
  }

  @tailrec
  def connectCitiesAndUpdateMapRec(cityGraph: Map[String, Set[String]], pair: List[Pair]) : Map[String, Set[String]] = {
    if(pair.isEmpty)
      cityGraph
    else{
      val leftMapEntry = cityGraph.find(_._1 == pair.head.left).get
      val leftSet = leftMapEntry._2
      val newLeftSet = leftSet.filterNot(_ == pair.head.right)

      val rightMapEntry = cityGraph.find(_._1 == pair.head.right).get
      val rightSet = rightMapEntry._2
      val newRightSet = rightSet.filterNot(_ == pair.head.left)

      val finalLeftSet = newLeftSet ++ newRightSet
      val newLeftMapEntry = (leftMapEntry._1 -> finalLeftSet)

      val modifiedCityGraph = cityGraph.filterNot(_ == leftMapEntry).filterNot(_ == rightMapEntry) + (newLeftMapEntry)

      val mergedMap = mergeConnecitons(modifiedCityGraph, pair.head)

      connectCitiesAndUpdateMapRec(mergedMap, pair.tail)
    }
  }

  def mergeConnecitons(cityGraph: Map[String, Set[String]], pair: Pair) : Map[String, Set[String]] = {
    mergeConnecitonsRec(cityGraph, cityGraph, pair)
  }

  @tailrec
  def mergeConnecitonsRec(input: Map[String, Set[String]], cityGraph: Map[String, Set[String]], pair: Pair) : Map[String, Set[String]] = {
    if(input.isEmpty)
      cityGraph
    else{
      val headEntry = input.head
      val newCityGraph = if(headEntry._2.exists(_ == pair.right)){
        val newValues = headEntry._2.filterNot(_ == pair.right) + (pair.left)
        cityGraph.filterNot(_._2 == headEntry._2) + (headEntry._1 -> newValues)
      }else{
        cityGraph
      }

      mergeConnecitonsRec(input.tail, newCityGraph, pair)
    }
  }
}
