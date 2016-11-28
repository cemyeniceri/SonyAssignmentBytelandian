import scala.annotation.tailrec

/**
  * Created by cemyeniceri on 28/11/16.
  */
object Solver {

  // evaluates result for generated cityGraph
  def evaluateStepCount(cityGraph: Map[String, Set[String]]): Int = {
    evaluateStepCountRec(0, cityGraph)
  }

  @tailrec
  def evaluateStepCountRec(numberOfSteps: Int, cityGraph: Map[String, Set[String]]): Int = {
    if(cityGraph.size == 1)
      numberOfSteps
    else{

      // finds to be connected city pairs
      val connectedCities = createConnectedCities(cityGraph)

      // combine pairs and update cityGraph
      val newCityGraph = connectCitiesAndUpdateMap(cityGraph, connectedCities)

      //increment numberOfStep and call helper method with updated cityGraph
      evaluateStepCountRec(numberOfSteps + 1, newCityGraph)
    }
  }


  def createConnectedCities(cityGraph: Map[String, Set[String]]): List[Pair] = {
    createConnectedCitiesRec(cityGraph, cityGraph, List())
  }

  // not tailrec because of consCityGraph input
  def createConnectedCitiesRec(consCityGraph: Map[String, Set[String]], cityGraph: Map[String, Set[String]], acc: List[Pair]): List[Pair] = {
    if(cityGraph.isEmpty)
      acc
    else{
      val (key, values) = cityGraph.head

      // already in use check
      // if a city has been used already, dont estimate again with another city
      if(acc.exists(x=> x.left == key || x.right == key))
        createConnectedCitiesRec(consCityGraph, cityGraph.tail, acc)
      else{

        // find deepest city number
        val theBestCityToConnect = getSmallestConnection(consCityGraph, values)

        // check again, either used with another city
        val newAcc = if(!acc.exists(x=> x.left == theBestCityToConnect || x.right == theBestCityToConnect)){
          acc :+ Pair(key, theBestCityToConnect)
        }else{
          acc
        }

        createConnectedCitiesRec(consCityGraph, cityGraph.tail, newAcc)
      }
    }
  }

  // obtain city number which has smallest connections set
  def getSmallestConnection(cityGraph: Map[String, Set[String]], values: Set[String]): String = {

    val t = values.map{x=>
      val size = cityGraph(x).size
      (x, size)
    }
    t.minBy(_._2)._1
  }


  // combine cities and update cityGraph due to combination results
  def connectCitiesAndUpdateMap(cityGraph: Map[String, Set[String]], pair: List[Pair]) : Map[String, Set[String]] = {
    connectCitiesAndUpdateMapRec(cityGraph, pair)
  }

  @tailrec
  def connectCitiesAndUpdateMapRec(cityGraph: Map[String, Set[String]], pair: List[Pair]) : Map[String, Set[String]] = {
    if(pair.isEmpty)
      cityGraph
    else{

      // find left element of pair and remove right connection from connection set
      val leftMapEntry = cityGraph.find(_._1 == pair.head.left).get
      val leftSet = leftMapEntry._2
      val newLeftSet = leftSet.filterNot(_ == pair.head.right)

      // find right element of pair and remove left connection from connection set
      val rightMapEntry = cityGraph.find(_._1 == pair.head.right).get
      val rightSet = rightMapEntry._2
      val newRightSet = rightSet.filterNot(_ == pair.head.left)

      // right element of pair will be removed anymore, so update set of element with right's set
      val finalLeftSet = newLeftSet ++ newRightSet
      val newLeftMapEntry = (leftMapEntry._1 -> finalLeftSet)


      // remove right element and existent left element from cityGraph, then add new left element
      val modifiedCityGraph = cityGraph.filterNot(_ == leftMapEntry).filterNot(_ == rightMapEntry) + (newLeftMapEntry)


      // merge connections, if some of cityGraph elements have right element
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
