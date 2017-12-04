import scala.io.Source

class Field(fileName: String, dim: Int, controlSum: Int) {

  def findSuitableCombinations(): Unit = checkRightFourSquares()

  private val squares: Array[Square] = {
    val res = Array.fill(dim)(Square(0, 0, 0, 0))
    var i = 0
    for (line <- Source.fromFile(fileName).getLines) {
      val splits = line.split(" ").map((p) => p.toInt).toArray
      res(i) = new Square(splits)
      i += 1
    }
    res
  }

  private val permutationIndexes: List[Int] = 0 until dim toList

  private def checkCross(indexes: List[Int], i1: Int, i2: Int, i3: Int, i4: Int): Boolean = {
    squares(indexes(i1)).nLU + squares(indexes(i2)).nRU + squares(indexes(i3)).nLD + squares(indexes(i4)).nRD == controlSum
  }

  private def checkRightFourSquares(): Unit = {
    permutationIndexes.combinations(4).foreach((k) => {
      k.permutations.foreach((p) => if (checkCross(p, 3, 2, 1, 0) && squares(p(1)).nRD + squares(p(3)).nRU <= controlSum) checkLeftFourSquares(p))
    })
  }

  private def checkLeftFourSquares(alreadySet: List[Int]): Unit = {
    permutationIndexes.filter((p) => !alreadySet.contains(p)).combinations(4).foreach((k) => {
      k.permutations.foreach((p) => if (checkCross(p, 3, 2, 1, 0)
        && squares(p(1)).nRD + squares(alreadySet.head).nLD + squares(p(3)).nRU + squares(alreadySet(2)).nLU == controlSum
        && squares(p.head).nLD + squares(p(2)).nLU <= controlSum)
        checkUpTwoSquares(alreadySet, p))
    })
  }

  private def checkUpTwoSquares(alreadySetRight: List[Int], alreadySetLeft: List[Int]): Unit = {
    permutationIndexes.filter((p) => !alreadySetRight.contains(p) && !alreadySetLeft.contains(p)).combinations(2).foreach((k) => {
      k.permutations.foreach((p) => if (squares(p.head).nRU + squares(p(1)).nLU <= controlSum
        && squares(p.head).nRD + squares(p(1)).nLD + squares(alreadySetLeft(1)).nRU + squares(alreadySetRight.head).nLU == controlSum
        && squares(p.head).nLD + squares(alreadySetLeft.head).nRU + squares(alreadySetLeft(1)).nLU <= controlSum
        && squares(p(1)).nRD + squares(alreadySetRight.head).nRU + squares(alreadySetRight(1)).nLU <= controlSum)
        checkDownTwoSquares(alreadySetRight, alreadySetLeft, p))
    })
  }

  def checkDownTwoSquares(alreadySetRight: List[Int], alreadySetLeft: List[Int], alreadySetUp: List[Int]): Unit = {
    permutationIndexes.filter((p) => !alreadySetRight.contains(p) && !alreadySetLeft.contains(p) && !alreadySetUp.contains(p)).combinations(2).foreach((k) => {
      k.permutations.foreach((p) => if (squares(p.head).nRD + squares(p(1)).nLD <= controlSum
        && squares(p.head).nRU + squares(p(1)).nLU + squares(alreadySetLeft(3)).nRD + squares(alreadySetRight(2)).nLD == controlSum
        && squares(p.head).nLU + squares(alreadySetLeft(3)).nLD + squares(alreadySetLeft(2)).nRD <= controlSum && squares(p(1)).nRU + squares(alreadySetRight(2)).nRD + squares(alreadySetRight(3)).nLD <= controlSum)
        printCurrentArrangement(alreadySetRight, alreadySetLeft, alreadySetUp, p))
    })
  }

  private def printCurrentArrangement(right: List[Int], left: List[Int], up: List[Int], down: List[Int]): Unit = {
    up.foreach((k) => println(squares(k)))
    for (i <- 0 to 1) println(squares(left(i)))
    for (i <- 0 to 1) println(squares(right(i)))
    for (i <- 2 to 3) println(squares(left(i)))
    for (i <- 2 to 3) println(squares(right(i)))
    down.foreach((k) => println(squares(k)))
    println()
  }

}
