object Main {
  def main(args: Array[String]): Unit = {
    time {
      new Field("input.txt", 12, 10).findSuitableCombinations()
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
}