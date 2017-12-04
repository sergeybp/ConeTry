case class Square(var nLU: Int, var nRU: Int, var nLD: Int, var nRD: Int) {

  def this(values: Array[Int]) = this(values(0), values(1), values(2), values(3))

  override def toString: String = s"$nLU $nRU $nLD $nRD"
}
