package streams

object Individual extends App {
  def fun(seq: Seq[Int]): List[Option[Double]] = {

    def part = new PartialFunction[Int, Double] {
      def fact(x: Double, acc: Double = 1): Double = {
        if (x == 0) acc
        else fact(x - 1, acc * x)
      }

      override def apply(v1: Int): Double = {
        if (v1 < 55) fact(v1) + v1
        else v1
      }

      override def isDefinedAt(x: Int): Boolean = x >= 0 && x != 55
    }
    seq.map((x:Int) => if (part.isDefinedAt(x)) Some(part.apply(x)) else None).toList
  }

  def getValid(list: List[Option[Double]]): List[Double] = for (Some(x) <- list) yield x

  val validRes = (fun andThen getValid)(-250 to 250)

  // HOF 1 partition
  val oddsAndEvens = validRes.partition(_ % 2 == 0)

  // HOF 2 find
  val first = validRes.find(_ >= 1_000_000) match {
    case None => 1_000_000
    case Some(x) => x
  }

  // HOF 3 reduce
  val reduced = validRes.reduce((acc: Double, x: Double) => acc + x)

  // HOF 4 forall
  val isPositive = validRes.forall(_ > 0)

  // HOF 5 takeWhile
  val takenWhile = validRes.takeWhile(_ < 1000)

}
