package quickcheck

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import quickcheck.TestFunc.{fact, part}

import scala.annotation.tailrec

class TestFunc extends Properties("Function") {

  property("IllegalValues") = forAll { (x : Int) =>
    if (x < 0 || x == 55) part.lift(x).isEmpty
    else true
  }

  property("LegalValues") = forAll { (x: Int) =>
    if (!part.isDefinedAt(x)) true
    else if (x < 55) fact(x) + x == part(x)
    else part(x) == x
  }
}

object TestFunc {

  @tailrec
  def fact(x: Double, acc: Double = 1): Double = {
    if (x == 0) acc
    else fact(x - 1, acc * x)
  }

  def part = new PartialFunction[Int, Double] {
    override def apply(v1: Int): Double = {
      if (v1 < 55) fact(v1) + v1
      else v1
    }

    override def isDefinedAt(x: Int): Boolean = x >= 0 && x != 55

  }

}
