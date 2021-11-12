package streams
import Individual.*

class IndividualSuite extends munit.FunSuite {

  test("negative numbers") {
    assert(fun(-100 to -1).forall(x => x == None))
  }
  test("out of D(x)") {
    assert(fun(55 until 56).forall(x => x == None))
  }

  test("valid calculations") {
    assert(fun(1 until 2).apply(0) match {
      case None => false
      case Some(x) => x == 2
    })

    assert(fun(241 until 242).apply(0) match {
      case None => false
      case Some(x) => x == 241
    })

  }

  test("reduced sum") {
    assert(reduced == validRes.sum)
  }

  test("takenWhile") {
    validRes.find(x => x >= 1000) match {
      case None => assert(takenWhile == validRes)
      case Some(value) => assert(validRes.apply(validRes.indexOf(value) - 1) == takenWhile.last)
    }
  }
}
