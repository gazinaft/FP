package quickcheck.test

import org.junit.{Assert, Test}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Test.check
import quickcheck.TestFunc

class PersonalCheckSuite {
  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq:_*)

  @Test def `Test of personal task`: Unit =
    Assert.assertTrue(
      check(asProp(new TestFunc))(identity).passed
    )
}
