package scalashop

import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite extends munit.FunSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  def check(x: Int, y: Int, expected: Int, dst:Img): Unit =
    assert(dst(x, y) == expected)

  test("boxBlurKernel with radius 0") {
    val src = new Img(3, 3)
    for {
      x <- 0 until 3
      y <- 0 until 3
    } src(x, y) = rgba(x, y, x, y)

    for (x <- 0 until 3; y <- 0 until 3)
      assert(boxBlurKernel(src, x, y, 0) == rgba(x, y, x, y))
  }

  test("HorizontalBoxBlur blur with radius 2 is correct") {
    val width = 2
    val height = 2

    val src = new Img(width, height)
    val dst = new Img(width, height)

    src(0, 0) = 0
    src(1, 0) = 1
    src(0, 1) = 3
    src(1, 1) = 4

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    check(0, 0, 2, dst)
    check(1, 0, 2, dst)
    check(0, 1, 2, dst)
    check(1, 1, 2, dst)
  }

  test("VerticalBoxBlur parBlur with radius 2 is correct") {

    val width = 4
    val height = 3
    val src = new Img(width, height)
    val dst = new Img(width, height)

    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 3
    src(0, 1) = 4; src(1, 1) = 5; src(2, 1) = 6; src(3, 1) = 7
    src(0, 2) = 8; src(1, 2) = 9; src(2, 2) = 10; src(3, 2) = 11
    VerticalBoxBlur.parBlur(src, dst, 2, 2)

    check(0, 0, 5, dst)
    check(1, 0, 5, dst)
    check(2, 0, 5, dst)
    check(3, 0, 6, dst)
    check(0, 1, 5, dst)
    check(1, 1, 5, dst)
    check(2, 1, 5, dst)
    check(3, 1, 6, dst)
    check(0, 2, 5, dst)
    check(1, 2, 5, dst)
    check(2, 2, 5, dst)
    check(3, 2, 6, dst)
  }

  test("VerticalBoxBlur`s result is equal to basic blur result") {
    val src1 = new Img(3, 3)
    val src2 = new Img(3, 3)
    val dst2 = new Img(3, 3)


    for (x <- 0 until 3; y <- 0 until 3)
      src1(x, y) = rgba(x, y, x, y)
    for (x <- 0 until 3; y <- 0 until 3)
      src2(x, y) = rgba(x, y, x, y)

    VerticalBoxBlur.parBlur(src2, dst2, 2, 1)

    for (x <- 0 until 3; y <- 0 until 3)
      assert(boxBlurKernel(src1, x, y, 1) == dst2(x,y))
  }

  test("HorizontalBoxBlur`s result is equal to basic blur result") {
    val src1 = new Img(3, 3)
    val src2 = new Img(3, 3)
    val dst2 = new Img(3, 3)


    for (x <- 0 until 3; y <- 0 until 3)
      src1(x, y) = rgba(x, y, x, y)
    for (x <- 0 until 3; y <- 0 until 3)
      src2(x, y) = rgba(x, y, x, y)

    HorizontalBoxBlur.parBlur(src2, dst2, 2, 1)

    for (x <- 0 until 3; y <- 0 until 3)
      assert(boxBlurKernel(src1, x, y, 1) == dst2(x,y))

  }

  test("Blurs don't differ from task count") {
    val src = new Img(3, 3)
    for (x <- 0 until 3; y <- 0 until 3) {
      src(x, y) = rgba(x, y, x, y)
    }
    val dst1 = new Img(3, 3)
    val dst2 = new Img(3, 3)
    val dst3 = new Img(3, 3)
    val dst4 = new Img(3, 3)
    HorizontalBoxBlur.parBlur(src, dst1, 1, 1)
    HorizontalBoxBlur.parBlur(src, dst2, 2, 1)
    HorizontalBoxBlur.parBlur(src, dst3, 3, 1)
    HorizontalBoxBlur.parBlur(src, dst4, 4, 1)

    for (x <- 0 until 3; y <- 0 until 3) {
      assert(dst1(x, y) == dst2(x,y))
      assert(dst2(x, y) == dst3(x,y))
      assert(dst3(x, y) == dst4(x,y))
    }

  }


}
