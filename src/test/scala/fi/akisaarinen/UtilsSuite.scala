package fi.akisaarinen

import org.scalatra.test.scalatest._
import org.scalatest.matchers._
import org.scalatest.FunSuite

class UtilsSuite extends FunSuite with ShouldMatchers {
  test("Weight fitting works") {
    val dim1_1 = Weight(List(1));
    val dim1_2 = Weight(List(2));
    val dim1_3 = Weight(List(3));
    dim1_1.fits(dim1_1) should equal (true)
    dim1_1.fits(dim1_2) should equal (false)
    dim1_2.fits(dim1_1) should equal (true)
    dim1_2.fits(dim1_2) should equal (true)
    dim1_2.fits(dim1_3) should equal (false)
    dim1_3.fits(dim1_3) should equal (true)
    dim1_3.fits(dim1_1) should equal (true)
    dim1_3.fits(dim1_2) should equal (true)

    val dim2_1 = Weight(List(1, 1));
    val dim2_2 = Weight(List(2, 2));
    val dim2_3 = Weight(List(3, 3));
    dim2_1.fits(dim2_1) should equal (true)
    dim2_1.fits(dim2_2) should equal (false)
    dim2_2.fits(dim2_1) should equal (true)
    dim2_2.fits(dim2_2) should equal (true)
    dim2_2.fits(dim2_3) should equal (false)
    dim2_3.fits(dim2_3) should equal (true)
    dim2_3.fits(dim2_1) should equal (true)
    dim2_3.fits(dim2_2) should equal (true)

    dim2_3.fits(Weight(List(1, 2))) should equal(true)
    dim2_3.fits(Weight(List(1, 3))) should equal(true)
    dim2_3.fits(Weight(List(2, 3))) should equal(true)

    val dim3_1 = Weight(List(1, 1, 1));
    val dim3_2 = Weight(List(2, 2, 2));
    val dim3_3 = Weight(List(3, 3, 3));
    dim3_1.fits(dim3_1) should equal (true)
    dim3_1.fits(dim3_2) should equal (false)
    dim3_2.fits(dim3_1) should equal (true)
    dim3_2.fits(dim3_2) should equal (true)
    dim3_2.fits(dim3_3) should equal (false)
    dim3_3.fits(dim3_3) should equal (true)
    dim3_3.fits(dim3_1) should equal (true)
    dim3_3.fits(dim3_2) should equal (true)

    dim3_3.fits(Weight(List(1, 2, 3))) should equal(true)
    dim3_3.fits(Weight(List(1, 3, 3))) should equal(true)
    dim3_3.fits(Weight(List(2, 3, 2))) should equal(true)
  }
}
