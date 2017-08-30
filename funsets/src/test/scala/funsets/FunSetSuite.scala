package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = union(s1, s2)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(s4, 1), "Union 1")
      assert(contains(s4, 2), "Union 2")
      assert(!contains(s4, 3), "Union 3")
    }
  }

  test("intersect contains elements in both sets") {
    new TestSets {
      val s5 = intersect(s4, s1)
      assert(contains(s5, 1), "Intersect")
      assert(!contains(s5, 2), "Intersect")
    }
  }

  test("diff contains elements in first set but not second set") {
    new TestSets {
      val s5 = diff(s4, s1)
      assert(contains(s5, 2), "Diff")
      assert(!contains(s5, 1), "Diff")
    }
  }

  test("filter based on predicate and contained in set") {
    new TestSets {
      val s5 = union(s4, s3)
      def p(x: Int) = x % 2 == 0
      def s6 = filter(s5, p)
      assert(contains(s6, 2), "filter")
      assert(!contains(s6, 1), "filter")
      assert(!contains(s6, 3), "filter")
    }
  }

  test("forall works") {
    new TestSets {
      val s5 = (x: Int) => x % 4 == 0 // multiples of 4
      val s6 = (x: Int) => x % 3 == 0 // multiples of 3
      def isEven(x: Int) = x % 2 == 0
      assert(forall(s5, isEven), "forall") // all multliples of 4 are even
      assert(!forall(s6, isEven), "forall")
    }
  }

  test("exists works") {
    new TestSets {
      val s5 = (x: Int) => x % 4 == 0 // multiples of 4
      val s6 = (x: Int) => x % 3 == 0 // multiples of 3
      def isEven(x: Int) = x % 2 == 0
      assert(exists(s5, isEven), "exists") // all multliples of 4 are even
      assert(exists(s6, isEven), "exists") // some multiples of 3 are even
    }
  }

  test("map works") {
    new TestSets {
      val s5 = (x: Int) => x % 3 == 0 // multiples of 3
      def isEven(x: Int) = x % 2 == 0
      def multiplyTwo(x: Int) = 2 * x
      assert(forall(map(s5, multiplyTwo), isEven), "map") // multiply all terms by 2 makes them even
    }
  }


}
