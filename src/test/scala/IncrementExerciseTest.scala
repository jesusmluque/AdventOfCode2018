import org.scalatest.FlatSpec

import scala.io.Source

class IncrementExerciseTest  extends FlatSpec {

  "A increments List of 1,2,3,4,5,6,-1,-2,-3,-4,-5,-6" should "sum 0" in {
    assert(IncrementExercise.result(List(1,2,3,4,5,6,-1,-2,-3,-4,-5,-6)) === 0)
  }

  "A big increments List" should "end" in {
    assert(IncrementExercise.result(Source.fromResource("increments.txt").getLines.toList.map(_.toInt)) === 582)
  }

  "A increments List of 1,2,3,4,5,6,-1,-2,-3,-4,-5,-6" should "repeat 15 twice first" in {
    assert(IncrementExercise.firstTwice(List(1,2,3,4,5,6,-1,-2,-3,-4,-5,-6)) === Some(15))
  }

  "A increments List of 1,2,3,4,5,6,-1,-2,-3,-4,-1,-6" should "repeat 15 twice first" in {
    assert(IncrementExercise.firstTwice(List(1,2,3,4,5,6,-1,-2,-3,-4,-1,-6)) === Some(15))
  }
  "A increments List of 1,2,3,4,5,6,-1,-2,-2,-4,-1,-6" should "no one repeated" in {
    assert(IncrementExercise.firstTwice(List(1,2,3,4,5,6,-1,-2,-2,-4,-1,-6)) === Some(6))
  }
  "A increments List of 1,2,3,4,5,6,-20,-2,-2,-4,-1,-6" should "repeat 1 twice first" in {
    assert(IncrementExercise.firstTwice(List(1,2,3,4,5,6,-1,-2,-2,-4,-1,-6)) === Some(6))
  }
  "A big increments List" should "find the first frequency repeated twice" in {
    assert(IncrementExercise.firstTwice(Source.fromResource("increments2.txt").getLines.toList.map(_.toInt)) === Some(488))
  }
}
