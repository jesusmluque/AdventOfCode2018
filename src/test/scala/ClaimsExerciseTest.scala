import org.scalatest.FlatSpec

class ClaimsExerciseTest extends FlatSpec {

  "The set of claims #1 @ 1,3: 4x4, #2 @ 3,1: 4x4, #3 @ 5,5: 2x2" should "be 2" in {
    assert(ClaimsExercise.overlapClaims(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2")) == 2)
  }

  "The set of claims #1 @ 3,1: 4x4, #2 @ 1,3: 4x4, #3 @ 5,5: 2x2" should "be 2" in {
    assert(ClaimsExercise.overlapClaims(List("#1 @ 3,1: 4x4", "#2 @ 1,3: 4x4", "#3 @ 5,5: 2x2")) == 2)
  }

  "The set of claims #1 @ 1,3: 4x4, #2 @ 10,30: 4x4, #3 @ 5,5: 2x2" should "be 0" in {
    assert(ClaimsExercise.overlapClaims(List("#1 @ 1,3: 4x4", "#2 @ 10,30: 4x4", "#3 @ 5,5: 2x2")) == 0)
  }


}
