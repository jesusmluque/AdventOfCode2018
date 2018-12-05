import scala.util.matching.Regex
import scalaz._

object ClaimsExercise {

  import Scalaz._
  class Claim(val id: Int, val left: Int, val top: Int, val width: Int, val size: Int)
  object Claim {
    def apply(claimString: String) = {
      println(claimString)
      val reg: Regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

      val a = reg.findAllMatchIn(claimString).next()
      new Claim(a.group(1).toInt, a.group(2).toInt, a.group(3).toInt, a.group(4).toInt, a.group(5).toInt)

    }
  }

  def overlap(a: Claim, b: Claim) = {
    val leftDelta = Math.abs(a.left - b.left)
    val topDelta = Math.abs(a.top - b.top)
    if ((a.left <= b.left && (leftDelta < a.width)) && (a.top <= b.top && (topDelta < a.size)))
      true
    else if ((a.left <= b.left && (leftDelta < a.width)) && (a.top >= b.top && (topDelta < b.size)))
      true
    else if ((a.left >= b.left && (leftDelta < b.width)) && (a.top >= b.top && (topDelta < b.size)))
      true
    else if ((a.left >= b.left && (leftDelta < b.width)) && (a.top <= b.top && (topDelta < a.size)))
      true
    else
      false
  }

  def overlapClaims(claimsStr: List[String]):Int = {
    val claims = claimsStr.map(Claim(_))
    (claims |@| claims) {(a,b) => overlap(a,b)}.count(s => s) - claims.length
  }
}
