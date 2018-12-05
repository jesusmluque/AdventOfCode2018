import ClaimsExercise.Claim
import scala.io.Source
import scalaz._
import Scalaz._

import scala.util.matching.Regex

val reg: Regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

for (a <- reg.findAllMatchIn("#2 @ 1,3: 4x4")) {
  println(s"${a.group(1)}")
  println(s"${a.group(2)}")
  println(s"${a.group(3)}")
  println(s"${a.group(4)}")
  println(s"${a.group(5)}")
}

val a = Claim("#1 @ 1,3: 4x4")

val b = Claim("#2 @ 2,3: 4x4")
val leftDelta = Math.abs(a.left - b.left)
val topDelta = Math.abs(a.top - b.top)
ClaimsExercise.overlap(Claim("#1 @ 1,3: 4x4"), Claim("#1 @ 2,3: 4x4"))
if ((a.left <= b.left && (leftDelta < a.width)) && (a.top <= b.top && (topDelta < a.size)))
  true
else
  false

IncrementExercise.result(List(1,2,3,4,5,-1,-2,-3,-4,-5,6))


val dropN = (s: Stream[Int], n: Int) => s.zipWithIndex.filter(_._2 < n).map(_._1)
dropN(Stream(4,3,5,3,1,8,9,11,23,4,5), 6).toList

Some(9).filter(List(3,4,5,6,1,3).contains)
//IncrementExercise.firstTwice(List(1,2,3,4,5,6,-1,-2,-3,-4,-5,-6))._1
//IncrementExercise.firstTwice(Source.fromResource("increments.txt").getLines.toList.map(_.toInt))
/*
   abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both. 1,1
abbcde contains two b, but no letter appears exactly three times. 2,1
abcccd contains three c, but no letter appears exactly two times.2,2
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab
    */
"aabcdd".toCharArray.foldLeft(Map[Char, Option[Int]]()) {(acc, next) => acc.updated(next, acc.get(next).flatten.map(_ + 1).orElse(Some(1)))}
val items = List("abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz")
//(items |@| items) {(a,b) => a.zip(b).filter(n => n._1 == n._2).unzip}.map(_._1).filter(_.length == items.head.length - 1).map(_.mkString(""))