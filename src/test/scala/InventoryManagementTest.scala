import org.scalatest.FlatSpec

import scala.io.Source

class InventoryManagementTest extends FlatSpec {

  "inventory with 5 items" should "have a checksum of 12" in {
    /*
    abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab
     */
    assert(InventoryManagement.checksum(List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")) === Some(12))
  }
  "inventory with a lot of items" should "have a result of " in {
    assert(InventoryManagement.checksum(Source.fromResource("inventory1.txt").getLines.toList) === Some(8715))
  }
  "find the correct id " should "have as id " in {
    /*
    abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
     */
    assert(InventoryManagement.correctId(List("abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz")) === Some("fgij"))
  }
  "find the correct id in a big set of ids" should "have a result of " in {
    assert(InventoryManagement.correctId(Source.fromResource("inventory1.txt").getLines.toList) === Some("adfd"))
  }
}
