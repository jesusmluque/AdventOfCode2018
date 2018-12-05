import scalaz._


object InventoryManagement {

  def checksum(items: List[String]):Option[Int] = {
    val res = items.foldLeft[(Option[Int], Option[Int])]((Some(0), Some(0))) { (acc, next) => {
      val count = next.toCharArray.foldLeft(Map[Char, Option[Int]]()) {(acc, next) => acc.updated(next, acc.get(next).flatten.map(_ + 1).orElse(Some(1)))}
      (acc._1.map(x => x + (if (count.exists(n => n._2.contains(2))) 1 else 0)), acc._2.map(y => y + (if (count.exists(n => n._2.contains(3))) 1 else 0)))
    }
    }
    for {
      two <- res._1
      three <- res._2
    } yield two * three
  }

  def correctId(items: List[String]):Option[String] = {
    import Scalaz._
    val res = (items |@| items) {(a,b) => a.zip(b).filter(n => n._1 == n._2).unzip}.map(_._1).filter(_.length == items.head.length - 1).map(_.mkString(""))
    res match {
      case List() => None
      case a :: _ => Some(a)
    }
  }
}
