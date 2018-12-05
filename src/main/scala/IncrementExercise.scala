object IncrementExercise {

  def result(increments: List[Int]) =  increments.sum
  def firstTwice(increments: List[Int]) = {
    def findTwice(list: List[Int]):(List[Int], Option[Int]) = {
      val res = increments.foldLeft[(List[Int], Option[Int])]((list, None)) { (acc, next) => {
          val newfreq = next + acc._1.head
          (newfreq :: acc._1, acc._2 match {
            case None => Some(newfreq).filter(acc._1.contains)
            case a => a
          })
        }
      }
      res._2 match {
        case None => findTwice(res._1)
        case _ => res
      }
    }
    findTwice(List(0))._2
  }
}
