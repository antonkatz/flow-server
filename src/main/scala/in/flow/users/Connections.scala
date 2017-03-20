package in.flow.users

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Everything and anything connections
  */
object Connections {
  import in.flow.algorithm.AlgorithmSettings._

  /** @return a sequence where each element is a collection of user accounts; the index of the first sequence
    *         corresponds to the depth at which this given user is connected to users in the collection */
  def getVisibleConnections(user: UserAccount): Future[Seq[Set[UserAccount]]] = {
    val starting_set = Set(user)
    getVisibleConnections(starting_set)
  }

  private def getVisibleConnections(of: Set[UserAccount], full_flat_set: Set[UserAccount] = Set(), full_leveled_set:
  Seq[Set[UserAccount]] = Nil, levels_left: Int = connections_search_depth): Future[Seq[Set[UserAccount]]] = {
    val ffs = full_flat_set ++ of

    val this_level_users = Future.sequence(of map Users.loadUserConnections)
    this_level_users flatMap (tlu => {
      val this_level = of flatMap {u => u.connections map {_._1}} diff ffs

      val ll = levels_left - 1
      if (ll > 0 && this_level.nonEmpty) {
        val fls = full_leveled_set :+ this_level
        getVisibleConnections(this_level, ffs, fls, ll)
      } else {
        Future(full_leveled_set)
      }
    })
  }
}
