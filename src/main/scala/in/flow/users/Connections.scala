package in.flow.users

import java.util.Base64

import in.flow.db.{Db, DbSchema, UserAccountConnectionStorable}
import in.flow.commformats.InternalCommFormats.UserConnectionType.UserConnectionType
import scribe.Logger
import in.flow.{DatabaseError, FutureErrorFlow, InvalidInputError, WithErrorFlow, getLogger, global_sha}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.PostgresProfile.api._

import scala.collection.{immutable => imm}

/**
  * Everything and anything connections
  */
object Connections {
  import in.flow.algorithm.AlgorithmSettings._

  private val logger: Logger = "Connections"

  /** @return a sequence where each element is a collection of user accounts; the index of the first sequence
    *         corresponds to the depth at which this given user is connected to users in the collection */
  def getVisibleConnections(user: UserAccount): Future[Seq[Set[UserAccount]]] = {
    getVisibleConnectionsLeveled(user)
  }

  /**   from user connections finds all users whose ids match
    *  @return list of ids mapped to dispaly names in the same order as the list of ids was given */
  def resolveIdsToNames(ids: Iterable[String], asker: UserAccount): Future[WithErrorFlow[Set[(String, String)]]] = {
    getVisibleConnectionsFlat(asker) map {cons =>
      val consWithSelf = cons + asker
      val unique_ids = ids.toSet
      val names = unique_ids map {id => consWithSelf find {_.user_id == id}} map {_ map {u => u.user_id -> u.display_name}}
      // there should not be any that are missing
      if (names exists {_.isEmpty})
        Left(InvalidInputError("hmm... snooping where you shouldn't, are you?"))
      else
        Right(names.flatten)
    }
  }

  /** Not Safe - does not check that the users actually exists */
  def connectUsers(from: UserAccountPointer, to: UserAccountPointer, connection_type: UserConnectionType):
  FutureErrorFlow[_] = {
    checkConnectionExists(from, to) flowWith((exists: Boolean) => if (!exists) {
      storeConnection(from_id = from.user_id, to_id = to.user_id, connection_type)
    } else Future(Right()))
  }

  /** asynchronous; NOT SAFE -- does no check that the user ids exist, does not check that
    * the connection already exists (the database should though)
    * @return a future of database access to create the connection */
  private def storeConnection(from_id: String, to_id: String,
                                  connection_type: UserConnectionType): Future[WithErrorFlow[Unit]] = {
    val id = Base64.getEncoder.encodeToString(global_sha.digest((from_id + to_id).getBytes))
    val con = UserAccountConnectionStorable(id, from_id, to_id, connection_type.toString)
    val f = Db.run(DbSchema.user_account_connections += con)
    f map {_ => Right(): WithErrorFlow[Unit]} recover {
      case e => logger.error(s"Could not store a connection between users ${from_id} and ${to_id}: " +
        s"${e.getMessage}")
        Left(DatabaseError("we couldn't connect you two folks"))
    }
  }

  /** checks that `u1` is not connected to `u2` and that `u2` is not connected to `u1` */
  private def checkConnectionExists(u1: UserAccountPointer, u2: UserAccountPointer): FutureErrorFlow[Boolean] = {
    val q = DbSchema.user_account_connections.filter(con =>
      (con.from === u1.user_id && con.to === u2.user_id) ||
        (con.to === u1.user_id && con.from === u2.user_id)
    ).exists
    Db.run(q.result) map {Right(_)} recover {
      case e => logger.error(s"""Could not check if the connection already exists between ${u1.user_id} and ${u2
        .user_id}: ${e.getMessage}""")
        Left(DatabaseError("we couldn't check if you folks are connected"))
    }
  }

  private def getVisibleConnectionsLeveled(of: UserAccount, levels_left: Int = connections_search_depth):
  Future[Seq[Set[UserAccount]]] = {
    getVisibleConnections(Set(of), (b: Seq[Set[UserAccount]], l: Set[UserAccount]) => {b :+ l}, Seq(), levels_left)
  }

  private def getVisibleConnectionsFlat(of: UserAccount, levels_left: Int = connections_search_depth):
  Future[Set[UserAccount]] = {
    getVisibleConnections(Set(of), (b: imm.Set[UserAccount], l: Set[UserAccount]) => {b ++ l}, imm.Set[UserAccount](),
      levels_left)
  }

  /**
    * finds all unique connections of users in `of` within a certain depth
    * takes a custom function, keeps its output from previous step in a buffer
    * */
  private def getVisibleConnections[T](of: Set[UserAccount], collection_function: (T, Set[UserAccount]) => T,
                                       collection_buffer:T, levels_left: Int,
                                      full_flat_set: Set[UserAccount] = Set()):
  Future[T] = {
    val ffs = full_flat_set ++ of

    val this_level_users = Future.sequence(of map Users.loadUserConnections)
    this_level_users flatMap (tlu => {
      val this_level = of flatMap {u => u.connections map {_._1}} diff ffs

      val ll = levels_left - 1
      if (ll > 0 && this_level.nonEmpty) {
        val buffer = collection_function(collection_buffer, this_level)
        getVisibleConnections(this_level, collection_function, buffer, ll, ffs)
      } else {
        Future(collection_buffer)
      }
    })
  }
}