package productionsetup

import java.sql.Timestamp
import java.util.Base64

import in.flow.db.{Db, DbSchema}
import in.flow.security.Encryption
import in.flow.users.registration.Registrar
import in.flow.users.{UserAccount, Users}
import in.flow.utils.Hex
import in.flow_test
import org.scalatest.WordSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import slick.jdbc.PostgresProfile.api._

/**
  * Created by anton on 12/02/17.
  */
class ProductionSetup extends WordSpec {
  "Production setup" when {

    "creating antons account" should {
      var anton: UserAccount = null
      "generate antons account" in {
        val antons_key =
          """-----BEGIN PUBLIC KEY-----
            |MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAzGgC5J1USoPOwDZ7F63m
            |MNnB8Sdfjwcy9/gfBPGj6E87HPoZkfMco1edYyXptZKEguJuEFxyP+8cSjwf6Igf
            |O46wEzta2wrCFe75LFVeLUsCl2ekWck42pEjO+58pKLPgs73ynwr/I/SZCOoyyzo
            |DnFuHC4zNfZ3d9IXVsRRn9YT8Tdvq6R/J2qjglW747G9+7txkXEZzoi89aDK9Ql0
            |oiRtl61jMGGVyVoZgaIq5409xZ3Cw2/gejgx6Zp1RWqR3B1VOj6DgzVCJpKVW4QA
            |0eeHJbDkpGzIsHeYzlEJKy02JjJq5zv2ksOJ4DiLOp75KyHkMcVdXksFDo3y0Bio
            |zSwTArgPtZZazDdviiSOFN+c1+1ALTWEXCgakpg+xUhd5lbGhBb53g/Jiwgey2XL
            |yRML4hVk023HtfF+5EdZWvBc1VzAxznZ5HCl3P3QkxsGJk/dMEub4Vpet5Tnr+QW
            |hVBdp/pwXK9AAYl+Oc+1OgcLtC3AjMa9esAEtZ5kz+cimSkr1XCTuqRvE2FwlXgr
            |umfIOkVBgwdPuXcSegUOAHLgHWEzevUf10dIzhp37BO4ltuLyTmwILqe8yJwJaHF
            |xmtjz0lBfv+cQxdGbRoHpTQ5LfcjVWTjOtswK2AaTc/t2DUX4GF+kdfcrdIoPDfD
            |3c5bwQwMWzDFWos8BROgiH8CAwEAAQ==
            |-----END PUBLIC KEY-----""".stripMargin
        val key = Encryption.parsePublicKey(antons_key)
        key foreach {k =>
          Users.getUserId(k) foreach {id =>
            val now = in.flow.getNow
            val timestamp = Timestamp.from(now)
            val inst = timestamp.toInstant
            val u: UserAccount = UserAccount(id, "anton kats", k)
            anton = u
            val ins = Db.run(DbSchema.user_accounts += u.storable)
            val r = Await.result(ins, Duration.Inf)
            println(r)

            val ret = Await.result(Db.run(DbSchema.user_accounts.filter(_.id === id).result), Duration.Inf  )
            val ret_time = ret.head.timestamp
            val ret_inst = ret_time.toInstant
            print(ret)
          }
        }
      }

      "generate some invite codes from some user" in {
        println()
//        val from_user = Await.result(Users.getUser(anton.user_id), Duration.Inf).get
        val from_user = Await.result(Users.getUser("FQ6ENiykQ1k79iagge1LN7kQCf0FPcdhQZjbwnW/Dx8="), Duration.Inf).get
        for (i <- 1 to 20) {
          val inv = Registrar.createInvitation(from_user)
          println(inv.right.get.code)
        }
      }
    }
  }
}
