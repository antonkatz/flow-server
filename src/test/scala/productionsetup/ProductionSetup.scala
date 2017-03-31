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
      val antons_key =
        """|-----BEGIN PUBLIC KEY-----
           |MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA38extTmtQfWMbh+xgM9f
           |RYrT6/hYoZB2yz0AyXVkqaTNMhrlpgUkc/AKEK3Ai5Z4oCpI6+hs8zbdeRDT2q3H
           |Bh/9aWQ4J/KPFe5npPYNKfn0H9E9vwLVv6cHkkOHkpJ5CAYa9JYnWgMDrTSBA9G9
           |g27jjTsw4stKFVRILvNWUYyBXazmcUI42AtbgV31r2bCBZd7b9muhQzNsVPrF7EC
           |fazvOMZOTtdblWbbzF2w8/K8r30C6kaN3WH6kfETjV8UifxLZyrwNP+oPqj336am
           |hKGIDm8F98gE9+3EZgEBNm7zf2o3kmwl7cfmjOg+1kblkdFXt8eAvOyLHmLgn84H
           |PxgCaLi3bYwFcZJ9eA8voZFQOj1WaG+Njh2K/c71iUNRuZlYTDgf9B4HJZOLT9Jm
           |HuKkGuARuwc5EC892o44IuQ7aDgFUd9t33mmXPLrEzWzQA+0gpIGhvuxNLiW7M+i
           |NkZnpaa9bvct9WFGCGyjuQbO4LSvO/XszzjgA5c0tgMQD0715niXkxl12fuvQKrh
           |7YF2E+V7KZhEK5KaQ2RAg/4a0EzKyDHYSfIyQGA1qqkq47TP5U63MPZWzJvH92Mc
           |WxjFIkUlruQW3/9qE5dgCWv4XbaReTc8xGgUJ0K/HK3H6mdjxAfyl0YsQtbBxOHi
           |IhS/dU0xsBE1UkLXHJgOm+8CAwEAAQ==
           |-----END PUBLIC KEY-----
           |""".stripMargin
      var anton: UserAccount = null
      "generate antons account" in {
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

      "generate some invite codes from anton" in {
        println()
        for (i <- 1 to 20) {
          val i = Registrar.createInvitation(anton)
          println(i.right.get.code)
        }
      }
    }
  }
}
