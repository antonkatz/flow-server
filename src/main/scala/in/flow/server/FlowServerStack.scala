package in.flow.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.{`Access-Control-Allow-Headers`, `Access-Control-Allow-Origin`}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, Rejection, RejectionHandler, Route}
import akka.stream.ActorMaterializer
import in.flow.server.ServerDirectives._
import org.slf4j.LoggerFactory

import scala.io.StdIn

object FlowServerStack extends InnerRoutes {
  private val logger = LoggerFactory.getLogger("SS")

  def main(args: Array[String]) {

    implicit val system = ActorSystem("flow-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val cors_headers = Seq(`Access-Control-Allow-Origin`.*)

    implicit def myExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case e: Throwable =>
          logger.error("Excepitons: {}", e.getMessage)
          respondWithHeaders(cors_headers: _*) {
            /* fixme this should be nicer */
            complete(StatusCodes.InternalServerError)
          }
      }

    implicit def myRejectionHandler: RejectionHandler = RejectionHandler.newBuilder()
      .handleAll(defaultRejectionBehaviour).result()
      .mapRejectionResponse(r => {
        r.copy(headers = r.headers ++ cors_headers)
      })

    def defaultRejectionBehaviour(r: scala.collection.immutable.Seq[Rejection]): Route = {
      logger.error("Rejections: {}", r.map(_.toString).mkString(";\t\n"))
      RejectionHandler.default(r) getOrElse complete((BadRequest, "Unknown error"))
    }

    val route = respondWithHeaders(cors_headers: _*) {
      (extractUri & extractMethod) { (uri, method) =>
        logger.debug("{} request @ {}", Seq(method, uri.toRelative): _*)

        // this is for pesky browsers that need access-control-origin-headers
        // making sure this is a cors request
        headerValueByName("Access-Control-Request-Method") { _ =>
          logger.debug("cors request")
          extractRequest { r =>
            r.headers.find(_ is "Access-Control-Request-Headers".toLowerCase) match {
              case Some(h) =>
                respondWithHeader(`Access-Control-Allow-Headers`(h.value())) {
                  complete(StatusCodes.OK)
                }
              case _ => complete(StatusCodes.ExpectationFailed)
            }
          }
        } ~ securityDirective { implicit s =>
          logger.debug("entering secured code")
          (secureRequestDirective(s) & secureResponseDirective(s) & securityRejectionHandler(s)) {
            insecureInnerRoute(s)
          }
        }
      }
    }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println("Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
