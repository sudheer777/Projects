package zio_grpc.examples.lab4

import io.grpc.examples.helloworld.helloworld.ZioHelloworld.GreeterClient
import io.grpc.examples.helloworld.helloworld.{HelloRequest,HelloReply, CompRequest , CompReply}
import io.grpc.ManagedChannelBuilder
import zio.Console._
import scalapb.zio_grpc.ZManagedChannel
import zio._

object ComputeClient extends zio.ZIOAppDefault {
  val clientLayer: Layer[Throwable, GreeterClient] =
    GreeterClient.live(
      ZManagedChannel(
        ManagedChannelBuilder.forAddress("localhost", 9000).usePlaintext()
      )
    )

  def myAppLogic = {
    val lst = List(10,5,6,20)
    for {
      r <- GreeterClient.sayHello(HelloRequest("World"))
      _ <- printLine(r.message)
      r2 <- ZIO.foreachPar(lst) { no =>
               GreeterClient.sayComp(CompRequest("fact",no))
               }
      _ <- { val z = lst.zip(r2).map {case (arg,r) => (arg,r.res)}
             printLine(s"Ans for Par fact = ${z}")
           }
      r3 <- ZIO.foreachPar(lst) { no =>
        GreeterClient.sayComp(CompRequest("fib",no))
      }
      _ <- { val z = lst.zip(r3).map {case (arg,r) => (arg,r.res)}
        printLine(s"Ans for Par fib = ${z}")
      }
      r4 <- ZIO.foreachPar(lst) { no =>
        GreeterClient.sayComp(CompRequest("nthPrime",no))
      }
      _ <- { val z = lst.zip(r4).map {case (arg,r) => (arg,r.res)}
        printLine(s"Ans for Par nthPrime = ${z}")
      }
      r5 <- ZIO.foreachPar(lst) { no =>
        GreeterClient.sayComp(CompRequest("isPrime",no))
      }
      _ <- { val z = lst.zip(r5).map {case (arg,r) => (arg,r.res)}
        printLine(s"Ans for Par isPrime = ${z}")
      }
      - <- printLine(s"Set count as 200")
      _ <- GreeterClient.sayComp(CompRequest("setCnt",200) )
      z1 <- GreeterClient.sayComp(CompRequest("getCnt",0))
      - <- printLine(s"Ans for Par getCnt = ${z1.res}")

      - <- printLine(s"Set count as 800")
      _ <- GreeterClient.sayComp(CompRequest("setCnt",800) )
      z2 <- GreeterClient.sayComp(CompRequest("getCnt",0))
      - <- printLine(s"Ans for Par getCnt = ${z2.res}")

    } yield ()
    }

  final def run =
    myAppLogic.provideCustomLayer(clientLayer).exitCode
}
