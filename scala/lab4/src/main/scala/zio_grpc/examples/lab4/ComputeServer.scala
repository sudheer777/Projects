package zio_grpc.examples.lab4

import io.grpc.Status
import scalapb.zio_grpc.ServerMain
import scalapb.zio_grpc.ServiceList
import zio._
import zio.Console._

// code template was taken from the helloworld example
import io.grpc.examples.helloworld.helloworld.ZioHelloworld.ZGreeter
import io.grpc.examples.helloworld.helloworld.{HelloReply, HelloRequest, CompRequest , CompReply}
import java.util.concurrent.atomic._

object GreeterImpl extends ZGreeter[ZEnv, Any] {

  private def fact(x:Int):Int = {
    if (x==0) 1
    else x * fact(x-1)
  }

  /*
     this is an Atomic Integer from java.util.concurrent.atomic._
     It can be used to synchronize access
     from concurrent threads of execution.
     Use of get/set may be racy.
     However, addAndGet can ensure that its
       combined add and get operation is atomic.
  */
  private val cnt = new AtomicInteger(0)
  private def get_Cnt():UIO[Int] = ZIO.succeed(cnt.get())
  private def set_Cnt(n:Int):UIO[Unit] = ZIO.succeed(cnt.set(n))
  private def add_and_get_Cnt(n:Int):UIO[Int] = ZIO.succeed(cnt.addAndGet(n))

  private lazy val fibs:LazyList[BigInt] =
      BigInt(1) #:: BigInt(1) #:: fibs.zip(fibs.tail).map (n=> n._1 + n._2)
  private def inf(i:Int):LazyList[BigInt] =
      BigInt(i) #:: inf(i+1)
  private lazy val primes:LazyList[BigInt] = {
      sieve(inf(2))
   }
  private def nth[A](xs:LazyList[A], n:Int):A  = {
      xs.drop(n-1).head
  }
  private def sieve(xs:LazyList[BigInt]): LazyList[BigInt] = {
      xs match {
          case x #:: xs1 => x #:: sieve(xs1.filter(m => ((m mod x) != 0)))
      }
  }
  def sayHello(
      request: HelloRequest
  ): ZIO[zio.ZEnv, Status, HelloReply] =
    printLine(s"Got request: $request").orDie zipRight
      ZIO.succeed(HelloReply(s"Hello, ${request.name}"))
    /*
    for {
      _ <- printLine(s"Got request: $request")
      r <- ZIO.succeed(HelloReply(s"Hello, ${request.name}"))
    } yield r
    */

  def sayComp(
      request: CompRequest
  ): ZIO[zio.ZEnv, Status, CompReply] =
    {
     for {
      // racy use of AtomicInteger
       // used add_and_get atomic operation instead of get and set
      no <- add_and_get_Cnt(1)
      // This get is used when getCnt used
      cn  <- get_Cnt()
       _ <- printLine(s"Got compute request ($no) fpr ${request.cmd} ${request.arg}").orDie
      // set cnt variable if setCnt command is given
      _ <- if (request.cmd == "setCnt") set_Cnt(request.arg) else ZIO.succeed()
       r <- if (request.cmd=="fact")
              ZIO.succeed(CompReply(true,fact(request.arg)))
            else if (request.cmd=="fib")
              ZIO.succeed(CompReply(true,nth(fibs, request.arg).toInt))
            else if (request.cmd=="nthPrime")
              ZIO.succeed(CompReply(true,nth(primes, request.arg).toInt))
            else if (request.cmd=="isPrime")
              ZIO.succeed(CompReply(true,if (primes.takeWhile(x => x <= request.arg).exists(x => x.toInt == request.arg)) 1 else 0))
            else if (request.cmd=="getCnt")
              ZIO.succeed(CompReply(true,cn))
            else if (request.cmd=="setCnt") {
              ZIO.succeed(CompReply(true,0))
            } else ZIO.succeed(CompReply(false,-1))
     } yield r
     /*
     (printLine(s"Got compute request ${request.cmd} ${request.arg}").orDie zipRight 
     (if (request.cmd=="fact") ZIO.succeed(CompReply(true,fact(request.arg)))
      else ZIO.succeed(CompReply(false,-1)))
      )
      */
    }
}

object ComputeServer extends ServerMain {
  def services: ServiceList[zio.ZEnv] = ServiceList.add(GreeterImpl)
  val effect: ZIO[zio.ZEnv, Nothing, Unit] =
     for {
       _ <- printLine("Hello, World!").orDie
     } yield ()
}

