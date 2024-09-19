package org.sireum.hamr.codegen.common.util.test

import com.zaxxer.nuprocess._
import org.sireum.$internal.CollectionCompat.Converters._
import org.sireum._

import java.io.PrintWriter
import java.nio.charset.{StandardCharsets => SC}
import java.nio.file.{Path => JPath, Paths => JPaths}
import java.nio.{ByteBuffer => BB}
import java.util.concurrent.{TimeUnit => TU}

/* Customization of Kekinian's Os.proc functionality meant for
 * HAMR Codegen regression testing.  It optionally scans stdout
 * for a key that, when found, will trigger a newline to be sent
 * once a timer expires
 */
object TestOs {

  var isNative: B = NativeUtil.isNative

  def proc(e: Os.Proc): Os.Proc.Result = {
    return proc2(e, None(), None())
  }

  def proc2(ep: Os.Proc, timeoutKey: Option[String], textToSendAfterTimeout: Option[String]): Os.Proc.Result = {
    val e: Os.Proc =
      if (ep.isScript)
        if (Os.isWin) ep(cmds = ISZ[String]("cmd", "/c") ++ ep.cmds)
        else ep(cmds = "sh" +: ep.cmds)
      else ep

    def nativ(): Os.Proc.Result = {
      halt("Native not implemented")
    }

    def jvm(): Os.Proc.Result = {
      val commands = new java.util.ArrayList(e.cmds.elements.map(_.value).asJavaCollection)
      val m = scala.collection.mutable.Map[Predef.String, Predef.String]()
      if (e.shouldAddEnv) {
        for ((k, v) <- System.getenv().asScala) {
          val key = k.toString
          val value = v.toString
          m(key) = value
        }
      }
      for ((k, v) <- e.envMap) {
        val key = k.toString
        val value = v.toString
        m(key) = value
      }
      if (e.shouldPrintEnv) {
        for ((k, v) <- m) {
          println(s"$k = $v")
        }
      }
      if (e.shouldPrintCommands) {
        println(e.cmds.elements.mkString(" "))
      }
      val npb = new NuProcessBuilder(commands, m.asJava)
      npb.setCwd(toNIO(e.wd.value))
      val out = new java.io.ByteArrayOutputStream()
      val err = new java.io.ByteArrayOutputStream()

      val useTimeoutKey: B = timeoutKey.nonEmpty && e.timeoutInMillis > 0

      def stopDemoTimer(np: NuProcess, timeout: Long): Unit = {
        new Thread(() => {
          println(s"<<<<<<< Starting a 'timer' for ${timeout} ms >>>>>>>>")
          Thread.sleep(timeout)

          if (textToSendAfterTimeout.nonEmpty) {
            np.wantWrite()
            Thread.sleep(1) // wait for input stream to be flushed
            np.closeStdin(false)
          }
        }).start()
      }

      var p: NuProcess = null
      npb.setProcessListener(new NuAbstractProcessHandler {
        def append(isOut: B, buffer: BB): Unit = {
          if (e.shouldOutputConsole) {
            lazy val s = {
              val bytes = new Array[Byte](buffer.remaining)
              buffer.get(bytes)
              new Predef.String(bytes, SC.UTF_8)
            }
            if (useTimeoutKey && s.contains(timeoutKey.get.native)) {
              stopDemoTimer(p, e.timeoutInMillis.toLong)
            }
            if (isOut) System.out.print(s)
            else if (e.isErrBuffered && !e.isErrAsOut) for (_ <- 0 until buffer.remaining()) err.write(buffer.get)
            else System.err.print(s)
          } else {
            if (isOut || e.isErrAsOut) for (_ <- 0 until buffer.remaining()) out.write(buffer.get)
            else for (_ <- 0 until buffer.remaining()) err.write(buffer.get)
          }
        }

        override def onStdinReady(buffer: BB): Boolean = {
          if (textToSendAfterTimeout.nonEmpty) {
            buffer.put(textToSendAfterTimeout.get.native.getBytes())
          }
          return false
        }

        override def onStderr(buffer: BB, closed: Boolean): Unit = {
          if (!closed) append(F, buffer)
        }

        override def onStdout(buffer: BB, closed: Boolean): Unit = {
          if (!closed) append(T, buffer)
        }
      })
      p = npb.start()
      if (p != null && p.isRunning) {
        e.in match {
          case Some(in) =>
            p.writeStdin(BB.wrap(in.value.getBytes(SC.UTF_8)))
          case _ =>
        }

        if (!useTimeoutKey && e.timeoutInMillis > 0) {
          stopDemoTimer(p, e.timeoutInMillis.toLong)
        }

        // 0 for infinite wait
        val exitCode = p.waitFor(0, TU.MILLISECONDS)

        if (exitCode != scala.Int.MinValue) return Os.Proc.Result.Normal(exitCode,
          out.toString(SC.UTF_8.name), err.toString(SC.UTF_8.name))
        if (p.isRunning) try {
          p.destroy(false)
          p.waitFor(500, TU.MICROSECONDS)
        } catch {
          case _: Throwable =>
        }
        if (p.isRunning)
          try p.destroy(true)
          catch {
            case _: Throwable =>
          }
        Os.Proc.Result.Timeout(out.toString, err.toString)
      } else Os.Proc.Result.Exception(s"Could not execute command: ${e.cmds.elements.mkString(" ")}")
    }

    try {
      if (isNative || e.shouldUseStandardLib) {
        nativ()
      } else {
        try {
          jvm()
        } catch {
          case _: UnsatisfiedLinkError | _: NumberFormatException | _: ExceptionInInitializerError =>
            isNative = T
            nativ()
        }
      }
    } catch {
      case t: Throwable =>
        val sw = new java.io.StringWriter
        t.printStackTrace(new PrintWriter(sw))
        Os.Proc.Result.Exception(sw.toString)
    }
  }

  private def toNIO(path: String): JPath = JPaths.get(path.value)
}
