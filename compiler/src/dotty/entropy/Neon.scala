package dotty.entropy

import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.core.Contexts.Context


/**
 * Selective tracing for Dotty
 */
object Neon
  private val log = collection.mutable.ListBuffer.empty[String]

  private def currentHash: String =
    val md = MessageDigest.getInstance("MD5")
    md.update(log.mkString.getBytes)
    val digest: Array[Byte] = md.digest()
    DatatypeConverter.printHexBinary(digest).toLowerCase

  def trace(msg: String)(using ctx: Context): Unit =
    val traceInsideMethod = ctx.settings.EtraceInsideMethod.value
    if traceInsideMethod.isEmpty || Thread.currentThread.getStackTrace.exists(_.getMethodName.contains(traceInsideMethod))
      log.append(msg)
      if ctx.settings.EinspectAtHash.value.isEmpty
        println(s"""\u001b[43;1m\u001b[30m${currentHash}\u001b[0m $msg""")

  def inspect(f: => Unit)(using ctx: Context): Unit =
    val targetHash = ctx.settings.EinspectAtHash.value
    if currentHash == targetHash then f
