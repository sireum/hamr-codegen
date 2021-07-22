// #Sireum
// @formatter:off

// This file is auto-generated from TestResult.scala

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

object TestMsgPack {

  object Constants {

    val TestResource: Z = -32

    val TestResult: Z = -31

  }

  object Writer {

    @record class Default(val writer: MessagePack.Writer.Impl) extends Writer

  }

  @msig trait Writer {

    def writer: MessagePack.Writer

    def writeTestResource(o: TestResource): Unit = {
      writer.writeZ(Constants.TestResource)
      writer.writeString(o.content)
      writer.writeB(o.overwrite)
      writer.writeB(o.makeExecutable)
      writer.writeB(o.makeCRLF)
    }

    def writeTestResult(o: TestResult): Unit = {
      writer.writeZ(Constants.TestResult)
      writer.writeMap(o.map, writer.writeString _, writeTestResource _)
    }

    def result: ISZ[U8] = {
      return writer.result
    }

  }

  object Reader {

    @record class Default(val reader: MessagePack.Reader.Impl) extends Reader {
      def errorOpt: Option[MessagePack.ErrorMsg] = {
        return reader.errorOpt
      }
    }

  }

  @msig trait Reader {

    def reader: MessagePack.Reader

    def readTestResource(): TestResource = {
      val r = readTestResourceT(F)
      return r
    }

    def readTestResourceT(typeParsed: B): TestResource = {
      if (!typeParsed) {
        reader.expectZ(Constants.TestResource)
      }
      val content = reader.readString()
      val overwrite = reader.readB()
      val makeExecutable = reader.readB()
      val makeCRLF = reader.readB()
      return TestResource(content, overwrite, makeExecutable, makeCRLF)
    }

    def readTestResult(): TestResult = {
      val r = readTestResultT(F)
      return r
    }

    def readTestResultT(typeParsed: B): TestResult = {
      if (!typeParsed) {
        reader.expectZ(Constants.TestResult)
      }
      val map = reader.readMap(reader.readString _, readTestResource _)
      return TestResult(map)
    }

  }

  def to[T](data: ISZ[U8], f: Reader => T): Either[T, MessagePack.ErrorMsg] = {
    val rd = Reader.Default(MessagePack.reader(data))
    rd.reader.init()
    val r = f(rd)
    rd.errorOpt match {
      case Some(e) => return Either.Right(e)
      case _ => return Either.Left(r)
    }
  }

  def fromTestResource(o: TestResource, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTestResource(o)
    return w.result
  }

  def toTestResource(data: ISZ[U8]): Either[TestResource, MessagePack.ErrorMsg] = {
    def fTestResource(reader: Reader): TestResource = {
      val r = reader.readTestResource()
      return r
    }
    val r = to(data, fTestResource _)
    return r
  }

  def fromTestResult(o: TestResult, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTestResult(o)
    return w.result
  }

  def toTestResult(data: ISZ[U8]): Either[TestResult, MessagePack.ErrorMsg] = {
    def fTestResult(reader: Reader): TestResult = {
      val r = reader.readTestResult()
      return r
    }
    val r = to(data, fTestResult _)
    return r
  }

}