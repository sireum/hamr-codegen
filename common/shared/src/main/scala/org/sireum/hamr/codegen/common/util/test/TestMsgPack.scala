// #Sireum
// @formatter:off

// This file is auto-generated from TestResult.scala

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

object TestMsgPack {

  object Constants {

    val TestMarker: Z = -32

    val ITestResource: Z = -31

    val ETestResource: Z = -30

    val TestResult: Z = -29

  }

  object Writer {

    @record class Default(val writer: MessagePack.Writer.Impl) extends Writer

  }

  @msig trait Writer {

    def writer: MessagePack.Writer

    def writeTestResource(o: TestResource): Unit = {
      o match {
        case o: ITestResource => writeITestResource(o)
        case o: ETestResource => writeETestResource(o)
      }
    }

    def writeTestMarker(o: TestMarker): Unit = {
      writer.writeZ(Constants.TestMarker)
      writer.writeString(o.beginMarker)
      writer.writeString(o.endMarker)
    }

    def writeITestResource(o: ITestResource): Unit = {
      writer.writeZ(Constants.ITestResource)
      writer.writeString(o.content)
      writer.writeB(o.overwrite)
      writer.writeB(o.makeExecutable)
      writer.writeB(o.makeCRLF)
      writer.writeISZ(o.markers, writeTestMarker _)
    }

    def writeETestResource(o: ETestResource): Unit = {
      writer.writeZ(Constants.ETestResource)
      writer.writeString(o.content)
      writer.writeB(o.symlink)
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
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.ITestResource => val r = readITestResourceT(T); return r
        case Constants.ETestResource => val r = readETestResourceT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of TestResource.")
          val r = readETestResourceT(T)
          return r
      }
    }

    def readTestMarker(): TestMarker = {
      val r = readTestMarkerT(F)
      return r
    }

    def readTestMarkerT(typeParsed: B): TestMarker = {
      if (!typeParsed) {
        reader.expectZ(Constants.TestMarker)
      }
      val beginMarker = reader.readString()
      val endMarker = reader.readString()
      return TestMarker(beginMarker, endMarker)
    }

    def readITestResource(): ITestResource = {
      val r = readITestResourceT(F)
      return r
    }

    def readITestResourceT(typeParsed: B): ITestResource = {
      if (!typeParsed) {
        reader.expectZ(Constants.ITestResource)
      }
      val content = reader.readString()
      val overwrite = reader.readB()
      val makeExecutable = reader.readB()
      val makeCRLF = reader.readB()
      val markers = reader.readISZ(readTestMarker _)
      return ITestResource(content, overwrite, makeExecutable, makeCRLF, markers)
    }

    def readETestResource(): ETestResource = {
      val r = readETestResourceT(F)
      return r
    }

    def readETestResourceT(typeParsed: B): ETestResource = {
      if (!typeParsed) {
        reader.expectZ(Constants.ETestResource)
      }
      val content = reader.readString()
      val symlink = reader.readB()
      return ETestResource(content, symlink)
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

  def fromTestMarker(o: TestMarker, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTestMarker(o)
    return w.result
  }

  def toTestMarker(data: ISZ[U8]): Either[TestMarker, MessagePack.ErrorMsg] = {
    def fTestMarker(reader: Reader): TestMarker = {
      val r = reader.readTestMarker()
      return r
    }
    val r = to(data, fTestMarker _)
    return r
  }

  def fromITestResource(o: ITestResource, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeITestResource(o)
    return w.result
  }

  def toITestResource(data: ISZ[U8]): Either[ITestResource, MessagePack.ErrorMsg] = {
    def fITestResource(reader: Reader): ITestResource = {
      val r = reader.readITestResource()
      return r
    }
    val r = to(data, fITestResource _)
    return r
  }

  def fromETestResource(o: ETestResource, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeETestResource(o)
    return w.result
  }

  def toETestResource(data: ISZ[U8]): Either[ETestResource, MessagePack.ErrorMsg] = {
    def fETestResource(reader: Reader): ETestResource = {
      val r = reader.readETestResource()
      return r
    }
    val r = to(data, fETestResource _)
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