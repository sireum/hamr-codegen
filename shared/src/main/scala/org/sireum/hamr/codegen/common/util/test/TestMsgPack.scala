// #Sireum
// @formatter:off

// This file is auto-generated from TestResult.scala

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.hamr.codegen.common.util.test.TestResource
import org.sireum.hamr.codegen.common.util.test.TestMarker
import org.sireum.hamr.codegen.common.util.test.TestPlaceholderMarker
import org.sireum.hamr.codegen.common.util.test.TestBlockMarker
import org.sireum.hamr.codegen.common.util.test.ITestResource
import org.sireum.hamr.codegen.common.util.test.ETestResource
import org.sireum.hamr.codegen.common.util.test.TestResult

object TestMsgPack {

  object Constants {

    val TestPlaceholderMarker: Z = -32

    val TestBlockMarker: Z = -31

    val ITestResource: Z = -30

    val ETestResource: Z = -29

    val TestResult: Z = -28

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
      o match {
        case o: TestPlaceholderMarker => writeTestPlaceholderMarker(o)
        case o: TestBlockMarker => writeTestBlockMarker(o)
      }
    }

    def writeTestPlaceholderMarker(o: TestPlaceholderMarker): Unit = {
      writer.writeZ(Constants.TestPlaceholderMarker)
      writer.writeString(o.id)
    }

    def writeTestBlockMarker(o: TestBlockMarker): Unit = {
      writer.writeZ(Constants.TestBlockMarker)
      writer.writeString(o.id)
      writer.writeString(o.beginPrefix)
      writer.writeOption(o.optBeginSuffix, writer.writeString _)
      writer.writeString(o.endPrefix)
      writer.writeOption(o.optEndSuffix, writer.writeString _)
    }

    def writeITestResource(o: ITestResource): Unit = {
      writer.writeZ(Constants.ITestResource)
      writer.writeString(o.content)
      writer.writeISZ(o.markers, writeTestMarker _)
      writer.writeB(o.invertMarkers)
      writer.writeB(o.overwrite)
      writer.writeB(o.makeExecutable)
      writer.writeB(o.makeCRLF)
      writer.writeB(o.isDatatype)
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
      val i = reader.curr
      val t = reader.readZ()
      t match {
        case Constants.TestPlaceholderMarker => val r = readTestPlaceholderMarkerT(T); return r
        case Constants.TestBlockMarker => val r = readTestBlockMarkerT(T); return r
        case _ =>
          reader.error(i, s"$t is not a valid type of TestMarker.")
          val r = readTestBlockMarkerT(T)
          return r
      }
    }

    def readTestPlaceholderMarker(): TestPlaceholderMarker = {
      val r = readTestPlaceholderMarkerT(F)
      return r
    }

    def readTestPlaceholderMarkerT(typeParsed: B): TestPlaceholderMarker = {
      if (!typeParsed) {
        reader.expectZ(Constants.TestPlaceholderMarker)
      }
      val id = reader.readString()
      return TestPlaceholderMarker(id)
    }

    def readTestBlockMarker(): TestBlockMarker = {
      val r = readTestBlockMarkerT(F)
      return r
    }

    def readTestBlockMarkerT(typeParsed: B): TestBlockMarker = {
      if (!typeParsed) {
        reader.expectZ(Constants.TestBlockMarker)
      }
      val id = reader.readString()
      val beginPrefix = reader.readString()
      val optBeginSuffix = reader.readOption(reader.readString _)
      val endPrefix = reader.readString()
      val optEndSuffix = reader.readOption(reader.readString _)
      return TestBlockMarker(id, beginPrefix, optBeginSuffix, endPrefix, optEndSuffix)
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
      val markers = reader.readISZ(readTestMarker _)
      val invertMarkers = reader.readB()
      val overwrite = reader.readB()
      val makeExecutable = reader.readB()
      val makeCRLF = reader.readB()
      val isDatatype = reader.readB()
      return ITestResource(content, markers, invertMarkers, overwrite, makeExecutable, makeCRLF, isDatatype)
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

  def fromTestPlaceholderMarker(o: TestPlaceholderMarker, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTestPlaceholderMarker(o)
    return w.result
  }

  def toTestPlaceholderMarker(data: ISZ[U8]): Either[TestPlaceholderMarker, MessagePack.ErrorMsg] = {
    def fTestPlaceholderMarker(reader: Reader): TestPlaceholderMarker = {
      val r = reader.readTestPlaceholderMarker()
      return r
    }
    val r = to(data, fTestPlaceholderMarker _)
    return r
  }

  def fromTestBlockMarker(o: TestBlockMarker, pooling: B): ISZ[U8] = {
    val w = Writer.Default(MessagePack.writer(pooling))
    w.writeTestBlockMarker(o)
    return w.result
  }

  def toTestBlockMarker(data: ISZ[U8]): Either[TestBlockMarker, MessagePack.ErrorMsg] = {
    def fTestBlockMarker(reader: Reader): TestBlockMarker = {
      val r = reader.readTestBlockMarker()
      return r
    }
    val r = to(data, fTestBlockMarker _)
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