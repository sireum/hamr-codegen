import $file.runtime.Runtime
import $file.act.Act
import $file.air.Air
import $file.slang.Slang
import $file.parser.Parser
import $file.sysml.SysML
import $file.arsit.Arsit
import $file.art.Art
import $file.Codegen

object runtime extends mill.Module {

  object macros extends Runtime.Module.Macros

  object test extends Runtime.Module.Test {
    override def macrosObject = macros
  }

  trait testProvider extends Runtime.Module.TestProvider {
    override def testObject = test
  }

  object library extends Runtime.Module.Library with testProvider
}

object slang extends mill.Module {

  object ast extends Slang.Module.Ast with runtime.testProvider {
    final override def libraryObject = runtime.library
  }

  object parser extends Slang.Module.Parser with runtime.testProvider {
    final override def astObject = ast
  }

  object tipe extends Slang.Module.Tipe with runtime.testProvider {
    final override def astObject = ast
    final override def testObject = runtime.test
  }

  object frontend extends Slang.Module.FrontEnd with runtime.testProvider {
    final override def parserObject = parser
    final override def tipeObject = tipe
  }

}

object air extends Air.Module {
  final override def libraryObject = runtime.library
  final override def testObject = runtime.test
  final override def slangTipeObject = slang.tipe
}

object parser extends Parser.Module {
  final override def libraryObject = runtime.library

  final override def testObject = runtime.test
}

def topParser = parser

object sysml extends mill.Module {
  object parser extends SysML.ParserModule {
    final override def airObject = air
    final override def parserObject = topParser
  }
}

object codegen extends Codegen.Module.Codegen {

  final override val millSourcePath = super.millSourcePath / os.up

  object common extends Codegen.Module.Common {
    final override def airObject = air
    final override def slangFrontendObject = slang.frontend
    final override def sysmlParserObject = sysml.parser
  }

  object act extends Act.Module {
    final override def airObject = air

    final override def commonObject = common
  }

  object arsit extends Arsit.Module {
    final override def airObject = air

    final override def commonObject = common
  }

  final override def actObject = act
  final override def airObject = air
  final override def arsitObject = arsit
  final override def sysmlParserObject = sysml.parser

  final override def testObject = runtime.test

  object art extends Art.Module {
    final override def libraryObject = runtime.library
    final override def testObject = runtime.test
  }

}