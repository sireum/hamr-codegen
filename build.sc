import $file.runtime.Runtime
import $file.act.Act
import $file.air.Air
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

object air extends Air.Module {
  final override def libraryObject = runtime.library
  final override def testObject = runtime.test
}

object codegen extends Codegen.Module.Codegen {

  final override val millSourcePath = super.millSourcePath / os.up

  object common extends Codegen.Module.Common {
    final override def airObject = air
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

  final override def testObject = runtime.test

  object art extends Art.Module {
    final override def libraryObject = runtime.library
    final override def testObject = runtime.test
  }

}