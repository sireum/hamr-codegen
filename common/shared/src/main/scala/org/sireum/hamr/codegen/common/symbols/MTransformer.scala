// #Sireum
// @formatter:off

/*
 Copyright (c) 2021, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This file is auto-generated from AadlSymbols.scala, BTSSymbols.scala

package org.sireum.hamr.codegen.common.symbols

import org.sireum._

object MTransformer {

  @record class PreResult[T](continu: B,
                             resultOpt: MOption[T])

  val PreResultBTSState: PreResult[BTSState] = PreResult(T, MNone())

  val PostResultBTSState: MOption[BTSState] = MNone()

  val PreResultBTSVariable: PreResult[BTSVariable] = PreResult(T, MNone())

  val PostResultBTSVariable: MOption[BTSVariable] = MNone()

  val PreResultBTSExpKey: PreResult[BTSExpKey] = PreResult(T, MNone())

  val PostResultBTSExpKey: MOption[BTSExpKey] = MNone()

  val PreResultBTSSymbolTable: PreResult[BTSSymbolTable] = PreResult(T, MNone())

  val PostResultBTSSymbolTable: MOption[BTSSymbolTable] = MNone()

  def transformISZ[T](s: IS[Z, T], f: T => MOption[T]): MOption[IS[Z, T]] = {
    val s2: MS[Z, T] = s.toMS
    var changed: B = F
    for (i <- s2.indices) {
      val e: T = s(i)
      val r: MOption[T] = f(e)
      changed = changed || r.nonEmpty
      s2(i) = r.getOrElse(e)
    }
    if (changed) {
      return MSome(s2.toIS)
    } else {
      return MNone()
    }
  }

  val PreResultAadlSystem: PreResult[AadlSystem] = PreResult(T, MNone())

  val PostResultAadlSystem: MOption[AadlSystem] = MNone()

  val PreResultAadlProcessor: PreResult[AadlProcessor] = PreResult(T, MNone())

  val PostResultAadlProcessor: MOption[AadlProcessor] = MNone()

  val PreResultAadlVirtualProcessor: PreResult[AadlVirtualProcessor] = PreResult(T, MNone())

  val PostResultAadlVirtualProcessor: MOption[AadlVirtualProcessor] = MNone()

  val PreResultAadlProcess: PreResult[AadlProcess] = PreResult(T, MNone())

  val PostResultAadlProcess: MOption[AadlProcess] = MNone()

  val PreResultAadlThreadGroup: PreResult[AadlThreadGroup] = PreResult(T, MNone())

  val PostResultAadlThreadGroup: MOption[AadlThreadGroup] = MNone()

  val PreResultAadlThread: PreResult[AadlThread] = PreResult(T, MNone())

  val PostResultAadlThread: MOption[AadlThread] = MNone()

  val PreResultAadlDevice: PreResult[AadlDevice] = PreResult(T, MNone())

  val PostResultAadlDevice: MOption[AadlDevice] = MNone()

  val PreResultAadlSubprogram: PreResult[AadlSubprogram] = PreResult(T, MNone())

  val PostResultAadlSubprogram: MOption[AadlSubprogram] = MNone()

  val PreResultAadlTODOComponent: PreResult[AadlTODOComponent] = PreResult(T, MNone())

  val PostResultAadlTODOComponent: MOption[AadlTODOComponent] = MNone()

  val PreResultAadlEventPort: PreResult[AadlEventPort] = PreResult(T, MNone())

  val PostResultAadlEventPort: MOption[AadlEventPort] = MNone()

  val PreResultAadlEventDataPort: PreResult[AadlEventDataPort] = PreResult(T, MNone())

  val PostResultAadlEventDataPort: MOption[AadlEventDataPort] = MNone()

  val PreResultAadlDataPort: PreResult[AadlDataPort] = PreResult(T, MNone())

  val PostResultAadlDataPort: MOption[AadlDataPort] = MNone()

  val PreResultAadlParameter: PreResult[AadlParameter] = PreResult(T, MNone())

  val PostResultAadlParameter: MOption[AadlParameter] = MNone()

  val PreResultAadlFeatureTODO: PreResult[AadlFeatureTODO] = PreResult(T, MNone())

  val PostResultAadlFeatureTODO: MOption[AadlFeatureTODO] = MNone()

  val PreResultAadlPortConnection: PreResult[AadlPortConnection] = PreResult(T, MNone())

  val PostResultAadlPortConnection: MOption[AadlPortConnection] = MNone()

  val PreResultAadlConnectionTODO: PreResult[AadlConnectionTODO] = PreResult(T, MNone())

  val PostResultAadlConnectionTODO: MOption[AadlConnectionTODO] = MNone()

  val PreResultBTSAnnexInfo: PreResult[BTSAnnexInfo] = PreResult(T, MNone())

  val PostResultBTSAnnexInfo: MOption[BTSAnnexInfo] = MNone()

  val PreResultTodoAnnexInfo: PreResult[TodoAnnexInfo] = PreResult(T, MNone())

  val PostResultTodoAnnexInfo: MOption[TodoAnnexInfo] = MNone()

}

import MTransformer._

@msig trait MTransformer {

  def preBTSSymbol(o: BTSSymbol): PreResult[BTSSymbol] = {
    o match {
      case o: BTSState =>
        val r: PreResult[BTSSymbol] = preBTSState(o) match {
         case PreResult(continu, MSome(r: BTSSymbol)) => PreResult(continu, MSome[BTSSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type BTSSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[BTSSymbol]())
        }
        return r
      case o: BTSVariable =>
        val r: PreResult[BTSSymbol] = preBTSVariable(o) match {
         case PreResult(continu, MSome(r: BTSSymbol)) => PreResult(continu, MSome[BTSSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type BTSSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[BTSSymbol]())
        }
        return r
    }
  }

  def preBTSState(o: BTSState): PreResult[BTSState] = {
    return PreResultBTSState
  }

  def preBTSVariable(o: BTSVariable): PreResult[BTSVariable] = {
    return PreResultBTSVariable
  }

  def preBTSKey(o: BTSKey): PreResult[BTSKey] = {
    o match {
      case o: BTSExpKey =>
        val r: PreResult[BTSKey] = preBTSExpKey(o) match {
         case PreResult(continu, MSome(r: BTSKey)) => PreResult(continu, MSome[BTSKey](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type BTSKey")
         case PreResult(continu, _) => PreResult(continu, MNone[BTSKey]())
        }
        return r
    }
  }

  def preBTSExpKey(o: BTSExpKey): PreResult[BTSExpKey] = {
    return PreResultBTSExpKey
  }

  def preBTSSymbolTable(o: BTSSymbolTable): PreResult[BTSSymbolTable] = {
    return PreResultBTSSymbolTable
  }

  def preAadlSymbol(o: AadlSymbol): PreResult[AadlSymbol] = {
    o match {
      case o: BTSState =>
        val r: PreResult[AadlSymbol] = preBTSState(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: BTSVariable =>
        val r: PreResult[AadlSymbol] = preBTSVariable(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlSystem =>
        val r: PreResult[AadlSymbol] = preAadlSystem(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlProcessor =>
        val r: PreResult[AadlSymbol] = preAadlProcessor(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: PreResult[AadlSymbol] = preAadlVirtualProcessor(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlProcess =>
        val r: PreResult[AadlSymbol] = preAadlProcess(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlThreadGroup =>
        val r: PreResult[AadlSymbol] = preAadlThreadGroup(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlThread =>
        val r: PreResult[AadlSymbol] = preAadlThread(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlDevice =>
        val r: PreResult[AadlSymbol] = preAadlDevice(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlSubprogram =>
        val r: PreResult[AadlSymbol] = preAadlSubprogram(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlTODOComponent =>
        val r: PreResult[AadlSymbol] = preAadlTODOComponent(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlEventPort =>
        val r: PreResult[AadlSymbol] = preAadlEventPort(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlEventDataPort =>
        val r: PreResult[AadlSymbol] = preAadlEventDataPort(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlDataPort =>
        val r: PreResult[AadlSymbol] = preAadlDataPort(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlParameter =>
        val r: PreResult[AadlSymbol] = preAadlParameter(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlFeatureTODO =>
        val r: PreResult[AadlSymbol] = preAadlFeatureTODO(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlPortConnection =>
        val r: PreResult[AadlSymbol] = preAadlPortConnection(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
      case o: AadlConnectionTODO =>
        val r: PreResult[AadlSymbol] = preAadlConnectionTODO(o) match {
         case PreResult(continu, MSome(r: AadlSymbol)) => PreResult(continu, MSome[AadlSymbol](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlSymbol")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlSymbol]())
        }
        return r
    }
  }

  def preAadlComponent(o: AadlComponent): PreResult[AadlComponent] = {
    o match {
      case o: AadlSystem =>
        val r: PreResult[AadlComponent] = preAadlSystem(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlProcessor =>
        val r: PreResult[AadlComponent] = preAadlProcessor(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: PreResult[AadlComponent] = preAadlVirtualProcessor(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlProcess =>
        val r: PreResult[AadlComponent] = preAadlProcess(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlThreadGroup =>
        val r: PreResult[AadlComponent] = preAadlThreadGroup(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlThread =>
        val r: PreResult[AadlComponent] = preAadlThread(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlDevice =>
        val r: PreResult[AadlComponent] = preAadlDevice(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlSubprogram =>
        val r: PreResult[AadlComponent] = preAadlSubprogram(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
      case o: AadlTODOComponent =>
        val r: PreResult[AadlComponent] = preAadlTODOComponent(o) match {
         case PreResult(continu, MSome(r: AadlComponent)) => PreResult(continu, MSome[AadlComponent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlComponent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlComponent]())
        }
        return r
    }
  }

  def preAadlSystem(o: AadlSystem): PreResult[AadlSystem] = {
    return PreResultAadlSystem
  }

  def preProcessor(o: Processor): PreResult[Processor] = {
    o match {
      case o: AadlProcessor =>
        val r: PreResult[Processor] = preAadlProcessor(o) match {
         case PreResult(continu, MSome(r: Processor)) => PreResult(continu, MSome[Processor](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type Processor")
         case PreResult(continu, _) => PreResult(continu, MNone[Processor]())
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: PreResult[Processor] = preAadlVirtualProcessor(o) match {
         case PreResult(continu, MSome(r: Processor)) => PreResult(continu, MSome[Processor](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type Processor")
         case PreResult(continu, _) => PreResult(continu, MNone[Processor]())
        }
        return r
    }
  }

  def preAadlProcessor(o: AadlProcessor): PreResult[AadlProcessor] = {
    return PreResultAadlProcessor
  }

  def preAadlVirtualProcessor(o: AadlVirtualProcessor): PreResult[AadlVirtualProcessor] = {
    return PreResultAadlVirtualProcessor
  }

  def preAadlProcess(o: AadlProcess): PreResult[AadlProcess] = {
    return PreResultAadlProcess
  }

  def preAadlThreadGroup(o: AadlThreadGroup): PreResult[AadlThreadGroup] = {
    return PreResultAadlThreadGroup
  }

  def preAadlThreadOrDevice(o: AadlThreadOrDevice): PreResult[AadlThreadOrDevice] = {
    o match {
      case o: AadlThread =>
        val r: PreResult[AadlThreadOrDevice] = preAadlThread(o) match {
         case PreResult(continu, MSome(r: AadlThreadOrDevice)) => PreResult(continu, MSome[AadlThreadOrDevice](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlThreadOrDevice")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlThreadOrDevice]())
        }
        return r
      case o: AadlDevice =>
        val r: PreResult[AadlThreadOrDevice] = preAadlDevice(o) match {
         case PreResult(continu, MSome(r: AadlThreadOrDevice)) => PreResult(continu, MSome[AadlThreadOrDevice](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlThreadOrDevice")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlThreadOrDevice]())
        }
        return r
    }
  }

  def preAadlThread(o: AadlThread): PreResult[AadlThread] = {
    return PreResultAadlThread
  }

  def preAadlDevice(o: AadlDevice): PreResult[AadlDevice] = {
    return PreResultAadlDevice
  }

  def preAadlSubprogram(o: AadlSubprogram): PreResult[AadlSubprogram] = {
    return PreResultAadlSubprogram
  }

  def preAadlTODOComponent(o: AadlTODOComponent): PreResult[AadlTODOComponent] = {
    return PreResultAadlTODOComponent
  }

  def preAadlFeature(o: AadlFeature): PreResult[AadlFeature] = {
    o match {
      case o: AadlEventPort =>
        val r: PreResult[AadlFeature] = preAadlEventPort(o) match {
         case PreResult(continu, MSome(r: AadlFeature)) => PreResult(continu, MSome[AadlFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeature]())
        }
        return r
      case o: AadlEventDataPort =>
        val r: PreResult[AadlFeature] = preAadlEventDataPort(o) match {
         case PreResult(continu, MSome(r: AadlFeature)) => PreResult(continu, MSome[AadlFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeature]())
        }
        return r
      case o: AadlDataPort =>
        val r: PreResult[AadlFeature] = preAadlDataPort(o) match {
         case PreResult(continu, MSome(r: AadlFeature)) => PreResult(continu, MSome[AadlFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeature]())
        }
        return r
      case o: AadlParameter =>
        val r: PreResult[AadlFeature] = preAadlParameter(o) match {
         case PreResult(continu, MSome(r: AadlFeature)) => PreResult(continu, MSome[AadlFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeature]())
        }
        return r
      case o: AadlFeatureTODO =>
        val r: PreResult[AadlFeature] = preAadlFeatureTODO(o) match {
         case PreResult(continu, MSome(r: AadlFeature)) => PreResult(continu, MSome[AadlFeature](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeature")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeature]())
        }
        return r
    }
  }

  def preAadlFeatureEvent(o: AadlFeatureEvent): PreResult[AadlFeatureEvent] = {
    o match {
      case o: AadlEventPort =>
        val r: PreResult[AadlFeatureEvent] = preAadlEventPort(o) match {
         case PreResult(continu, MSome(r: AadlFeatureEvent)) => PreResult(continu, MSome[AadlFeatureEvent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeatureEvent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeatureEvent]())
        }
        return r
      case o: AadlEventDataPort =>
        val r: PreResult[AadlFeatureEvent] = preAadlEventDataPort(o) match {
         case PreResult(continu, MSome(r: AadlFeatureEvent)) => PreResult(continu, MSome[AadlFeatureEvent](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeatureEvent")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeatureEvent]())
        }
        return r
    }
  }

  def preAadlFeatureData(o: AadlFeatureData): PreResult[AadlFeatureData] = {
    o match {
      case o: AadlEventDataPort =>
        val r: PreResult[AadlFeatureData] = preAadlEventDataPort(o) match {
         case PreResult(continu, MSome(r: AadlFeatureData)) => PreResult(continu, MSome[AadlFeatureData](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeatureData")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeatureData]())
        }
        return r
      case o: AadlDataPort =>
        val r: PreResult[AadlFeatureData] = preAadlDataPort(o) match {
         case PreResult(continu, MSome(r: AadlFeatureData)) => PreResult(continu, MSome[AadlFeatureData](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeatureData")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeatureData]())
        }
        return r
      case o: AadlParameter =>
        val r: PreResult[AadlFeatureData] = preAadlParameter(o) match {
         case PreResult(continu, MSome(r: AadlFeatureData)) => PreResult(continu, MSome[AadlFeatureData](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlFeatureData")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlFeatureData]())
        }
        return r
    }
  }

  def preAadlEventPort(o: AadlEventPort): PreResult[AadlEventPort] = {
    return PreResultAadlEventPort
  }

  def preAadlEventDataPort(o: AadlEventDataPort): PreResult[AadlEventDataPort] = {
    return PreResultAadlEventDataPort
  }

  def preAadlDataPort(o: AadlDataPort): PreResult[AadlDataPort] = {
    return PreResultAadlDataPort
  }

  def preAadlParameter(o: AadlParameter): PreResult[AadlParameter] = {
    return PreResultAadlParameter
  }

  def preAadlFeatureTODO(o: AadlFeatureTODO): PreResult[AadlFeatureTODO] = {
    return PreResultAadlFeatureTODO
  }

  def preAadlConnection(o: AadlConnection): PreResult[AadlConnection] = {
    o match {
      case o: AadlPortConnection =>
        val r: PreResult[AadlConnection] = preAadlPortConnection(o) match {
         case PreResult(continu, MSome(r: AadlConnection)) => PreResult(continu, MSome[AadlConnection](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlConnection")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlConnection]())
        }
        return r
      case o: AadlConnectionTODO =>
        val r: PreResult[AadlConnection] = preAadlConnectionTODO(o) match {
         case PreResult(continu, MSome(r: AadlConnection)) => PreResult(continu, MSome[AadlConnection](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AadlConnection")
         case PreResult(continu, _) => PreResult(continu, MNone[AadlConnection]())
        }
        return r
    }
  }

  def preAadlPortConnection(o: AadlPortConnection): PreResult[AadlPortConnection] = {
    return PreResultAadlPortConnection
  }

  def preAadlConnectionTODO(o: AadlConnectionTODO): PreResult[AadlConnectionTODO] = {
    return PreResultAadlConnectionTODO
  }

  def preAnnexInfo(o: AnnexInfo): PreResult[AnnexInfo] = {
    o match {
      case o: BTSAnnexInfo =>
        val r: PreResult[AnnexInfo] = preBTSAnnexInfo(o) match {
         case PreResult(continu, MSome(r: AnnexInfo)) => PreResult(continu, MSome[AnnexInfo](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AnnexInfo")
         case PreResult(continu, _) => PreResult(continu, MNone[AnnexInfo]())
        }
        return r
      case o: TodoAnnexInfo =>
        val r: PreResult[AnnexInfo] = preTodoAnnexInfo(o) match {
         case PreResult(continu, MSome(r: AnnexInfo)) => PreResult(continu, MSome[AnnexInfo](r))
         case PreResult(_, MSome(_)) => halt("Can only produce object of type AnnexInfo")
         case PreResult(continu, _) => PreResult(continu, MNone[AnnexInfo]())
        }
        return r
    }
  }

  def preBTSAnnexInfo(o: BTSAnnexInfo): PreResult[BTSAnnexInfo] = {
    return PreResultBTSAnnexInfo
  }

  def preTodoAnnexInfo(o: TodoAnnexInfo): PreResult[TodoAnnexInfo] = {
    return PreResultTodoAnnexInfo
  }

  def postBTSSymbol(o: BTSSymbol): MOption[BTSSymbol] = {
    o match {
      case o: BTSState =>
        val r: MOption[BTSSymbol] = postBTSState(o) match {
         case MSome(result: BTSSymbol) => MSome[BTSSymbol](result)
         case MSome(_) => halt("Can only produce object of type BTSSymbol")
         case _ => MNone[BTSSymbol]()
        }
        return r
      case o: BTSVariable =>
        val r: MOption[BTSSymbol] = postBTSVariable(o) match {
         case MSome(result: BTSSymbol) => MSome[BTSSymbol](result)
         case MSome(_) => halt("Can only produce object of type BTSSymbol")
         case _ => MNone[BTSSymbol]()
        }
        return r
    }
  }

  def postBTSState(o: BTSState): MOption[BTSState] = {
    return PostResultBTSState
  }

  def postBTSVariable(o: BTSVariable): MOption[BTSVariable] = {
    return PostResultBTSVariable
  }

  def postBTSKey(o: BTSKey): MOption[BTSKey] = {
    o match {
      case o: BTSExpKey =>
        val r: MOption[BTSKey] = postBTSExpKey(o) match {
         case MSome(result: BTSKey) => MSome[BTSKey](result)
         case MSome(_) => halt("Can only produce object of type BTSKey")
         case _ => MNone[BTSKey]()
        }
        return r
    }
  }

  def postBTSExpKey(o: BTSExpKey): MOption[BTSExpKey] = {
    return PostResultBTSExpKey
  }

  def postBTSSymbolTable(o: BTSSymbolTable): MOption[BTSSymbolTable] = {
    return PostResultBTSSymbolTable
  }

  def postAadlSymbol(o: AadlSymbol): MOption[AadlSymbol] = {
    o match {
      case o: BTSState =>
        val r: MOption[AadlSymbol] = postBTSState(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: BTSVariable =>
        val r: MOption[AadlSymbol] = postBTSVariable(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlSystem =>
        val r: MOption[AadlSymbol] = postAadlSystem(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlProcessor =>
        val r: MOption[AadlSymbol] = postAadlProcessor(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: MOption[AadlSymbol] = postAadlVirtualProcessor(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlProcess =>
        val r: MOption[AadlSymbol] = postAadlProcess(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlThreadGroup =>
        val r: MOption[AadlSymbol] = postAadlThreadGroup(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlThread =>
        val r: MOption[AadlSymbol] = postAadlThread(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlDevice =>
        val r: MOption[AadlSymbol] = postAadlDevice(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlSubprogram =>
        val r: MOption[AadlSymbol] = postAadlSubprogram(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlTODOComponent =>
        val r: MOption[AadlSymbol] = postAadlTODOComponent(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlEventPort =>
        val r: MOption[AadlSymbol] = postAadlEventPort(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlEventDataPort =>
        val r: MOption[AadlSymbol] = postAadlEventDataPort(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlDataPort =>
        val r: MOption[AadlSymbol] = postAadlDataPort(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlParameter =>
        val r: MOption[AadlSymbol] = postAadlParameter(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlFeatureTODO =>
        val r: MOption[AadlSymbol] = postAadlFeatureTODO(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlPortConnection =>
        val r: MOption[AadlSymbol] = postAadlPortConnection(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
      case o: AadlConnectionTODO =>
        val r: MOption[AadlSymbol] = postAadlConnectionTODO(o) match {
         case MSome(result: AadlSymbol) => MSome[AadlSymbol](result)
         case MSome(_) => halt("Can only produce object of type AadlSymbol")
         case _ => MNone[AadlSymbol]()
        }
        return r
    }
  }

  def postAadlComponent(o: AadlComponent): MOption[AadlComponent] = {
    o match {
      case o: AadlSystem =>
        val r: MOption[AadlComponent] = postAadlSystem(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlProcessor =>
        val r: MOption[AadlComponent] = postAadlProcessor(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: MOption[AadlComponent] = postAadlVirtualProcessor(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlProcess =>
        val r: MOption[AadlComponent] = postAadlProcess(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlThreadGroup =>
        val r: MOption[AadlComponent] = postAadlThreadGroup(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlThread =>
        val r: MOption[AadlComponent] = postAadlThread(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlDevice =>
        val r: MOption[AadlComponent] = postAadlDevice(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlSubprogram =>
        val r: MOption[AadlComponent] = postAadlSubprogram(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
      case o: AadlTODOComponent =>
        val r: MOption[AadlComponent] = postAadlTODOComponent(o) match {
         case MSome(result: AadlComponent) => MSome[AadlComponent](result)
         case MSome(_) => halt("Can only produce object of type AadlComponent")
         case _ => MNone[AadlComponent]()
        }
        return r
    }
  }

  def postAadlSystem(o: AadlSystem): MOption[AadlSystem] = {
    return PostResultAadlSystem
  }

  def postProcessor(o: Processor): MOption[Processor] = {
    o match {
      case o: AadlProcessor =>
        val r: MOption[Processor] = postAadlProcessor(o) match {
         case MSome(result: Processor) => MSome[Processor](result)
         case MSome(_) => halt("Can only produce object of type Processor")
         case _ => MNone[Processor]()
        }
        return r
      case o: AadlVirtualProcessor =>
        val r: MOption[Processor] = postAadlVirtualProcessor(o) match {
         case MSome(result: Processor) => MSome[Processor](result)
         case MSome(_) => halt("Can only produce object of type Processor")
         case _ => MNone[Processor]()
        }
        return r
    }
  }

  def postAadlProcessor(o: AadlProcessor): MOption[AadlProcessor] = {
    return PostResultAadlProcessor
  }

  def postAadlVirtualProcessor(o: AadlVirtualProcessor): MOption[AadlVirtualProcessor] = {
    return PostResultAadlVirtualProcessor
  }

  def postAadlProcess(o: AadlProcess): MOption[AadlProcess] = {
    return PostResultAadlProcess
  }

  def postAadlThreadGroup(o: AadlThreadGroup): MOption[AadlThreadGroup] = {
    return PostResultAadlThreadGroup
  }

  def postAadlThreadOrDevice(o: AadlThreadOrDevice): MOption[AadlThreadOrDevice] = {
    o match {
      case o: AadlThread =>
        val r: MOption[AadlThreadOrDevice] = postAadlThread(o) match {
         case MSome(result: AadlThreadOrDevice) => MSome[AadlThreadOrDevice](result)
         case MSome(_) => halt("Can only produce object of type AadlThreadOrDevice")
         case _ => MNone[AadlThreadOrDevice]()
        }
        return r
      case o: AadlDevice =>
        val r: MOption[AadlThreadOrDevice] = postAadlDevice(o) match {
         case MSome(result: AadlThreadOrDevice) => MSome[AadlThreadOrDevice](result)
         case MSome(_) => halt("Can only produce object of type AadlThreadOrDevice")
         case _ => MNone[AadlThreadOrDevice]()
        }
        return r
    }
  }

  def postAadlThread(o: AadlThread): MOption[AadlThread] = {
    return PostResultAadlThread
  }

  def postAadlDevice(o: AadlDevice): MOption[AadlDevice] = {
    return PostResultAadlDevice
  }

  def postAadlSubprogram(o: AadlSubprogram): MOption[AadlSubprogram] = {
    return PostResultAadlSubprogram
  }

  def postAadlTODOComponent(o: AadlTODOComponent): MOption[AadlTODOComponent] = {
    return PostResultAadlTODOComponent
  }

  def postAadlFeature(o: AadlFeature): MOption[AadlFeature] = {
    o match {
      case o: AadlEventPort =>
        val r: MOption[AadlFeature] = postAadlEventPort(o) match {
         case MSome(result: AadlFeature) => MSome[AadlFeature](result)
         case MSome(_) => halt("Can only produce object of type AadlFeature")
         case _ => MNone[AadlFeature]()
        }
        return r
      case o: AadlEventDataPort =>
        val r: MOption[AadlFeature] = postAadlEventDataPort(o) match {
         case MSome(result: AadlFeature) => MSome[AadlFeature](result)
         case MSome(_) => halt("Can only produce object of type AadlFeature")
         case _ => MNone[AadlFeature]()
        }
        return r
      case o: AadlDataPort =>
        val r: MOption[AadlFeature] = postAadlDataPort(o) match {
         case MSome(result: AadlFeature) => MSome[AadlFeature](result)
         case MSome(_) => halt("Can only produce object of type AadlFeature")
         case _ => MNone[AadlFeature]()
        }
        return r
      case o: AadlParameter =>
        val r: MOption[AadlFeature] = postAadlParameter(o) match {
         case MSome(result: AadlFeature) => MSome[AadlFeature](result)
         case MSome(_) => halt("Can only produce object of type AadlFeature")
         case _ => MNone[AadlFeature]()
        }
        return r
      case o: AadlFeatureTODO =>
        val r: MOption[AadlFeature] = postAadlFeatureTODO(o) match {
         case MSome(result: AadlFeature) => MSome[AadlFeature](result)
         case MSome(_) => halt("Can only produce object of type AadlFeature")
         case _ => MNone[AadlFeature]()
        }
        return r
    }
  }

  def postAadlFeatureEvent(o: AadlFeatureEvent): MOption[AadlFeatureEvent] = {
    o match {
      case o: AadlEventPort =>
        val r: MOption[AadlFeatureEvent] = postAadlEventPort(o) match {
         case MSome(result: AadlFeatureEvent) => MSome[AadlFeatureEvent](result)
         case MSome(_) => halt("Can only produce object of type AadlFeatureEvent")
         case _ => MNone[AadlFeatureEvent]()
        }
        return r
      case o: AadlEventDataPort =>
        val r: MOption[AadlFeatureEvent] = postAadlEventDataPort(o) match {
         case MSome(result: AadlFeatureEvent) => MSome[AadlFeatureEvent](result)
         case MSome(_) => halt("Can only produce object of type AadlFeatureEvent")
         case _ => MNone[AadlFeatureEvent]()
        }
        return r
    }
  }

  def postAadlFeatureData(o: AadlFeatureData): MOption[AadlFeatureData] = {
    o match {
      case o: AadlEventDataPort =>
        val r: MOption[AadlFeatureData] = postAadlEventDataPort(o) match {
         case MSome(result: AadlFeatureData) => MSome[AadlFeatureData](result)
         case MSome(_) => halt("Can only produce object of type AadlFeatureData")
         case _ => MNone[AadlFeatureData]()
        }
        return r
      case o: AadlDataPort =>
        val r: MOption[AadlFeatureData] = postAadlDataPort(o) match {
         case MSome(result: AadlFeatureData) => MSome[AadlFeatureData](result)
         case MSome(_) => halt("Can only produce object of type AadlFeatureData")
         case _ => MNone[AadlFeatureData]()
        }
        return r
      case o: AadlParameter =>
        val r: MOption[AadlFeatureData] = postAadlParameter(o) match {
         case MSome(result: AadlFeatureData) => MSome[AadlFeatureData](result)
         case MSome(_) => halt("Can only produce object of type AadlFeatureData")
         case _ => MNone[AadlFeatureData]()
        }
        return r
    }
  }

  def postAadlEventPort(o: AadlEventPort): MOption[AadlEventPort] = {
    return PostResultAadlEventPort
  }

  def postAadlEventDataPort(o: AadlEventDataPort): MOption[AadlEventDataPort] = {
    return PostResultAadlEventDataPort
  }

  def postAadlDataPort(o: AadlDataPort): MOption[AadlDataPort] = {
    return PostResultAadlDataPort
  }

  def postAadlParameter(o: AadlParameter): MOption[AadlParameter] = {
    return PostResultAadlParameter
  }

  def postAadlFeatureTODO(o: AadlFeatureTODO): MOption[AadlFeatureTODO] = {
    return PostResultAadlFeatureTODO
  }

  def postAadlConnection(o: AadlConnection): MOption[AadlConnection] = {
    o match {
      case o: AadlPortConnection =>
        val r: MOption[AadlConnection] = postAadlPortConnection(o) match {
         case MSome(result: AadlConnection) => MSome[AadlConnection](result)
         case MSome(_) => halt("Can only produce object of type AadlConnection")
         case _ => MNone[AadlConnection]()
        }
        return r
      case o: AadlConnectionTODO =>
        val r: MOption[AadlConnection] = postAadlConnectionTODO(o) match {
         case MSome(result: AadlConnection) => MSome[AadlConnection](result)
         case MSome(_) => halt("Can only produce object of type AadlConnection")
         case _ => MNone[AadlConnection]()
        }
        return r
    }
  }

  def postAadlPortConnection(o: AadlPortConnection): MOption[AadlPortConnection] = {
    return PostResultAadlPortConnection
  }

  def postAadlConnectionTODO(o: AadlConnectionTODO): MOption[AadlConnectionTODO] = {
    return PostResultAadlConnectionTODO
  }

  def postAnnexInfo(o: AnnexInfo): MOption[AnnexInfo] = {
    o match {
      case o: BTSAnnexInfo =>
        val r: MOption[AnnexInfo] = postBTSAnnexInfo(o) match {
         case MSome(result: AnnexInfo) => MSome[AnnexInfo](result)
         case MSome(_) => halt("Can only produce object of type AnnexInfo")
         case _ => MNone[AnnexInfo]()
        }
        return r
      case o: TodoAnnexInfo =>
        val r: MOption[AnnexInfo] = postTodoAnnexInfo(o) match {
         case MSome(result: AnnexInfo) => MSome[AnnexInfo](result)
         case MSome(_) => halt("Can only produce object of type AnnexInfo")
         case _ => MNone[AnnexInfo]()
        }
        return r
    }
  }

  def postBTSAnnexInfo(o: BTSAnnexInfo): MOption[BTSAnnexInfo] = {
    return PostResultBTSAnnexInfo
  }

  def postTodoAnnexInfo(o: TodoAnnexInfo): MOption[TodoAnnexInfo] = {
    return PostResultTodoAnnexInfo
  }

  def transformBTSSymbol(o: BTSSymbol): MOption[BTSSymbol] = {
    val preR: PreResult[BTSSymbol] = preBTSSymbol(o)
    val r: MOption[BTSSymbol] = if (preR.continu) {
      val o2: BTSSymbol = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[BTSSymbol] = o2 match {
        case o2: BTSState =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: BTSVariable =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSSymbol = r.getOrElse(o)
    val postR: MOption[BTSSymbol] = postBTSSymbol(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSState(o: BTSState): MOption[BTSState] = {
    val preR: PreResult[BTSState] = preBTSState(o)
    val r: MOption[BTSState] = if (preR.continu) {
      val o2: BTSState = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSState = r.getOrElse(o)
    val postR: MOption[BTSState] = postBTSState(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSVariable(o: BTSVariable): MOption[BTSVariable] = {
    val preR: PreResult[BTSVariable] = preBTSVariable(o)
    val r: MOption[BTSVariable] = if (preR.continu) {
      val o2: BTSVariable = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSVariable = r.getOrElse(o)
    val postR: MOption[BTSVariable] = postBTSVariable(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSKey(o: BTSKey): MOption[BTSKey] = {
    val preR: PreResult[BTSKey] = preBTSKey(o)
    val r: MOption[BTSKey] = if (preR.continu) {
      val o2: BTSKey = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[BTSKey] = o2 match {
        case o2: BTSExpKey =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSKey = r.getOrElse(o)
    val postR: MOption[BTSKey] = postBTSKey(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSExpKey(o: BTSExpKey): MOption[BTSExpKey] = {
    val preR: PreResult[BTSExpKey] = preBTSExpKey(o)
    val r: MOption[BTSExpKey] = if (preR.continu) {
      val o2: BTSExpKey = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSExpKey = r.getOrElse(o)
    val postR: MOption[BTSExpKey] = postBTSExpKey(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSSymbolTable(o: BTSSymbolTable): MOption[BTSSymbolTable] = {
    val preR: PreResult[BTSSymbolTable] = preBTSSymbolTable(o)
    val r: MOption[BTSSymbolTable] = if (preR.continu) {
      val o2: BTSSymbolTable = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSSymbolTable = r.getOrElse(o)
    val postR: MOption[BTSSymbolTable] = postBTSSymbolTable(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlSymbol(o: AadlSymbol): MOption[AadlSymbol] = {
    val preR: PreResult[AadlSymbol] = preAadlSymbol(o)
    val r: MOption[AadlSymbol] = if (preR.continu) {
      val o2: AadlSymbol = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlSymbol] = o2 match {
        case o2: BTSState =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: BTSVariable =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlSystem =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlVirtualProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlProcess =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlThreadGroup =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlThread =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
        case o2: AadlDevice =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
        case o2: AadlSubprogram =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlParameter]] = transformISZ(o2.parameters, transformAadlParameter _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), parameters = r1.getOrElse(o2.parameters)))
          else
            MNone()
        case o2: AadlTODOComponent =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlEventPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlEventDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlParameter =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlFeatureTODO =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlPortConnection =>
          val r0: MOption[AadlComponent] = transformAadlComponent(o2.srcComponent)
          val r1: MOption[AadlFeature] = transformAadlFeature(o2.srcFeature)
          val r2: MOption[AadlComponent] = transformAadlComponent(o2.dstComponent)
          val r3: MOption[AadlFeature] = transformAadlFeature(o2.dstFeature)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty)
            MSome(o2(srcComponent = r0.getOrElse(o2.srcComponent), srcFeature = r1.getOrElse(o2.srcFeature), dstComponent = r2.getOrElse(o2.dstComponent), dstFeature = r3.getOrElse(o2.dstFeature)))
          else
            MNone()
        case o2: AadlConnectionTODO =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlSymbol = r.getOrElse(o)
    val postR: MOption[AadlSymbol] = postAadlSymbol(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlComponent(o: AadlComponent): MOption[AadlComponent] = {
    val preR: PreResult[AadlComponent] = preAadlComponent(o)
    val r: MOption[AadlComponent] = if (preR.continu) {
      val o2: AadlComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlComponent] = o2 match {
        case o2: AadlSystem =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlVirtualProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlProcess =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlThreadGroup =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlThread =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
        case o2: AadlDevice =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
        case o2: AadlSubprogram =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlParameter]] = transformISZ(o2.parameters, transformAadlParameter _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), parameters = r1.getOrElse(o2.parameters)))
          else
            MNone()
        case o2: AadlTODOComponent =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlComponent = r.getOrElse(o)
    val postR: MOption[AadlComponent] = postAadlComponent(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlSystem(o: AadlSystem): MOption[AadlSystem] = {
    val preR: PreResult[AadlSystem] = preAadlSystem(o)
    val r: MOption[AadlSystem] = if (preR.continu) {
      val o2: AadlSystem = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlSystem = r.getOrElse(o)
    val postR: MOption[AadlSystem] = postAadlSystem(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformProcessor(o: Processor): MOption[Processor] = {
    val preR: PreResult[Processor] = preProcessor(o)
    val r: MOption[Processor] = if (preR.continu) {
      val o2: Processor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[Processor] = o2 match {
        case o2: AadlProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
        case o2: AadlVirtualProcessor =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: Processor = r.getOrElse(o)
    val postR: MOption[Processor] = postProcessor(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlProcessor(o: AadlProcessor): MOption[AadlProcessor] = {
    val preR: PreResult[AadlProcessor] = preAadlProcessor(o)
    val r: MOption[AadlProcessor] = if (preR.continu) {
      val o2: AadlProcessor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlProcessor = r.getOrElse(o)
    val postR: MOption[AadlProcessor] = postAadlProcessor(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlVirtualProcessor(o: AadlVirtualProcessor): MOption[AadlVirtualProcessor] = {
    val preR: PreResult[AadlVirtualProcessor] = preAadlVirtualProcessor(o)
    val r: MOption[AadlVirtualProcessor] = if (preR.continu) {
      val o2: AadlVirtualProcessor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlVirtualProcessor = r.getOrElse(o)
    val postR: MOption[AadlVirtualProcessor] = postAadlVirtualProcessor(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlProcess(o: AadlProcess): MOption[AadlProcess] = {
    val preR: PreResult[AadlProcess] = preAadlProcess(o)
    val r: MOption[AadlProcess] = if (preR.continu) {
      val o2: AadlProcess = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlProcess = r.getOrElse(o)
    val postR: MOption[AadlProcess] = postAadlProcess(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlThreadGroup(o: AadlThreadGroup): MOption[AadlThreadGroup] = {
    val preR: PreResult[AadlThreadGroup] = preAadlThreadGroup(o)
    val r: MOption[AadlThreadGroup] = if (preR.continu) {
      val o2: AadlThreadGroup = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlThreadGroup = r.getOrElse(o)
    val postR: MOption[AadlThreadGroup] = postAadlThreadGroup(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlThreadOrDevice(o: AadlThreadOrDevice): MOption[AadlThreadOrDevice] = {
    val preR: PreResult[AadlThreadOrDevice] = preAadlThreadOrDevice(o)
    val r: MOption[AadlThreadOrDevice] = if (preR.continu) {
      val o2: AadlThreadOrDevice = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlThreadOrDevice] = o2 match {
        case o2: AadlThread =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
        case o2: AadlDevice =>
          val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
          val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty)
            MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlThreadOrDevice = r.getOrElse(o)
    val postR: MOption[AadlThreadOrDevice] = postAadlThreadOrDevice(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlThread(o: AadlThread): MOption[AadlThread] = {
    val preR: PreResult[AadlThread] = preAadlThread(o)
    val r: MOption[AadlThread] = if (preR.continu) {
      val o2: AadlThread = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlThread = r.getOrElse(o)
    val postR: MOption[AadlThread] = postAadlThread(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlDevice(o: AadlDevice): MOption[AadlDevice] = {
    val preR: PreResult[AadlDevice] = preAadlDevice(o)
    val r: MOption[AadlDevice] = if (preR.continu) {
      val o2: AadlDevice = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      val r1: MOption[IS[Z, AadlFeature]] = transformISZ(o2.ports, transformAadlFeature _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents), ports = r1.getOrElse(o2.ports)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlDevice = r.getOrElse(o)
    val postR: MOption[AadlDevice] = postAadlDevice(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlSubprogram(o: AadlSubprogram): MOption[AadlSubprogram] = {
    val preR: PreResult[AadlSubprogram] = preAadlSubprogram(o)
    val r: MOption[AadlSubprogram] = if (preR.continu) {
      val o2: AadlSubprogram = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      val r1: MOption[IS[Z, AadlParameter]] = transformISZ(o2.parameters, transformAadlParameter _)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents), parameters = r1.getOrElse(o2.parameters)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlSubprogram = r.getOrElse(o)
    val postR: MOption[AadlSubprogram] = postAadlSubprogram(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlTODOComponent(o: AadlTODOComponent): MOption[AadlTODOComponent] = {
    val preR: PreResult[AadlTODOComponent] = preAadlTODOComponent(o)
    val r: MOption[AadlTODOComponent] = if (preR.continu) {
      val o2: AadlTODOComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[IS[Z, AadlComponent]] = transformISZ(o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(subComponents = r0.getOrElse(o2.subComponents)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlTODOComponent = r.getOrElse(o)
    val postR: MOption[AadlTODOComponent] = postAadlTODOComponent(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlFeature(o: AadlFeature): MOption[AadlFeature] = {
    val preR: PreResult[AadlFeature] = preAadlFeature(o)
    val r: MOption[AadlFeature] = if (preR.continu) {
      val o2: AadlFeature = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlFeature] = o2 match {
        case o2: AadlEventPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlEventDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlParameter =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlFeatureTODO =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlFeature = r.getOrElse(o)
    val postR: MOption[AadlFeature] = postAadlFeature(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlFeatureEvent(o: AadlFeatureEvent): MOption[AadlFeatureEvent] = {
    val preR: PreResult[AadlFeatureEvent] = preAadlFeatureEvent(o)
    val r: MOption[AadlFeatureEvent] = if (preR.continu) {
      val o2: AadlFeatureEvent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlFeatureEvent] = o2 match {
        case o2: AadlEventPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlEventDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlFeatureEvent = r.getOrElse(o)
    val postR: MOption[AadlFeatureEvent] = postAadlFeatureEvent(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlFeatureData(o: AadlFeatureData): MOption[AadlFeatureData] = {
    val preR: PreResult[AadlFeatureData] = preAadlFeatureData(o)
    val r: MOption[AadlFeatureData] = if (preR.continu) {
      val o2: AadlFeatureData = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlFeatureData] = o2 match {
        case o2: AadlEventDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlDataPort =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
        case o2: AadlParameter =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlFeatureData = r.getOrElse(o)
    val postR: MOption[AadlFeatureData] = postAadlFeatureData(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlEventPort(o: AadlEventPort): MOption[AadlEventPort] = {
    val preR: PreResult[AadlEventPort] = preAadlEventPort(o)
    val r: MOption[AadlEventPort] = if (preR.continu) {
      val o2: AadlEventPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlEventPort = r.getOrElse(o)
    val postR: MOption[AadlEventPort] = postAadlEventPort(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlEventDataPort(o: AadlEventDataPort): MOption[AadlEventDataPort] = {
    val preR: PreResult[AadlEventDataPort] = preAadlEventDataPort(o)
    val r: MOption[AadlEventDataPort] = if (preR.continu) {
      val o2: AadlEventDataPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlEventDataPort = r.getOrElse(o)
    val postR: MOption[AadlEventDataPort] = postAadlEventDataPort(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlDataPort(o: AadlDataPort): MOption[AadlDataPort] = {
    val preR: PreResult[AadlDataPort] = preAadlDataPort(o)
    val r: MOption[AadlDataPort] = if (preR.continu) {
      val o2: AadlDataPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlDataPort = r.getOrElse(o)
    val postR: MOption[AadlDataPort] = postAadlDataPort(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlParameter(o: AadlParameter): MOption[AadlParameter] = {
    val preR: PreResult[AadlParameter] = preAadlParameter(o)
    val r: MOption[AadlParameter] = if (preR.continu) {
      val o2: AadlParameter = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlParameter = r.getOrElse(o)
    val postR: MOption[AadlParameter] = postAadlParameter(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlFeatureTODO(o: AadlFeatureTODO): MOption[AadlFeatureTODO] = {
    val preR: PreResult[AadlFeatureTODO] = preAadlFeatureTODO(o)
    val r: MOption[AadlFeatureTODO] = if (preR.continu) {
      val o2: AadlFeatureTODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlFeatureTODO = r.getOrElse(o)
    val postR: MOption[AadlFeatureTODO] = postAadlFeatureTODO(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlConnection(o: AadlConnection): MOption[AadlConnection] = {
    val preR: PreResult[AadlConnection] = preAadlConnection(o)
    val r: MOption[AadlConnection] = if (preR.continu) {
      val o2: AadlConnection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AadlConnection] = o2 match {
        case o2: AadlPortConnection =>
          val r0: MOption[AadlComponent] = transformAadlComponent(o2.srcComponent)
          val r1: MOption[AadlFeature] = transformAadlFeature(o2.srcFeature)
          val r2: MOption[AadlComponent] = transformAadlComponent(o2.dstComponent)
          val r3: MOption[AadlFeature] = transformAadlFeature(o2.dstFeature)
          if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty)
            MSome(o2(srcComponent = r0.getOrElse(o2.srcComponent), srcFeature = r1.getOrElse(o2.srcFeature), dstComponent = r2.getOrElse(o2.dstComponent), dstFeature = r3.getOrElse(o2.dstFeature)))
          else
            MNone()
        case o2: AadlConnectionTODO =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlConnection = r.getOrElse(o)
    val postR: MOption[AadlConnection] = postAadlConnection(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlPortConnection(o: AadlPortConnection): MOption[AadlPortConnection] = {
    val preR: PreResult[AadlPortConnection] = preAadlPortConnection(o)
    val r: MOption[AadlPortConnection] = if (preR.continu) {
      val o2: AadlPortConnection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[AadlComponent] = transformAadlComponent(o2.srcComponent)
      val r1: MOption[AadlFeature] = transformAadlFeature(o2.srcFeature)
      val r2: MOption[AadlComponent] = transformAadlComponent(o2.dstComponent)
      val r3: MOption[AadlFeature] = transformAadlFeature(o2.dstFeature)
      if (hasChanged || r0.nonEmpty || r1.nonEmpty || r2.nonEmpty || r3.nonEmpty)
        MSome(o2(srcComponent = r0.getOrElse(o2.srcComponent), srcFeature = r1.getOrElse(o2.srcFeature), dstComponent = r2.getOrElse(o2.dstComponent), dstFeature = r3.getOrElse(o2.dstFeature)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlPortConnection = r.getOrElse(o)
    val postR: MOption[AadlPortConnection] = postAadlPortConnection(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAadlConnectionTODO(o: AadlConnectionTODO): MOption[AadlConnectionTODO] = {
    val preR: PreResult[AadlConnectionTODO] = preAadlConnectionTODO(o)
    val r: MOption[AadlConnectionTODO] = if (preR.continu) {
      val o2: AadlConnectionTODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AadlConnectionTODO = r.getOrElse(o)
    val postR: MOption[AadlConnectionTODO] = postAadlConnectionTODO(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformAnnexInfo(o: AnnexInfo): MOption[AnnexInfo] = {
    val preR: PreResult[AnnexInfo] = preAnnexInfo(o)
    val r: MOption[AnnexInfo] = if (preR.continu) {
      val o2: AnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: MOption[AnnexInfo] = o2 match {
        case o2: BTSAnnexInfo =>
          val r0: MOption[BTSSymbolTable] = transformBTSSymbolTable(o2.btsSymbolTable)
          if (hasChanged || r0.nonEmpty)
            MSome(o2(btsSymbolTable = r0.getOrElse(o2.btsSymbolTable)))
          else
            MNone()
        case o2: TodoAnnexInfo =>
          if (hasChanged)
            MSome(o2)
          else
            MNone()
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: AnnexInfo = r.getOrElse(o)
    val postR: MOption[AnnexInfo] = postAnnexInfo(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformBTSAnnexInfo(o: BTSAnnexInfo): MOption[BTSAnnexInfo] = {
    val preR: PreResult[BTSAnnexInfo] = preBTSAnnexInfo(o)
    val r: MOption[BTSAnnexInfo] = if (preR.continu) {
      val o2: BTSAnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: MOption[BTSSymbolTable] = transformBTSSymbolTable(o2.btsSymbolTable)
      if (hasChanged || r0.nonEmpty)
        MSome(o2(btsSymbolTable = r0.getOrElse(o2.btsSymbolTable)))
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: BTSAnnexInfo = r.getOrElse(o)
    val postR: MOption[BTSAnnexInfo] = postBTSAnnexInfo(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

  def transformTodoAnnexInfo(o: TodoAnnexInfo): MOption[TodoAnnexInfo] = {
    val preR: PreResult[TodoAnnexInfo] = preTodoAnnexInfo(o)
    val r: MOption[TodoAnnexInfo] = if (preR.continu) {
      val o2: TodoAnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        MSome(o2)
      else
        MNone()
    } else if (preR.resultOpt.nonEmpty) {
      MSome(preR.resultOpt.getOrElse(o))
    } else {
      MNone()
    }
    val hasChanged: B = r.nonEmpty
    val o2: TodoAnnexInfo = r.getOrElse(o)
    val postR: MOption[TodoAnnexInfo] = postTodoAnnexInfo(o2)
    if (postR.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return MSome(o2)
    } else {
      return MNone()
    }
  }

}