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

// This file is auto-generated from AadlSymbols.scala

// This file is auto-generated from BTSSymbols.scala

package org.sireum.hamr.codegen.common.symbols

import org.sireum._

object Transformer {

  @datatype class PreResult[Context, T](ctx: Context,
                                        continu: B,
                                        resultOpt: Option[T])

  @datatype class TPostResult[Context, T](ctx: Context,
                                     resultOpt: Option[T])

  @sig trait PrePost[Context] {

    @pure def preBTSSymbol(ctx: Context, o: BTSSymbol): PreResult[Context, BTSSymbol] = {
      o match {
        case o: BTSState =>
          val r: PreResult[Context, BTSSymbol] = preBTSState(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: BTSSymbol)) => PreResult(preCtx, continu, Some[BTSSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type BTSSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[BTSSymbol]())
          }
          return r
        case o: BTSVariable =>
          val r: PreResult[Context, BTSSymbol] = preBTSVariable(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: BTSSymbol)) => PreResult(preCtx, continu, Some[BTSSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type BTSSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[BTSSymbol]())
          }
          return r
      }
    }

    @pure def preBTSState(ctx: Context, o: BTSState): PreResult[Context, BTSState] = {
      return PreResult(ctx, T, None())
    }

    @pure def preBTSVariable(ctx: Context, o: BTSVariable): PreResult[Context, BTSVariable] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlSymbol(ctx: Context, o: AadlSymbol): PreResult[Context, AadlSymbol] = {
      o match {
        case o: BTSState =>
          val r: PreResult[Context, AadlSymbol] = preBTSState(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: BTSVariable =>
          val r: PreResult[Context, AadlSymbol] = preBTSVariable(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlSystem =>
          val r: PreResult[Context, AadlSymbol] = preAadlSystem(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlProcessor =>
          val r: PreResult[Context, AadlSymbol] = preAadlProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: PreResult[Context, AadlSymbol] = preAadlVirtualProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlProcess =>
          val r: PreResult[Context, AadlSymbol] = preAadlProcess(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlThreadGroup =>
          val r: PreResult[Context, AadlSymbol] = preAadlThreadGroup(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlThread =>
          val r: PreResult[Context, AadlSymbol] = preAadlThread(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlDevice =>
          val r: PreResult[Context, AadlSymbol] = preAadlDevice(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlSubprogram =>
          val r: PreResult[Context, AadlSymbol] = preAadlSubprogram(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlTODOComponent =>
          val r: PreResult[Context, AadlSymbol] = preAadlTODOComponent(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlEventPort =>
          val r: PreResult[Context, AadlSymbol] = preAadlEventPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: PreResult[Context, AadlSymbol] = preAadlEventDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlDataPort =>
          val r: PreResult[Context, AadlSymbol] = preAadlDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlFeatureTODO =>
          val r: PreResult[Context, AadlSymbol] = preAadlFeatureTODO(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlPortConnection =>
          val r: PreResult[Context, AadlSymbol] = preAadlPortConnection(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
        case o: AadlConnectionTODO =>
          val r: PreResult[Context, AadlSymbol] = preAadlConnectionTODO(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlSymbol)) => PreResult(preCtx, continu, Some[AadlSymbol](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlSymbol]())
          }
          return r
      }
    }

    @pure def preAadlComponent(ctx: Context, o: AadlComponent): PreResult[Context, AadlComponent] = {
      o match {
        case o: AadlSystem =>
          val r: PreResult[Context, AadlComponent] = preAadlSystem(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlProcessor =>
          val r: PreResult[Context, AadlComponent] = preAadlProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: PreResult[Context, AadlComponent] = preAadlVirtualProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlProcess =>
          val r: PreResult[Context, AadlComponent] = preAadlProcess(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlThreadGroup =>
          val r: PreResult[Context, AadlComponent] = preAadlThreadGroup(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlThread =>
          val r: PreResult[Context, AadlComponent] = preAadlThread(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlDevice =>
          val r: PreResult[Context, AadlComponent] = preAadlDevice(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlSubprogram =>
          val r: PreResult[Context, AadlComponent] = preAadlSubprogram(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
        case o: AadlTODOComponent =>
          val r: PreResult[Context, AadlComponent] = preAadlTODOComponent(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlComponent)) => PreResult(preCtx, continu, Some[AadlComponent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlComponent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlComponent]())
          }
          return r
      }
    }

    @pure def preAadlSystem(ctx: Context, o: AadlSystem): PreResult[Context, AadlSystem] = {
      return PreResult(ctx, T, None())
    }

    @pure def preProcessor(ctx: Context, o: Processor): PreResult[Context, Processor] = {
      o match {
        case o: AadlProcessor =>
          val r: PreResult[Context, Processor] = preAadlProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: Processor)) => PreResult(preCtx, continu, Some[Processor](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type Processor")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[Processor]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: PreResult[Context, Processor] = preAadlVirtualProcessor(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: Processor)) => PreResult(preCtx, continu, Some[Processor](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type Processor")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[Processor]())
          }
          return r
      }
    }

    @pure def preAadlProcessor(ctx: Context, o: AadlProcessor): PreResult[Context, AadlProcessor] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlVirtualProcessor(ctx: Context, o: AadlVirtualProcessor): PreResult[Context, AadlVirtualProcessor] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlProcess(ctx: Context, o: AadlProcess): PreResult[Context, AadlProcess] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlThreadGroup(ctx: Context, o: AadlThreadGroup): PreResult[Context, AadlThreadGroup] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlThreadOrDevice(ctx: Context, o: AadlThreadOrDevice): PreResult[Context, AadlThreadOrDevice] = {
      o match {
        case o: AadlThread =>
          val r: PreResult[Context, AadlThreadOrDevice] = preAadlThread(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlThreadOrDevice)) => PreResult(preCtx, continu, Some[AadlThreadOrDevice](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlThreadOrDevice")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlThreadOrDevice]())
          }
          return r
        case o: AadlDevice =>
          val r: PreResult[Context, AadlThreadOrDevice] = preAadlDevice(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlThreadOrDevice)) => PreResult(preCtx, continu, Some[AadlThreadOrDevice](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlThreadOrDevice")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlThreadOrDevice]())
          }
          return r
      }
    }

    @pure def preAadlThread(ctx: Context, o: AadlThread): PreResult[Context, AadlThread] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlDevice(ctx: Context, o: AadlDevice): PreResult[Context, AadlDevice] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlSubprogram(ctx: Context, o: AadlSubprogram): PreResult[Context, AadlSubprogram] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlTODOComponent(ctx: Context, o: AadlTODOComponent): PreResult[Context, AadlTODOComponent] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlFeature(ctx: Context, o: AadlFeature): PreResult[Context, AadlFeature] = {
      o match {
        case o: AadlEventPort =>
          val r: PreResult[Context, AadlFeature] = preAadlEventPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeature)) => PreResult(preCtx, continu, Some[AadlFeature](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeature")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeature]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: PreResult[Context, AadlFeature] = preAadlEventDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeature)) => PreResult(preCtx, continu, Some[AadlFeature](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeature")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeature]())
          }
          return r
        case o: AadlDataPort =>
          val r: PreResult[Context, AadlFeature] = preAadlDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeature)) => PreResult(preCtx, continu, Some[AadlFeature](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeature")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeature]())
          }
          return r
        case o: AadlFeatureTODO =>
          val r: PreResult[Context, AadlFeature] = preAadlFeatureTODO(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeature)) => PreResult(preCtx, continu, Some[AadlFeature](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeature")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeature]())
          }
          return r
      }
    }

    @pure def preAadlFeatureEvent(ctx: Context, o: AadlFeatureEvent): PreResult[Context, AadlFeatureEvent] = {
      o match {
        case o: AadlEventPort =>
          val r: PreResult[Context, AadlFeatureEvent] = preAadlEventPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeatureEvent)) => PreResult(preCtx, continu, Some[AadlFeatureEvent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeatureEvent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeatureEvent]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: PreResult[Context, AadlFeatureEvent] = preAadlEventDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeatureEvent)) => PreResult(preCtx, continu, Some[AadlFeatureEvent](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeatureEvent")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeatureEvent]())
          }
          return r
      }
    }

    @pure def preAadlFeatureData(ctx: Context, o: AadlFeatureData): PreResult[Context, AadlFeatureData] = {
      o match {
        case o: AadlEventDataPort =>
          val r: PreResult[Context, AadlFeatureData] = preAadlEventDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeatureData)) => PreResult(preCtx, continu, Some[AadlFeatureData](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeatureData")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeatureData]())
          }
          return r
        case o: AadlDataPort =>
          val r: PreResult[Context, AadlFeatureData] = preAadlDataPort(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlFeatureData)) => PreResult(preCtx, continu, Some[AadlFeatureData](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlFeatureData")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlFeatureData]())
          }
          return r
      }
    }

    @pure def preAadlEventPort(ctx: Context, o: AadlEventPort): PreResult[Context, AadlEventPort] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlEventDataPort(ctx: Context, o: AadlEventDataPort): PreResult[Context, AadlEventDataPort] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlDataPort(ctx: Context, o: AadlDataPort): PreResult[Context, AadlDataPort] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlFeatureTODO(ctx: Context, o: AadlFeatureTODO): PreResult[Context, AadlFeatureTODO] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlConnection(ctx: Context, o: AadlConnection): PreResult[Context, AadlConnection] = {
      o match {
        case o: AadlPortConnection =>
          val r: PreResult[Context, AadlConnection] = preAadlPortConnection(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlConnection)) => PreResult(preCtx, continu, Some[AadlConnection](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlConnection")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlConnection]())
          }
          return r
        case o: AadlConnectionTODO =>
          val r: PreResult[Context, AadlConnection] = preAadlConnectionTODO(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AadlConnection)) => PreResult(preCtx, continu, Some[AadlConnection](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AadlConnection")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AadlConnection]())
          }
          return r
      }
    }

    @pure def preAadlPortConnection(ctx: Context, o: AadlPortConnection): PreResult[Context, AadlPortConnection] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAadlConnectionTODO(ctx: Context, o: AadlConnectionTODO): PreResult[Context, AadlConnectionTODO] = {
      return PreResult(ctx, T, None())
    }

    @pure def preAnnexInfo(ctx: Context, o: AnnexInfo): PreResult[Context, AnnexInfo] = {
      o match {
        case o: BTSAnnexInfo =>
          val r: PreResult[Context, AnnexInfo] = preBTSAnnexInfo(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AnnexInfo)) => PreResult(preCtx, continu, Some[AnnexInfo](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AnnexInfo")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AnnexInfo]())
          }
          return r
        case o: TodoAnnexInfo =>
          val r: PreResult[Context, AnnexInfo] = preTodoAnnexInfo(ctx, o) match {
           case PreResult(preCtx, continu, Some(r: AnnexInfo)) => PreResult(preCtx, continu, Some[AnnexInfo](r))
           case PreResult(_, _, Some(_)) => halt("Can only produce object of type AnnexInfo")
           case PreResult(preCtx, continu, _) => PreResult(preCtx, continu, None[AnnexInfo]())
          }
          return r
      }
    }

    @pure def preBTSAnnexInfo(ctx: Context, o: BTSAnnexInfo): PreResult[Context, BTSAnnexInfo] = {
      return PreResult(ctx, T, None())
    }

    @pure def preTodoAnnexInfo(ctx: Context, o: TodoAnnexInfo): PreResult[Context, TodoAnnexInfo] = {
      return PreResult(ctx, T, None())
    }

    @pure def postBTSSymbol(ctx: Context, o: BTSSymbol): TPostResult[Context, BTSSymbol] = {
      o match {
        case o: BTSState =>
          val r: TPostResult[Context, BTSSymbol] = postBTSState(ctx, o) match {
           case TPostResult(postCtx, Some(result: BTSSymbol)) => TPostResult(postCtx, Some[BTSSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type BTSSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[BTSSymbol]())
          }
          return r
        case o: BTSVariable =>
          val r: TPostResult[Context, BTSSymbol] = postBTSVariable(ctx, o) match {
           case TPostResult(postCtx, Some(result: BTSSymbol)) => TPostResult(postCtx, Some[BTSSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type BTSSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[BTSSymbol]())
          }
          return r
      }
    }

    @pure def postBTSState(ctx: Context, o: BTSState): TPostResult[Context, BTSState] = {
      return TPostResult(ctx, None())
    }

    @pure def postBTSVariable(ctx: Context, o: BTSVariable): TPostResult[Context, BTSVariable] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlSymbol(ctx: Context, o: AadlSymbol): TPostResult[Context, AadlSymbol] = {
      o match {
        case o: BTSState =>
          val r: TPostResult[Context, AadlSymbol] = postBTSState(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: BTSVariable =>
          val r: TPostResult[Context, AadlSymbol] = postBTSVariable(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlSystem =>
          val r: TPostResult[Context, AadlSymbol] = postAadlSystem(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlProcessor =>
          val r: TPostResult[Context, AadlSymbol] = postAadlProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: TPostResult[Context, AadlSymbol] = postAadlVirtualProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlProcess =>
          val r: TPostResult[Context, AadlSymbol] = postAadlProcess(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlThreadGroup =>
          val r: TPostResult[Context, AadlSymbol] = postAadlThreadGroup(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlThread =>
          val r: TPostResult[Context, AadlSymbol] = postAadlThread(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlDevice =>
          val r: TPostResult[Context, AadlSymbol] = postAadlDevice(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlSubprogram =>
          val r: TPostResult[Context, AadlSymbol] = postAadlSubprogram(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlTODOComponent =>
          val r: TPostResult[Context, AadlSymbol] = postAadlTODOComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlEventPort =>
          val r: TPostResult[Context, AadlSymbol] = postAadlEventPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: TPostResult[Context, AadlSymbol] = postAadlEventDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlDataPort =>
          val r: TPostResult[Context, AadlSymbol] = postAadlDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlFeatureTODO =>
          val r: TPostResult[Context, AadlSymbol] = postAadlFeatureTODO(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlPortConnection =>
          val r: TPostResult[Context, AadlSymbol] = postAadlPortConnection(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
        case o: AadlConnectionTODO =>
          val r: TPostResult[Context, AadlSymbol] = postAadlConnectionTODO(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlSymbol)) => TPostResult(postCtx, Some[AadlSymbol](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlSymbol")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlSymbol]())
          }
          return r
      }
    }

    @pure def postAadlComponent(ctx: Context, o: AadlComponent): TPostResult[Context, AadlComponent] = {
      o match {
        case o: AadlSystem =>
          val r: TPostResult[Context, AadlComponent] = postAadlSystem(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlProcessor =>
          val r: TPostResult[Context, AadlComponent] = postAadlProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: TPostResult[Context, AadlComponent] = postAadlVirtualProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlProcess =>
          val r: TPostResult[Context, AadlComponent] = postAadlProcess(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlThreadGroup =>
          val r: TPostResult[Context, AadlComponent] = postAadlThreadGroup(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlThread =>
          val r: TPostResult[Context, AadlComponent] = postAadlThread(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlDevice =>
          val r: TPostResult[Context, AadlComponent] = postAadlDevice(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlSubprogram =>
          val r: TPostResult[Context, AadlComponent] = postAadlSubprogram(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
        case o: AadlTODOComponent =>
          val r: TPostResult[Context, AadlComponent] = postAadlTODOComponent(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlComponent)) => TPostResult(postCtx, Some[AadlComponent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlComponent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlComponent]())
          }
          return r
      }
    }

    @pure def postAadlSystem(ctx: Context, o: AadlSystem): TPostResult[Context, AadlSystem] = {
      return TPostResult(ctx, None())
    }

    @pure def postProcessor(ctx: Context, o: Processor): TPostResult[Context, Processor] = {
      o match {
        case o: AadlProcessor =>
          val r: TPostResult[Context, Processor] = postAadlProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: Processor)) => TPostResult(postCtx, Some[Processor](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type Processor")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[Processor]())
          }
          return r
        case o: AadlVirtualProcessor =>
          val r: TPostResult[Context, Processor] = postAadlVirtualProcessor(ctx, o) match {
           case TPostResult(postCtx, Some(result: Processor)) => TPostResult(postCtx, Some[Processor](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type Processor")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[Processor]())
          }
          return r
      }
    }

    @pure def postAadlProcessor(ctx: Context, o: AadlProcessor): TPostResult[Context, AadlProcessor] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlVirtualProcessor(ctx: Context, o: AadlVirtualProcessor): TPostResult[Context, AadlVirtualProcessor] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlProcess(ctx: Context, o: AadlProcess): TPostResult[Context, AadlProcess] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlThreadGroup(ctx: Context, o: AadlThreadGroup): TPostResult[Context, AadlThreadGroup] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlThreadOrDevice(ctx: Context, o: AadlThreadOrDevice): TPostResult[Context, AadlThreadOrDevice] = {
      o match {
        case o: AadlThread =>
          val r: TPostResult[Context, AadlThreadOrDevice] = postAadlThread(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlThreadOrDevice)) => TPostResult(postCtx, Some[AadlThreadOrDevice](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlThreadOrDevice")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlThreadOrDevice]())
          }
          return r
        case o: AadlDevice =>
          val r: TPostResult[Context, AadlThreadOrDevice] = postAadlDevice(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlThreadOrDevice)) => TPostResult(postCtx, Some[AadlThreadOrDevice](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlThreadOrDevice")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlThreadOrDevice]())
          }
          return r
      }
    }

    @pure def postAadlThread(ctx: Context, o: AadlThread): TPostResult[Context, AadlThread] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlDevice(ctx: Context, o: AadlDevice): TPostResult[Context, AadlDevice] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlSubprogram(ctx: Context, o: AadlSubprogram): TPostResult[Context, AadlSubprogram] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlTODOComponent(ctx: Context, o: AadlTODOComponent): TPostResult[Context, AadlTODOComponent] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlFeature(ctx: Context, o: AadlFeature): TPostResult[Context, AadlFeature] = {
      o match {
        case o: AadlEventPort =>
          val r: TPostResult[Context, AadlFeature] = postAadlEventPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeature)) => TPostResult(postCtx, Some[AadlFeature](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeature")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeature]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: TPostResult[Context, AadlFeature] = postAadlEventDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeature)) => TPostResult(postCtx, Some[AadlFeature](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeature")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeature]())
          }
          return r
        case o: AadlDataPort =>
          val r: TPostResult[Context, AadlFeature] = postAadlDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeature)) => TPostResult(postCtx, Some[AadlFeature](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeature")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeature]())
          }
          return r
        case o: AadlFeatureTODO =>
          val r: TPostResult[Context, AadlFeature] = postAadlFeatureTODO(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeature)) => TPostResult(postCtx, Some[AadlFeature](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeature")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeature]())
          }
          return r
      }
    }

    @pure def postAadlFeatureEvent(ctx: Context, o: AadlFeatureEvent): TPostResult[Context, AadlFeatureEvent] = {
      o match {
        case o: AadlEventPort =>
          val r: TPostResult[Context, AadlFeatureEvent] = postAadlEventPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeatureEvent)) => TPostResult(postCtx, Some[AadlFeatureEvent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeatureEvent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeatureEvent]())
          }
          return r
        case o: AadlEventDataPort =>
          val r: TPostResult[Context, AadlFeatureEvent] = postAadlEventDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeatureEvent)) => TPostResult(postCtx, Some[AadlFeatureEvent](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeatureEvent")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeatureEvent]())
          }
          return r
      }
    }

    @pure def postAadlFeatureData(ctx: Context, o: AadlFeatureData): TPostResult[Context, AadlFeatureData] = {
      o match {
        case o: AadlEventDataPort =>
          val r: TPostResult[Context, AadlFeatureData] = postAadlEventDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeatureData)) => TPostResult(postCtx, Some[AadlFeatureData](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeatureData")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeatureData]())
          }
          return r
        case o: AadlDataPort =>
          val r: TPostResult[Context, AadlFeatureData] = postAadlDataPort(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlFeatureData)) => TPostResult(postCtx, Some[AadlFeatureData](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlFeatureData")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlFeatureData]())
          }
          return r
      }
    }

    @pure def postAadlEventPort(ctx: Context, o: AadlEventPort): TPostResult[Context, AadlEventPort] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlEventDataPort(ctx: Context, o: AadlEventDataPort): TPostResult[Context, AadlEventDataPort] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlDataPort(ctx: Context, o: AadlDataPort): TPostResult[Context, AadlDataPort] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlFeatureTODO(ctx: Context, o: AadlFeatureTODO): TPostResult[Context, AadlFeatureTODO] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlConnection(ctx: Context, o: AadlConnection): TPostResult[Context, AadlConnection] = {
      o match {
        case o: AadlPortConnection =>
          val r: TPostResult[Context, AadlConnection] = postAadlPortConnection(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlConnection)) => TPostResult(postCtx, Some[AadlConnection](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlConnection")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlConnection]())
          }
          return r
        case o: AadlConnectionTODO =>
          val r: TPostResult[Context, AadlConnection] = postAadlConnectionTODO(ctx, o) match {
           case TPostResult(postCtx, Some(result: AadlConnection)) => TPostResult(postCtx, Some[AadlConnection](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AadlConnection")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AadlConnection]())
          }
          return r
      }
    }

    @pure def postAadlPortConnection(ctx: Context, o: AadlPortConnection): TPostResult[Context, AadlPortConnection] = {
      return TPostResult(ctx, None())
    }

    @pure def postAadlConnectionTODO(ctx: Context, o: AadlConnectionTODO): TPostResult[Context, AadlConnectionTODO] = {
      return TPostResult(ctx, None())
    }

    @pure def postAnnexInfo(ctx: Context, o: AnnexInfo): TPostResult[Context, AnnexInfo] = {
      o match {
        case o: BTSAnnexInfo =>
          val r: TPostResult[Context, AnnexInfo] = postBTSAnnexInfo(ctx, o) match {
           case TPostResult(postCtx, Some(result: AnnexInfo)) => TPostResult(postCtx, Some[AnnexInfo](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AnnexInfo")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AnnexInfo]())
          }
          return r
        case o: TodoAnnexInfo =>
          val r: TPostResult[Context, AnnexInfo] = postTodoAnnexInfo(ctx, o) match {
           case TPostResult(postCtx, Some(result: AnnexInfo)) => TPostResult(postCtx, Some[AnnexInfo](result))
           case TPostResult(_, Some(_)) => halt("Can only produce object of type AnnexInfo")
           case TPostResult(postCtx, _) => TPostResult(postCtx, None[AnnexInfo]())
          }
          return r
      }
    }

    @pure def postBTSAnnexInfo(ctx: Context, o: BTSAnnexInfo): TPostResult[Context, BTSAnnexInfo] = {
      return TPostResult(ctx, None())
    }

    @pure def postTodoAnnexInfo(ctx: Context, o: TodoAnnexInfo): TPostResult[Context, TodoAnnexInfo] = {
      return TPostResult(ctx, None())
    }

  }

  @pure def transformISZ[Context, T](ctx: Context, s: IS[Z, T], f: (Context, T) => TPostResult[Context, T] @pure): TPostResult[Context, IS[Z, T]] = {
    val s2: MS[Z, T] = s.toMS
    var changed: B = F
    var ctxi = ctx
    for (i <- s2.indices) {
      val e: T = s(i)
      val r: TPostResult[Context, T] = f(ctxi, e)
      ctxi = r.ctx
      changed = changed || r.resultOpt.nonEmpty
      s2(i) = r.resultOpt.getOrElse(e)
    }
    if (changed) {
      return TPostResult(ctxi, Some(s2.toIS))
    } else {
      return TPostResult[Context, IS[Z, T]](ctxi, None[IS[Z, T]]())
    }
  }

}

import Transformer._

@datatype class Transformer[Context](pp: PrePost[Context]) {

  @pure def transformBTSSymbol(ctx: Context, o: BTSSymbol): TPostResult[Context, BTSSymbol] = {
    val preR: PreResult[Context, BTSSymbol] = pp.preBTSSymbol(ctx, o)
    val r: TPostResult[Context, BTSSymbol] = if (preR.continu) {
      val o2: BTSSymbol = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, BTSSymbol] = o2 match {
        case o2: BTSState =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: BTSVariable =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BTSSymbol = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, BTSSymbol] = pp.postBTSSymbol(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformBTSState(ctx: Context, o: BTSState): TPostResult[Context, BTSState] = {
    val preR: PreResult[Context, BTSState] = pp.preBTSState(ctx, o)
    val r: TPostResult[Context, BTSState] = if (preR.continu) {
      val o2: BTSState = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BTSState = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, BTSState] = pp.postBTSState(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformBTSVariable(ctx: Context, o: BTSVariable): TPostResult[Context, BTSVariable] = {
    val preR: PreResult[Context, BTSVariable] = pp.preBTSVariable(ctx, o)
    val r: TPostResult[Context, BTSVariable] = if (preR.continu) {
      val o2: BTSVariable = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BTSVariable = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, BTSVariable] = pp.postBTSVariable(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlSymbol(ctx: Context, o: AadlSymbol): TPostResult[Context, AadlSymbol] = {
    val preR: PreResult[Context, AadlSymbol] = pp.preAadlSymbol(ctx, o)
    val r: TPostResult[Context, AadlSymbol] = if (preR.continu) {
      val o2: AadlSymbol = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlSymbol] = o2 match {
        case o2: BTSState =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: BTSVariable =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlSystem =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlVirtualProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlProcess =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlThreadGroup =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlThread =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlDevice =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlSubprogram =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.features, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), features = r1.resultOpt.getOrElse(o2.features))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlTODOComponent =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlEventPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlEventDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlFeatureTODO =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlPortConnection =>
          val r0: TPostResult[Context, AadlComponent] = transformAadlComponent(preR.ctx, o2.srcComponent)
          val r1: TPostResult[Context, AadlFeature] = transformAadlFeature(r0.ctx, o2.srcFeature)
          val r2: TPostResult[Context, AadlComponent] = transformAadlComponent(r1.ctx, o2.dstComponent)
          val r3: TPostResult[Context, AadlFeature] = transformAadlFeature(r2.ctx, o2.dstFeature)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
            TPostResult(r3.ctx, Some(o2(srcComponent = r0.resultOpt.getOrElse(o2.srcComponent), srcFeature = r1.resultOpt.getOrElse(o2.srcFeature), dstComponent = r2.resultOpt.getOrElse(o2.dstComponent), dstFeature = r3.resultOpt.getOrElse(o2.dstFeature))))
          else
            TPostResult(r3.ctx, None())
        case o2: AadlConnectionTODO =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlSymbol = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlSymbol] = pp.postAadlSymbol(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlComponent(ctx: Context, o: AadlComponent): TPostResult[Context, AadlComponent] = {
    val preR: PreResult[Context, AadlComponent] = pp.preAadlComponent(ctx, o)
    val r: TPostResult[Context, AadlComponent] = if (preR.continu) {
      val o2: AadlComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlComponent] = o2 match {
        case o2: AadlSystem =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlVirtualProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlProcess =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlThreadGroup =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlThread =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlDevice =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlSubprogram =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.features, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), features = r1.resultOpt.getOrElse(o2.features))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlTODOComponent =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlComponent = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlComponent] = pp.postAadlComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlSystem(ctx: Context, o: AadlSystem): TPostResult[Context, AadlSystem] = {
    val preR: PreResult[Context, AadlSystem] = pp.preAadlSystem(ctx, o)
    val r: TPostResult[Context, AadlSystem] = if (preR.continu) {
      val o2: AadlSystem = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlSystem = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlSystem] = pp.postAadlSystem(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformProcessor(ctx: Context, o: Processor): TPostResult[Context, Processor] = {
    val preR: PreResult[Context, Processor] = pp.preProcessor(ctx, o)
    val r: TPostResult[Context, Processor] = if (preR.continu) {
      val o2: Processor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, Processor] = o2 match {
        case o2: AadlProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
        case o2: AadlVirtualProcessor =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          if (hasChanged || r0.resultOpt.nonEmpty)
            TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
          else
            TPostResult(r0.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: Processor = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, Processor] = pp.postProcessor(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlProcessor(ctx: Context, o: AadlProcessor): TPostResult[Context, AadlProcessor] = {
    val preR: PreResult[Context, AadlProcessor] = pp.preAadlProcessor(ctx, o)
    val r: TPostResult[Context, AadlProcessor] = if (preR.continu) {
      val o2: AadlProcessor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlProcessor = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlProcessor] = pp.postAadlProcessor(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlVirtualProcessor(ctx: Context, o: AadlVirtualProcessor): TPostResult[Context, AadlVirtualProcessor] = {
    val preR: PreResult[Context, AadlVirtualProcessor] = pp.preAadlVirtualProcessor(ctx, o)
    val r: TPostResult[Context, AadlVirtualProcessor] = if (preR.continu) {
      val o2: AadlVirtualProcessor = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlVirtualProcessor = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlVirtualProcessor] = pp.postAadlVirtualProcessor(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlProcess(ctx: Context, o: AadlProcess): TPostResult[Context, AadlProcess] = {
    val preR: PreResult[Context, AadlProcess] = pp.preAadlProcess(ctx, o)
    val r: TPostResult[Context, AadlProcess] = if (preR.continu) {
      val o2: AadlProcess = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlProcess = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlProcess] = pp.postAadlProcess(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlThreadGroup(ctx: Context, o: AadlThreadGroup): TPostResult[Context, AadlThreadGroup] = {
    val preR: PreResult[Context, AadlThreadGroup] = pp.preAadlThreadGroup(ctx, o)
    val r: TPostResult[Context, AadlThreadGroup] = if (preR.continu) {
      val o2: AadlThreadGroup = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlThreadGroup = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlThreadGroup] = pp.postAadlThreadGroup(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlThreadOrDevice(ctx: Context, o: AadlThreadOrDevice): TPostResult[Context, AadlThreadOrDevice] = {
    val preR: PreResult[Context, AadlThreadOrDevice] = pp.preAadlThreadOrDevice(ctx, o)
    val r: TPostResult[Context, AadlThreadOrDevice] = if (preR.continu) {
      val o2: AadlThreadOrDevice = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlThreadOrDevice] = o2 match {
        case o2: AadlThread =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
        case o2: AadlDevice =>
          val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
          val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
            TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
          else
            TPostResult(r1.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlThreadOrDevice = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlThreadOrDevice] = pp.postAadlThreadOrDevice(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlThread(ctx: Context, o: AadlThread): TPostResult[Context, AadlThread] = {
    val preR: PreResult[Context, AadlThread] = pp.preAadlThread(ctx, o)
    val r: TPostResult[Context, AadlThread] = if (preR.continu) {
      val o2: AadlThread = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
        TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
      else
        TPostResult(r1.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlThread = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlThread] = pp.postAadlThread(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlDevice(ctx: Context, o: AadlDevice): TPostResult[Context, AadlDevice] = {
    val preR: PreResult[Context, AadlDevice] = pp.preAadlDevice(ctx, o)
    val r: TPostResult[Context, AadlDevice] = if (preR.continu) {
      val o2: AadlDevice = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.ports, transformAadlFeature _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
        TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), ports = r1.resultOpt.getOrElse(o2.ports))))
      else
        TPostResult(r1.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlDevice = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlDevice] = pp.postAadlDevice(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlSubprogram(ctx: Context, o: AadlSubprogram): TPostResult[Context, AadlSubprogram] = {
    val preR: PreResult[Context, AadlSubprogram] = pp.preAadlSubprogram(ctx, o)
    val r: TPostResult[Context, AadlSubprogram] = if (preR.continu) {
      val o2: AadlSubprogram = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      val r1: TPostResult[Context, IS[Z, AadlFeature]] = transformISZ(r0.ctx, o2.features, transformAadlFeature _)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty)
        TPostResult(r1.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents), features = r1.resultOpt.getOrElse(o2.features))))
      else
        TPostResult(r1.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlSubprogram = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlSubprogram] = pp.postAadlSubprogram(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlTODOComponent(ctx: Context, o: AadlTODOComponent): TPostResult[Context, AadlTODOComponent] = {
    val preR: PreResult[Context, AadlTODOComponent] = pp.preAadlTODOComponent(ctx, o)
    val r: TPostResult[Context, AadlTODOComponent] = if (preR.continu) {
      val o2: AadlTODOComponent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, IS[Z, AadlComponent]] = transformISZ(preR.ctx, o2.subComponents, transformAadlComponent _)
      if (hasChanged || r0.resultOpt.nonEmpty)
        TPostResult(r0.ctx, Some(o2(subComponents = r0.resultOpt.getOrElse(o2.subComponents))))
      else
        TPostResult(r0.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlTODOComponent = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlTODOComponent] = pp.postAadlTODOComponent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlFeature(ctx: Context, o: AadlFeature): TPostResult[Context, AadlFeature] = {
    val preR: PreResult[Context, AadlFeature] = pp.preAadlFeature(ctx, o)
    val r: TPostResult[Context, AadlFeature] = if (preR.continu) {
      val o2: AadlFeature = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlFeature] = o2 match {
        case o2: AadlEventPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlEventDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlFeatureTODO =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlFeature = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlFeature] = pp.postAadlFeature(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlFeatureEvent(ctx: Context, o: AadlFeatureEvent): TPostResult[Context, AadlFeatureEvent] = {
    val preR: PreResult[Context, AadlFeatureEvent] = pp.preAadlFeatureEvent(ctx, o)
    val r: TPostResult[Context, AadlFeatureEvent] = if (preR.continu) {
      val o2: AadlFeatureEvent = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlFeatureEvent] = o2 match {
        case o2: AadlEventPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlEventDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlFeatureEvent = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlFeatureEvent] = pp.postAadlFeatureEvent(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlFeatureData(ctx: Context, o: AadlFeatureData): TPostResult[Context, AadlFeatureData] = {
    val preR: PreResult[Context, AadlFeatureData] = pp.preAadlFeatureData(ctx, o)
    val r: TPostResult[Context, AadlFeatureData] = if (preR.continu) {
      val o2: AadlFeatureData = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlFeatureData] = o2 match {
        case o2: AadlEventDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: AadlDataPort =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlFeatureData = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlFeatureData] = pp.postAadlFeatureData(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlEventPort(ctx: Context, o: AadlEventPort): TPostResult[Context, AadlEventPort] = {
    val preR: PreResult[Context, AadlEventPort] = pp.preAadlEventPort(ctx, o)
    val r: TPostResult[Context, AadlEventPort] = if (preR.continu) {
      val o2: AadlEventPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlEventPort = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlEventPort] = pp.postAadlEventPort(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlEventDataPort(ctx: Context, o: AadlEventDataPort): TPostResult[Context, AadlEventDataPort] = {
    val preR: PreResult[Context, AadlEventDataPort] = pp.preAadlEventDataPort(ctx, o)
    val r: TPostResult[Context, AadlEventDataPort] = if (preR.continu) {
      val o2: AadlEventDataPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlEventDataPort = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlEventDataPort] = pp.postAadlEventDataPort(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlDataPort(ctx: Context, o: AadlDataPort): TPostResult[Context, AadlDataPort] = {
    val preR: PreResult[Context, AadlDataPort] = pp.preAadlDataPort(ctx, o)
    val r: TPostResult[Context, AadlDataPort] = if (preR.continu) {
      val o2: AadlDataPort = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlDataPort = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlDataPort] = pp.postAadlDataPort(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlFeatureTODO(ctx: Context, o: AadlFeatureTODO): TPostResult[Context, AadlFeatureTODO] = {
    val preR: PreResult[Context, AadlFeatureTODO] = pp.preAadlFeatureTODO(ctx, o)
    val r: TPostResult[Context, AadlFeatureTODO] = if (preR.continu) {
      val o2: AadlFeatureTODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlFeatureTODO = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlFeatureTODO] = pp.postAadlFeatureTODO(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlConnection(ctx: Context, o: AadlConnection): TPostResult[Context, AadlConnection] = {
    val preR: PreResult[Context, AadlConnection] = pp.preAadlConnection(ctx, o)
    val r: TPostResult[Context, AadlConnection] = if (preR.continu) {
      val o2: AadlConnection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AadlConnection] = o2 match {
        case o2: AadlPortConnection =>
          val r0: TPostResult[Context, AadlComponent] = transformAadlComponent(preR.ctx, o2.srcComponent)
          val r1: TPostResult[Context, AadlFeature] = transformAadlFeature(r0.ctx, o2.srcFeature)
          val r2: TPostResult[Context, AadlComponent] = transformAadlComponent(r1.ctx, o2.dstComponent)
          val r3: TPostResult[Context, AadlFeature] = transformAadlFeature(r2.ctx, o2.dstFeature)
          if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
            TPostResult(r3.ctx, Some(o2(srcComponent = r0.resultOpt.getOrElse(o2.srcComponent), srcFeature = r1.resultOpt.getOrElse(o2.srcFeature), dstComponent = r2.resultOpt.getOrElse(o2.dstComponent), dstFeature = r3.resultOpt.getOrElse(o2.dstFeature))))
          else
            TPostResult(r3.ctx, None())
        case o2: AadlConnectionTODO =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlConnection = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlConnection] = pp.postAadlConnection(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlPortConnection(ctx: Context, o: AadlPortConnection): TPostResult[Context, AadlPortConnection] = {
    val preR: PreResult[Context, AadlPortConnection] = pp.preAadlPortConnection(ctx, o)
    val r: TPostResult[Context, AadlPortConnection] = if (preR.continu) {
      val o2: AadlPortConnection = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val r0: TPostResult[Context, AadlComponent] = transformAadlComponent(preR.ctx, o2.srcComponent)
      val r1: TPostResult[Context, AadlFeature] = transformAadlFeature(r0.ctx, o2.srcFeature)
      val r2: TPostResult[Context, AadlComponent] = transformAadlComponent(r1.ctx, o2.dstComponent)
      val r3: TPostResult[Context, AadlFeature] = transformAadlFeature(r2.ctx, o2.dstFeature)
      if (hasChanged || r0.resultOpt.nonEmpty || r1.resultOpt.nonEmpty || r2.resultOpt.nonEmpty || r3.resultOpt.nonEmpty)
        TPostResult(r3.ctx, Some(o2(srcComponent = r0.resultOpt.getOrElse(o2.srcComponent), srcFeature = r1.resultOpt.getOrElse(o2.srcFeature), dstComponent = r2.resultOpt.getOrElse(o2.dstComponent), dstFeature = r3.resultOpt.getOrElse(o2.dstFeature))))
      else
        TPostResult(r3.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlPortConnection = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlPortConnection] = pp.postAadlPortConnection(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAadlConnectionTODO(ctx: Context, o: AadlConnectionTODO): TPostResult[Context, AadlConnectionTODO] = {
    val preR: PreResult[Context, AadlConnectionTODO] = pp.preAadlConnectionTODO(ctx, o)
    val r: TPostResult[Context, AadlConnectionTODO] = if (preR.continu) {
      val o2: AadlConnectionTODO = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AadlConnectionTODO = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AadlConnectionTODO] = pp.postAadlConnectionTODO(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformAnnexInfo(ctx: Context, o: AnnexInfo): TPostResult[Context, AnnexInfo] = {
    val preR: PreResult[Context, AnnexInfo] = pp.preAnnexInfo(ctx, o)
    val r: TPostResult[Context, AnnexInfo] = if (preR.continu) {
      val o2: AnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      val rOpt: TPostResult[Context, AnnexInfo] = o2 match {
        case o2: BTSAnnexInfo =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
        case o2: TodoAnnexInfo =>
          if (hasChanged)
            TPostResult(preR.ctx, Some(o2))
          else
            TPostResult(preR.ctx, None())
      }
      rOpt
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: AnnexInfo = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, AnnexInfo] = pp.postAnnexInfo(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformBTSAnnexInfo(ctx: Context, o: BTSAnnexInfo): TPostResult[Context, BTSAnnexInfo] = {
    val preR: PreResult[Context, BTSAnnexInfo] = pp.preBTSAnnexInfo(ctx, o)
    val r: TPostResult[Context, BTSAnnexInfo] = if (preR.continu) {
      val o2: BTSAnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: BTSAnnexInfo = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, BTSAnnexInfo] = pp.postBTSAnnexInfo(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

  @pure def transformTodoAnnexInfo(ctx: Context, o: TodoAnnexInfo): TPostResult[Context, TodoAnnexInfo] = {
    val preR: PreResult[Context, TodoAnnexInfo] = pp.preTodoAnnexInfo(ctx, o)
    val r: TPostResult[Context, TodoAnnexInfo] = if (preR.continu) {
      val o2: TodoAnnexInfo = preR.resultOpt.getOrElse(o)
      val hasChanged: B = preR.resultOpt.nonEmpty
      if (hasChanged)
        TPostResult(preR.ctx, Some(o2))
      else
        TPostResult(preR.ctx, None())
    } else if (preR.resultOpt.nonEmpty) {
      TPostResult(preR.ctx, Some(preR.resultOpt.getOrElse(o)))
    } else {
      TPostResult(preR.ctx, None())
    }
    val hasChanged: B = r.resultOpt.nonEmpty
    val o2: TodoAnnexInfo = r.resultOpt.getOrElse(o)
    val postR: TPostResult[Context, TodoAnnexInfo] = pp.postTodoAnnexInfo(r.ctx, o2)
    if (postR.resultOpt.nonEmpty) {
      return postR
    } else if (hasChanged) {
      return TPostResult(postR.ctx, Some(o2))
    } else {
      return TPostResult(postR.ctx, None())
    }
  }

}