package Untils

import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import spinal.core._

object AxiInit{
  //init axi4 slave bus
  def apply(bus: Axi4): Unit ={
    val ar = bus.ar
    val r = bus.r
    val aw = bus.aw
    val w = bus.w
    val b = bus.b
    ar.ready #= false
    aw.ready #= false
    w.ready #= false
    r.valid #= false
    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0

    b.valid #= false
    if (b.config.useId) b.id #= 0
    if (b.config.useResp) b.resp #= 0
    if (b.config.useBUser) b.user #= 0
  }

  //AxiReadOnly init slave bus
  def apply(bus:Axi4ReadOnly,readOnly:Boolean) = {
    require(readOnly)
    val ar = bus.ar
    val r = bus.r
    ar.ready #= false
    r.valid #= false
    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0
  }
}

object DRAMAxiDriver{

  def read(bus:Axi4,address:Int,clockDomain: ClockDomain) = {
    bus.ar.valid #= true
    bus.ar.addr #= address
    clockDomain.waitSamplingWhere(bus.ar.ready.toBoolean)

    bus.ar.valid #= false
    bus.r.ready #= true
    clockDomain.waitSampling()
    bus.r.payload.data.toBigInt
  }

  def write(bus:Axi4,address:Int,data:BigInt,clockDomain: ClockDomain) = {
    bus.aw.valid #= true
    bus.aw.addr #= address
    clockDomain.waitSamplingWhere(bus.aw.ready.toBoolean)

    bus.aw.valid #= false
    bus.w.valid #= true
    bus.w.data #= data
    bus.w.last #= true
    clockDomain.waitSampling()

    bus.w.valid #= false
    bus.w.last #= false
    bus.b.ready #= true
    clockDomain.waitSampling()
  }

}