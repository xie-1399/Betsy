package Untils


import spinal.core.sim.{SimConfig,SpinalSimConfig}

/* without the VCS support */

object CI {
  val simCfg = SimConfig
    .withWave
    .withVerilator
    .workspacePath("simulation")
}

object FST{
  val simCfg = SimConfig
    .withFstWave
    .withVerilator
    .workspacePath("simulation")
}

object SIMCFG{
  def apply(gtkFirst:Boolean = false): SpinalSimConfig = {
    if(gtkFirst) CI.simCfg else FST.simCfg
  }
}

