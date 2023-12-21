package Untils


import spinal.core.sim._
import spinal.sim._

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

object VCS{
  /* support for the vcs 2018 */
  val flags = VCSFlags(
    compileFlags = List("-kdb"),
    elaborateFlags = List("-kdb","-LDFLAGS -Wl,--no-as-needed")
  )
  val simCfg = SimConfig
    .withVCS(flags)
    .withFSDBWave
    .workspacePath("simulation")
}


object SIMCFG{
  def apply(gtkFirst:Boolean = false): SpinalSimConfig = {
    sys.env.get("VCS_HOME") match {
      case Some(_) => if(gtkFirst) FST.simCfg else VCS.simCfg
      case None => if(gtkFirst) CI.simCfg else FST.simCfg
  }
}

