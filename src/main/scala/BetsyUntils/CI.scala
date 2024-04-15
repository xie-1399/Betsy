package BetsyUntils

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/4/6      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** the CI is used to choose the sim tools (vcs or verilator) **
 */

import spinal.core.sim.{SimConfig,SpinalSimConfig}
import spinal.sim.VCSFlags

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
    .allOptimisation
    .workspacePath("simulation")
}

object VCS{

  val version = 2016

  val flags = if(version == 2018)VCSFlags(
    compileFlags = List("-kdb"),
    elaborateFlags = List("-kdb","-LDFLAGS -Wl,--no-as-needed")
  )else{
    /* if only gen the wave , no timing checking for the frontend simulation */
    VCSFlags(
      compileFlags = List("-kdb", "-cpp g++-4.8", "-cc gcc-4.8", "+define+UNIT_DELAY", "+define+no_warning","+nospecify","+notimingcheck"),
      elaborateFlags = List("-kdb", "-lca", "-cpp g++-4.8", "-cc gcc-4.8", "-LDFLAGS -Wl,--no-as-needed")
    )
  }
  val simCfg = SimConfig
    .withVCS(flags)
    .withFSDBWave
    .allOptimisation
    .withLogging
    .workspacePath("simulation")
}

object SIMCFG{
  def apply(gtkFirst:Boolean = false,compress:Boolean = false): SpinalSimConfig = {
    sys.env.get("VCS_HOME") match {
      case Some(_) => {
        (gtkFirst, compress) match {
          case (true,true) => FST.simCfg
          case (false,true) => FST.simCfg
          case (true,false) => CI.simCfg
          case (false,false) => VCS.simCfg
        }
      }
      case None => CI.simCfg
    }
  }
}
