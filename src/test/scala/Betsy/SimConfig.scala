package Betsy

import spinal.lib.bus.amba4.axi.sim.AxiMemorySimConfig

/* some sim constant for the axi */

object SimConfig {
  /* the axi4 memory sim */
  val axiconfig = AxiMemorySimConfig(maxOutstandingReads = 8,
    maxOutstandingWrites = 8,
    readResponseDelay = 8,
    writeResponseDelay = 8,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )

}
