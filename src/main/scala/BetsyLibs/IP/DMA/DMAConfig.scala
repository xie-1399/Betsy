package BetsyLibs.IP.DMA

/* the DMA Config decide the DMA Bus and Registers */
/* the control bus is APB and the data bus is AXI4 */

class DMAConfig( val controlBus:String = "APB3",
                 val dataBus:String = "AXI4",
                 val addrWidth: Int = 32,
                 val readDataWidth: Int = 32,
                 val writeDataWidth: Int = 32,
                 val readMaxBurst: Int = 0,
                 val writeMaxBurst: Int = 16,
                 val reader4KBarrier: Boolean = false,
                 val writer4KBarrier: Boolean = true,
                 val controlDataWidth: Int = 32,
                 val controlAddrWidth: Int = 32,
                 val controlRegCount: Int = 16, /* reg number in the dma control */
                 val fifoDepth: Int = 512) {
  assert(readDataWidth == writeDataWidth,"DMA Read and Write Width Should be equal!!!")
}

object Register {
  /* all registers address list*/
  val Ctrl = 0x00
  val Status = 0x04
  val InterruptMask = 0x08
  val InterruptStatus = 0x0c
  val ReaderStartAddr = 0x10
  val ReaderLineLen = 0x14
  val ReaderLineCnt = 0x18
  val ReaderStride = 0x1c
  val WriterStartAddr = 0x20
  val WriterLineLen = 0x24
  val WriterLineCnt = 0x28
  val WriterStride = 0x2c
  val Version = 0x30
  val Configuration = 0x34
}