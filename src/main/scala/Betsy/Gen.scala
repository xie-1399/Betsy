package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/7/8      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** generate the verilog with different config files(tiny + normal = large)**
 */

import spinal.core._

// generate verilog demo ( can use define works )

object Gen extends App{

  //embedding arch
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(SInt(4 bits),Architecture.embeddings())).printPruned()

  // tiny arch
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(SInt(8 bits),Architecture.tiny())).printPruned()

  // normal arch
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(SInt(16 bits),Architecture.normal())).printPruned()

  // large arch
  // SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(SInt(16 bits),Architecture.large())) printPruned()
}
