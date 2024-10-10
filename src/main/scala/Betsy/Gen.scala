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
  // SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(AFix(1 exp, -2 exp, true),Architecture.embeddings())).printPruned()

  // tiny arch
  // SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(AFix(3 exp, -4 exp, true),Architecture.tiny())).printPruned()

  // normal arch
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(AFix(7 exp, -8 exp, true),Architecture.normal())).printPruned()

  // large arch
  // SpinalVerilog(SpinalConfig().withoutEnumString())(new Top(AFix(7 exp, -8 exp, true),Architecture.large())) printPruned()
}
