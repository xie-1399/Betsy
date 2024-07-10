package Betsy

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/7/8      SpinalHDL Version: 1.94       **
 ** You should have received a copy of the MIT License along with this library **
 ** generate the verilog with different config files(tiny + normal = large)**
 */

import spinal.core._


object Gen extends App{
  // tiny arch
  SpinalVerilog(new Top(SInt(8 bits),Architecture.tiny()))


  // normal arch


  // large arch

}
