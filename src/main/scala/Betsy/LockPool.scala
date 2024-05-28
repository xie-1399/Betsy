package Betsy

import Betsy.Until.BetsyModule
import spinal.lib._
import spinal.core._

/**
 ** Betsy follow the MiT Licence.(c) xxl, All rights reserved **
 ** Update Time : 2024/5/25      SpinalHDL Version: 1.94      **
 ** You should have received a copy of the MIT License along with this library **
 ** **
 */

/* actor use for the lock */
case class Actor[T <: Data](gen:HardType[T]) extends Bundle with IMasterSlave {
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)
  override def asMaster(): Unit = {
    master(dataOut)
    slave(dataIn)
  }
}


class LockPool[T <: Data](gen:HardType[T],numActors:Int,numLocks:Int,select:T => UInt) extends BetsyModule{
  val maxDelay = 1 << 4
  val io = new Bundle{
    val actor = Vec(master(Actor(gen)),numActors)
    val lock = slave(Stream(new ConditionalReleaseLockControl(gen,numActors,numLocks,maxDelay)))
    val locked = master(Stream(new ConditionalReleaseLockControl(gen,numActors,numLocks,maxDelay)))
    val deadlocked = master Stream Bool()
  }

  val zero = Until.zero(gen())
  val lock = Vec(Reg(new ConditionalReleaseLock(gen,numActors,maxDelay))
    .init(ConditionalReleaseLock(gen,numActors,maxDelay)),numLocks)
  val actor = io.actor.map(_.dataIn)
  val lockControl = io.lock

  val default = new Area {
    val block = for((a,id) <- actor.zipWithIndex) yield {
      io.actor(id).dataOut.valid := False
      io.actor(id).dataOut.payload := zero
      a.ready := False
      val requiredLockId = select(a.payload)
      val requiredLock = lock(requiredLockId)
      val blocked = (requiredLock.held && requiredLock.by =/= U(id)) || (lockControl.fire && lockControl.payload.lock === requiredLockId && lockControl.payload.by =/= U(id))
      when(!blocked) {
        io.actor(id).dataOut.connectFrom(a)
      }
      blocked
    }
    io.locked.valid := io.lock.fire
    io.locked.payload <> io.lock.payload
    io.deadlocked.valid := False /* instead of Idle*/
    io.deadlocked.payload := False
  }

}


object LockPool extends App{
  def select[T <: Data](a:T):UInt = U(0)
  SpinalSystemVerilog(new LockPool(Bits(4 bits),2,1,select))
}