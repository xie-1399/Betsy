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

/* actor use for the lock (Abandon)*/
case class Actor[T <: Data](gen:HardType[T]) extends Bundle with IMasterSlave {
  val dataIn = Stream(gen)
  val dataOut = Stream(gen)
  override def asMaster(): Unit = {
    master(dataOut)
    slave(dataIn)
  }
}

/** Lock ==================>
 ** held (the lock is on , if held other actor must wait)
 ** by (hold the actor id with the lock on)
 ** cond (the condition used to release the lock )
 ** delayRelease (satisfy the condition and delay some cycles to release the lock) */

/** LockControl ==================>
 ** lock (the lock id)
 ** by (the actor id want to get lock)
 ** acquire (get the lock)
 ** cond (the condition used to release the lock)
 ** delayRelease (satisfy the condition and delay some cycles to release the lock) */


class LockPool[T <: Data](gen:HardType[T],numActors:Int,numLocks:Int,select:T => UInt) extends BetsyModule{
  val maxDelay = 1 << 4
  val io = new Bundle{
    val actor = Vec(master(Actor(gen)),numActors)
    val lockControl = slave(Stream(new ConditionalReleaseLockControl(gen,numActors,numLocks,maxDelay)))
    val lockedControl = master(Stream(new ConditionalReleaseLockControl(gen,numActors,numLocks,maxDelay)))
    val deadlocked = master Stream Bool()
  }

  val zero = Until.zero(gen())
  val lock = Vec(Reg(new ConditionalReleaseLock(gen,numActors,maxDelay))
    .init(ConditionalReleaseLock(gen,numActors,maxDelay)),numLocks)
  val actor = io.actor.map(_.dataIn)

  /* get the lock from the lock controller */
  def acquire(l: ConditionalReleaseLock[T]): Unit = {
    when(io.lockControl.valid) {
      l.held := io.lockControl.payload.acquire
      l.by := io.lockControl.payload.by
      l.cond <> io.lockControl.payload.cond
    }
  }

  def release(l: ConditionalReleaseLock[T]): Unit = {
    l.held := False
  }

  val default = new Area {
    val block = for((a,id) <- actor.zipWithIndex) yield {
      io.actor(id).dataOut.valid := False
      io.actor(id).dataOut.payload := zero
      a.ready := False
      val requiredLockId = select(a.payload)
      val requiredLock = lock(requiredLockId)
      val blocked = (requiredLock.held && requiredLock.by =/= U(id)) ||
        (io.lockControl.fire && io.lockControl.payload.lock === requiredLockId && io.lockControl.payload.by =/= U(id))
      when(!blocked) {
        io.actor(id).dataOut.connectFrom(a)
      }
      blocked
    }
    io.lockedControl.valid := io.lockControl.fire
    io.lockedControl.payload <> io.lockControl.payload
    io.deadlocked.valid := False /* instead of Idle*/
    io.deadlocked.payload := False
  }

  val incomingObserved = io.lockControl.valid && actor(io.lockControl.by).fire && actor(io.lockControl.by).payload === io.lockControl.cond
  val requestLock = lock(io.lockControl.payload.lock)
  io.lockControl.ready := !(requestLock.held && io.lockControl.payload.by =/= requestLock.by)

  val logic = new Area{
    for ((l, id) <- lock.zipWithIndex) {
      val incoming = io.lockControl.payload.lock === id
      val observed = actor(l.by).fire && actor(l.by).payload === l.cond
      when(l.held) {
        when(observed) {
          when(incoming) {
            when(incomingObserved) {
              release(l)
            }.otherwise {
              // release then immediately try to acquire
              release(l)
              acquire(l)
            }
          }.otherwise {
            release(l)
          }
        }.otherwise {
          when(incoming) {
            when(l.by === io.lockControl.payload.by) {
              // same actor acquiring lock, so allowed
              acquire(l)
            }
          }
        }
      }.otherwise {
        when(incoming) {
          when(!incomingObserved) {
            acquire(l)
          }
        }
      }
    }
  }

  when(default.block.reduce((a, b) => a && b)) {
    io.actor(lock(0).by).dataOut.connectFrom(actor(lock(0).by))
    // signal out when deadlock happens so we know that something is wrong
    io.deadlocked.payload.set()
    io.deadlocked.valid.set()
  }
}

object LockPool extends App{
  def select[T <: Data](a:T):UInt = U(0)
  SpinalSystemVerilog(new LockPool(Bits(4 bits),2,1,select))
}