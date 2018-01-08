//=======================================================================
// RISCV, A Simple As Possible Core
// Watson Huang
// Dec 16, 2016
// 
// Magic RAM for rvcore, has multiple ports and read data immediately
//=======================================================================
package rvcommon

import chisel3._
import chisel3.util._

//Ref: https://github.com/freechipsproject/chisel3/wiki/Memories
//Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/common/memory.scala

trait mram_op {

    val MF_X = UInt(0,1)
    val MF_RD = UInt(0,1)
    val MF_WR = UInt(1,1)

    val MT_X = UInt(3,2)
    val MT_B = UInt(1,2)
    val MT_H = UInt(2,2)
    val MT_W = UInt(3,2)
}

trait mram_def {
    val mram_io_width = 32
    val mram_base_width = 8
    val mram_size = 1024 //In slots, currently each slot is 32-bit(Word)
}

object mram_op extends mram_op
object mram_def extends mram_def

class mram_req(data_width: Int) extends Bundle {
    val addr = Output(UInt(rvspec.xlen.W))
    val data = Output(UInt(data_width.W))
    //Use pre-defined data width
    //Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/common/memory.scala
    val mfunc = Output(UInt(mram_op.MF_X.getWidth.W)) 
    val mtype = Output(UInt(mram_op.MT_X.getWidth.W))
    val valid = Output(Bool())
    val ready = Input(Bool())

    //Solve cloneType Error!?
    //Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/common/memory.scala
    override def cloneType = { new mram_req(data_width).asInstanceOf[this.type] }
}

class mram_resp(data_width: Int) extends Bundle {
    val data = Output(UInt(data_width.W))
    val valid = Output(Bool())
    
    //Solve cloneType Error!?
    //Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/common/memory.scala
    override def cloneType = { new mram_resp(data_width).asInstanceOf[this.type] }
}

class mram_io(data_width: Int) extends Bundle {
    val req = new mram_req(data_width)
    val resp = Flipped(new mram_resp(data_width))
    
    //Solve cloneType Error!?
    //Ref: https://github.com/ucb-bar/riscv-sodor/blob/master/src/common/memory.scala
    override def cloneType = { new mram_io(data_width).asInstanceOf[this.type] }
}

//Make a simple asynchronous ram for core functional validation
//Build with Mask ability, ref: https://github.com/freechipsproject/chisel3/wiki/Memories
class mram_async extends Module {
    
    val mask_size:Int = ((mram_def.mram_io_width+(mram_def.mram_base_width-1))/mram_def.mram_base_width)
    //Ref: https://github.com/freechipsproject/chisel3/blob/master/src/main/scala/chisel3/util/Math.scala
    val addr_lo = log2Up((mram_def.mram_io_width)/mram_def.mram_base_width)
    val addr_hi = log2Up(mram_def.mram_size) + addr_lo - 1

    //println("mram_mask_size:%d".format(mask_size)) //Check magic ram mask size should be 4 at RV32I
    //println("addr_hi:%d, addr_lo:%d".format(addr_hi, addr_lo))

    val io = IO(new Bundle {
        val inst_port = Flipped(new mram_io(rvspec.xlen))
        val data_port = Flipped(new mram_io(rvspec.xlen))
        val dbg_port = Flipped(new mram_io(rvspec.xlen))
    })
    
    //Calculate slots in mram_size by slot width is mram_io_width
    val amem = Mem(mram_def.mram_size, Vec(mask_size, UInt(mram_def.mram_base_width.W)))

    //Ref: https://github.com/freechipsproject/chisel3/wiki/Memories#masks
    //val data_out = Wire(Vec(mask_size, UInt(mram_def.mram_base_width.W)))
    //val data_in = Wire(Vec(mask_size, UInt(mram_def.mram_base_width.W)))
    //val mask = Wire(Vec(mask_size, Bool()))

    //inst. port, only read
    io.inst_port.req.ready := true.B
    io.inst_port.resp.valid := false.B
    when(io.inst_port.req.ready) {
        when(io.inst_port.req.valid) {
            switch(io.inst_port.req.mfunc) {
                is(mram_op.MF_RD) {
                    val addr = io.inst_port.req.addr(addr_hi, addr_lo)
                    io.inst_port.resp.data := amem.read(addr).asUInt
                    io.inst_port.resp.valid := true.B
                }
            }
        }
    }

    //data port, read/write
    io.data_port.req.ready := true.B
    io.data_port.resp.valid := false.B
    when(io.data_port.req.ready) {
        when(io.data_port.req.valid) {
            switch(io.data_port.req.mfunc) {
                is(mram_op.MF_WR) {
                    val addr = io.data_port.req.addr(addr_hi, addr_lo)
                    val data = io.data_port.req.data
                    val data_vec = Vec( data((mram_def.mram_base_width*1-1),mram_def.mram_base_width*0),    //data(07:00)
                                        data((mram_def.mram_base_width*2-1),mram_def.mram_base_width*1),    //data(15:08)
                                        data((mram_def.mram_base_width*3-1),mram_def.mram_base_width*2),    //data(23:16)
                                        data((mram_def.mram_base_width*4-1),mram_def.mram_base_width*3))    //data(31:24)
                                        
                    val data_mask = MuxLookup (
                        io.data_port.req.mtype,
                        //Ref: https://github.com/freechipsproject/chisel3/wiki/Cookbook#how-do-i-create-a-vec-of-bools-from-a-uint
                        Vec(UInt(0,mask_size).toBools),
                        Array(
                            mram_op.MT_B -> Vec(UInt(0x1,mask_size).toBools),
                            mram_op.MT_H -> Vec(UInt(0x3,mask_size).toBools),
                            mram_op.MT_W -> Vec(UInt(0xF,mask_size).toBools)
                        )
                    )

                    amem.write(addr, data_vec, data_mask)
                    //amem.write(io.data_port.req.addr, data_vec, data_mask)
                }
                is(mram_op.MF_RD) {
                    val addr = io.data_port.req.addr(addr_hi, addr_lo)
                    io.data_port.resp.data := amem.read(addr).asUInt
                    io.data_port.resp.valid := true.B
                }
            }
        }
    }

    //debug port, force read/write
    io.dbg_port.req.ready := true.B
    io.dbg_port.resp.valid := false.B
    //when(io.dbg_port.req.ready) {
        when(io.dbg_port.req.valid) {
            switch(io.dbg_port.req.mfunc) {
                is(mram_op.MF_WR) {
                    val addr = io.dbg_port.req.addr(addr_hi, addr_lo)
                    val data = io.dbg_port.req.data
                    val data_vec = Vec( data((mram_def.mram_base_width*1-1),mram_def.mram_base_width*0),    //data(07:00)
                                        data((mram_def.mram_base_width*2-1),mram_def.mram_base_width*1),    //data(15:08)
                                        data((mram_def.mram_base_width*3-1),mram_def.mram_base_width*2),    //data(23:16)
                                        data((mram_def.mram_base_width*4-1),mram_def.mram_base_width*3))    //data(31:24)

                    val data_mask = MuxLookup (
                        io.dbg_port.req.mtype,
                        Vec(UInt(0,mask_size).toBools),
                        Array(
                            mram_op.MT_B -> Vec(UInt(0x1,mask_size).toBools),
                            mram_op.MT_H -> Vec(UInt(0x3,mask_size).toBools),
                            mram_op.MT_W -> Vec(UInt(0xF,mask_size).toBools)
                        )
                    )

                    /*
                    //Debug at runtime
                    printf(p"$addr\n")
                    printf(p"$data\n")
                    printf(p"$data_vec\n")
                    printf(p"$data_mask\n")
                    */
                    amem.write(addr, data_vec, data_mask)
                }
                is(mram_op.MF_RD) {
                    val addr = io.dbg_port.req.addr(addr_hi, addr_lo)
                    io.dbg_port.resp.data := amem.read(addr).asUInt
                    io.dbg_port.resp.valid := true.B
                }
            }
        }
    //}

    when(reset.toBool()) {
        io.inst_port.req.ready := false.B
        io.inst_port.resp.valid := false.B
        io.inst_port.resp.data := 0.U
        io.data_port.req.ready := false.B
        io.data_port.resp.valid := false.B
        io.data_port.resp.data := 0.U
        io.dbg_port.req.ready := false.B
        io.dbg_port.resp.valid := false.B
        io.dbg_port.resp.data := 0.U
    }
}