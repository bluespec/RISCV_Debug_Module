// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package BSDebug;

// ================================================================
// This package integrates the JTag TAP Controller with the RISC-V
// debug module.
//
// Requests from the different client interfaces of the DM are 
// combined here into a single client request to the core. Similarly
// responses from the core are routed to the appropriate client in
// the DM.
//
// ================================================================
// BSV library imports

import Vector           :: *;
import FIFO             :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import Bus              :: *;
import Clocks           :: *;
import RegUInit         :: *;

// ----------------
// BSV additional libs

import GetPut_Aux       :: *;
import Semi_FIFOF       :: *;

// ================================================================
// Project imports

import SoC_Map          :: *;

import Debug_Module     :: *;
import Debug_Interfaces :: *;
import DM_CPU_Req_Rsp   :: *;
import Jtag             :: *;
import JtagTap          :: *;
import Giraffe_IFC      :: *;

// ================================================================
// Constant: cycles to hold SoC in reset for ndm reset:

UInt#(6) ndm_interval = 20;
UInt#(6) por_interval = 20;

// ================================================================
// The BSCore interface

interface BSDebug_IFC;
   interface Client #(DM_Sys_Req, DM_Sys_Rsp) toCore;
   interface Reset ndm_resetn;
   interface JTAG_IFC jtag;
endinterface

// ================================================================

// reset by power-on reset
(* synthesize *)
module mkBSDebug ((*reset="dmi_reset"*)Reset dmi_reset, BSDebug_IFC _ifc);
   let dmi_resetN <- mkResetInverter(dmi_reset); // dmi_reset is active-high

   // ----------------

   let debug_module <- mkDebug_Module;

   // ----------------
   // Instantiate JTAG TAP controller,
   // connect to core.dm_dmi;
   // and export its JTAG interface

   Wire#(Bit#(7)) w_dmi_req_addr <- mkDWire(0);
   Wire#(Bit#(32)) w_dmi_req_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_req_op <- mkDWire(0);

   Wire#(Bit#(32)) w_dmi_rsp_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_rsp_response <- mkDWire(0);

   BusReceiver#(Tuple3#(Bit#(7),Bit#(32),Bit#(2))) bus_dmi_req <- mkBusReceiver(reset_by dmi_resetN);
   BusSender#(Tuple2#(Bit#(32),Bit#(2))) bus_dmi_rsp <- mkBusSender(unpack(0), reset_by dmi_resetN);

   let jtagtap <- mkJtagTap(reset_by dmi_resetN);

   mkConnection(jtagtap.dmi.req_ready, pack(bus_dmi_req.in.ready), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_valid, compose(bus_dmi_req.in.valid, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_addr, w_dmi_req_addr._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_data, w_dmi_req_data._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_op, w_dmi_req_op._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_valid, pack(bus_dmi_rsp.out.valid), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_ready, compose(bus_dmi_rsp.out.ready, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_data, w_dmi_rsp_data, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_response, w_dmi_rsp_response, reset_by dmi_resetN);

   rule rl_dmi_req;
      bus_dmi_req.in.data(tuple3(w_dmi_req_addr, w_dmi_req_data, w_dmi_req_op));
   endrule

   rule rl_dmi_rsp;
      match {.data, .response} = bus_dmi_rsp.out.data;
      w_dmi_rsp_data <= data;
      w_dmi_rsp_response <= response;
   endrule

   (* preempts = "rl_dmi_req_cpu, rl_dmi_rsp_cpu" *)
   rule rl_dmi_req_cpu;
      match {.addr, .data, .op} = bus_dmi_req.out.first;
      bus_dmi_req.out.deq;
      case (op)
	 1: debug_module.dmi.read_addr(addr);
	 2: begin
	       debug_module.dmi.write(addr, data);
	       bus_dmi_rsp.in.enq(tuple2(?, 0));
	    end
	 default: bus_dmi_rsp.in.enq(tuple2(?, 2));
      endcase
   endrule

   rule rl_dmi_rsp_cpu;
      let data <- debug_module.dmi.read_data;
      bus_dmi_rsp.in.enq(tuple2(data, 0));
   endrule

   // ----------------
   // Reset behaviour:
   // ndm-reset resets hart and also asserts separate ndm_resetn signal.

   Bool isNDMreset  = True;
   Bool isHARTreset = !isNDMreset;

   FIFO #(Bool) ff_rst_req   <- mkFIFO1;
   FIFO #(Bool) ff_rst_rsp   <- mkFIFO1;
   FIFO #(Bool) ff_which_rst <- mkFIFO1;

   let clk      <- exposeCurrentClock();
   let resetIfc <- mkReset(2, True, clk);
   let ndm_rstn  = resetIfc.new_rst;

   rule hart_reset_rl;
      let running <- debug_module.hart0.hart_reset_client.request.get();
      ff_rst_req.enq(running);
      ff_which_rst.enq(isHARTreset);
   endrule

   rule ndm_reset_rl;
      let running <- debug_module.ndm_reset_client.request.get();
      ff_rst_req.enq(running);
      ff_which_rst.enq(isNDMreset);
      resetIfc.assertReset;
   endrule

   rule reset_rsp_rl;
      let running <- toGet(ff_rst_rsp).get();
      let which   <- toGet(ff_which_rst).get();
      if (which == isNDMreset)
	 debug_module.ndm_reset_client.response.put(running);
      else
	 debug_module.hart0.hart_reset_client.response.put(running);
   endrule

   // ----------------
   // DM Client Channels
   
   FIFO #(DM_Sys_Req) ff_dm_sys_req <- mkFIFO;
   FIFO #(DM_Sys_Rsp) ff_dm_sys_rsp <- mkFIFO;

   Reg #(Bit #(32))  rg_sb_writes   <- mkReg (0);
   Reg #(Bool)       rg_busy        <- mkReg(False);

`ifdef ISA_F
   (* mutually_exclusive = "rl_sbus_req, rl_gpr_req, rl_csr_req, rl_fpr_req" *)
`else
   (* mutually_exclusive = "rl_sbus_req, rl_gpr_req, rl_csr_req" *)
`endif
   rule rl_sbus_req;
      let sb_req <- debug_module.sb_client.request.get ();
      ff_dm_sys_req.enq (tagged SB sb_req);
      rg_busy <= True;

      if (!sb_req.read_not_write) begin
         // Writes can burst
         rg_sb_writes <= rg_sb_writes + 1;
      end
   endrule

   rule rl_reset_req (!rg_busy);
      let running = ff_rst_req.first; ff_rst_req.deq;
      ff_dm_sys_req.enq (tagged RST running);
      rg_busy <= True;
   endrule

   rule rl_runhalt_req (!rg_busy);
      let running <- debug_module.hart0.hart_client_run_halt.request.get();
      ff_dm_sys_req.enq (tagged HLT running);
      rg_busy <= True;
   endrule

   rule rl_gpr_req (!rg_busy);
      let req <- debug_module.hart0.hart_gpr_mem_client.request.get();
      ff_dm_sys_req.enq (tagged GPR req);
      rg_busy <= True;
   endrule

   rule rl_csr_req (!rg_busy);
      let req <- debug_module.hart0.hart_csr_mem_client.request.get();
      ff_dm_sys_req.enq (tagged CSR req);
      rg_busy <= True;
   endrule

`ifdef ISA_F
   rule rl_fpr_req (!rg_busy);
      let req <- debug_module.hart0.hart_fpr_mem_client.request.get();
      ff_dm_sys_req.enq (tagged FPR req);
      rg_busy <= True;
   endrule
`endif

   rule rl_sb_rsp (ff_dm_sys_rsp.first matches tagged SB .rsp);
      debug_module.sb_client.response.put (rsp);
      ff_dm_sys_rsp.deq;

      // For read responses, clear busy flag
      if (rsp.read_not_write) rg_busy <= False;

      // Write responses
      else begin
         // Decrement write response counter
         // If this is thae last write response, clear the busy flag
         rg_sb_writes <= rg_sb_writes - 1;
         if (rg_sb_writes == 1) rg_busy <= False;
      end
   endrule

   rule rl_gpr_rsp (ff_dm_sys_rsp.first matches tagged GPR .rsp);
      debug_module.hart0.hart_gpr_mem_client.response.put (rsp);
      ff_dm_sys_rsp.deq;
      rg_busy <= False;
   endrule

   rule rl_csr_rsp (ff_dm_sys_rsp.first matches tagged CSR .rsp);
      debug_module.hart0.hart_csr_mem_client.response.put (rsp);
      ff_dm_sys_rsp.deq;
      rg_busy <= False;
   endrule

`ifdef ISA_F
   rule rl_fpr_rsp (ff_dm_sys_rsp.first matches tagged FPR .rsp);
      debug_module.hart0.hart_fpr_mem_client.response.put (rsp);
      ff_dm_sys_rsp.deq;
      rg_busy <= False;
   endrule
`endif

   rule rl_rst_rsp (ff_dm_sys_rsp.first matches tagged RST .rsp);
      ff_rst_rsp.enq (rsp);
      ff_dm_sys_rsp.deq;
      rg_busy <= False;
   endrule

   rule rl_runhalt_rsp (ff_dm_sys_rsp.first matches tagged HLT .rsp);
      debug_module.hart0.hart_client_run_halt.response.put (rsp);
      ff_dm_sys_rsp.deq;
      rg_busy <= False;
   endrule

   interface toCore = toGPClient (ff_dm_sys_req, ff_dm_sys_rsp);
   interface Reset ndm_resetn = ndm_rstn;
   interface JTAG_IFC jtag = jtagtap.jtag;
 endmodule

// ================================================================

endpackage
