package Debug_Triage;

// ================================================================
// This package directs an incoming server request from the debug 
// module to the approriate server channel in the CPU. 
//
// Assumption: only one debug module transaction happens at a time,
// so e.g. there is never any danger of head-of-queue blocking.
//
// ================================================================
// BSV library imports

import FIFOF         :: *;
import ClientServer  ::*;
import Connectable   ::*;
import GetPut        ::*;

// ================================================================
// BSV Project Imports

import DM_CPU_Req_Rsp::*;
import ISA_Decls     ::*;
import Semi_FIFOF    ::*;
import Cur_Cycle     ::*;

// ================================================================
//
interface Triage_Ifc;
   interface Server #(DM_Sys_Req, DM_Sys_Rsp) server;
endinterface

// ================================================================
//
module mkDebugTriage #(
     Server #(Bool, Bool) reset_svr
   , Server #(Bool, Bool) runHalt_svr
   , Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN))  gpr
`ifdef ISA_F
   , Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(XLEN))  fpr
`endif
   , Server #(DM_CPU_Req #(12,  XLEN), DM_CPU_Rsp #(XLEN)) csr
   , Server #(SB_Sys_Req, SB_Sys_Rsp) mem
   ) (Triage_Ifc);

   Bit #(2) verbosity = 0; // 0: quiet; 1: rule firings; 2: details

   FIFOF #(DM_Sys_Req) ff_dm_sys_req <- mkFIFOF;
   FIFOF #(DM_Sys_Rsp) ff_dm_sys_rsp <- mkFIFOF;

   rule rl_rst_req (ff_dm_sys_req.first matches tagged RST .req);
      reset_svr.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_rst_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule

   rule rl_runhalt_req (ff_dm_sys_req.first matches tagged HLT .req);
      runHalt_svr.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_runhalt_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule

   rule rl_sb_req (ff_dm_sys_req.first matches tagged SB .req);
      mem.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_sb_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule

   rule rl_gpr_req (ff_dm_sys_req.first matches tagged GPR .req);
      gpr.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_gpr_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule

   rule rl_csr_req (ff_dm_sys_req.first matches tagged CSR .req);
      csr.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_csr_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule

`ifdef ISA_F
   rule rl_fpr_req (ff_dm_sys_req.first matches tagged FPR .req);
      fpr.request.put (req);
      ff_dm_sys_req.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_fpr_req", cur_cycle);
         if (verbosity > 1) 
            $display ("    req: ", fshow (req));
      end
   endrule
`endif

   // sbus response
   rule rl_sbus_rsp;
      let rsp <- mem.response.get ();
      ff_dm_sys_rsp.enq (tagged SB rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_sbus_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule

   // gpr response
   rule rl_gpr_rsp;
      let rsp <- gpr.response.get ();
      ff_dm_sys_rsp.enq (tagged GPR rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_gpr_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule

   // csr response
   rule rl_csr_rsp;
      let rsp <- csr.response.get ();
      ff_dm_sys_rsp.enq (tagged CSR rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_csr_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule

`ifdef ISA_F
   // fpr response
   rule rl_fpr_rsp;
      let rsp <- fpr.response.get ();
      ff_dm_sys_rsp.enq (tagged FPR rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_fpr_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule
`endif

   // reset response:
   rule rl_rst_rsp;
      let rsp <- reset_svr.response.get;
      ff_dm_sys_rsp.enq (tagged RST rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_rst_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule

   // run/halt response:
   rule rl_runhalt_rsp;
      let rsp <- runHalt_svr.response.get ();
      ff_dm_sys_rsp.enq (tagged HLT rsp);
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_runhalt_rsp", cur_cycle);
         if (verbosity > 1) 
            $display ("    rsp: ", fshow (rsp));
      end
   endrule

   interface server = toGPServer (ff_dm_sys_req, ff_dm_sys_rsp);
endmodule

endpackage
