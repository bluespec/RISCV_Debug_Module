package Debug_Triage;
// We assume that only one debug module transaction happens at a time, so e.g. there is never
// any danger of head-of-queue blocking.

// Lots more validity checking could be done.

import AXI4_Types ::*;
import Fabric_Defs ::*;
import ClientServer ::*;
import DM_CPU_Req_Rsp ::*;
import ISA_Decls ::*;
import Connectable ::*;
import GetPut ::*;
import Semi_FIFOF ::*;

interface Triage_Ifc;
   interface Server #(DM_Sys_Req, DM_Sys_Rsp) server;
endinterface

module mkDebugTriage #(
     Server #(Bool, Bool) reset_svr
   , Server #(Bool, Bool) runHalt_svr,
   , Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN))  gpr
`ifdef ISA_F
   , Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(XLEN))  fpr
`endif
   , Server #(DM_CPU_Req #(12,  XLEN), DM_CPU_Rsp #(XLEN)) csr
   , Server #(SB_Sys_Req, SB_Sys_Rsp) mem
   ) (Triage_Ifc);


   FIFOF #(DM_Sys_Req) ff_dm_sys_req <- mkFIFOF;
   FIFOF #(DM_Sys_Rsp) ff_dm_sys_rsp <- mkFIFOF;

   rule rl_rst_req (ff_dm_sys_req.first matches tagged RST .req);
      reset_svr.request.put (req);
      ff_dm_sys_req.deq;
   endrule

   rule rl_runhalt_req (ff_dm_sys_req.first matches tagged HLT .req);
      runHalt_svr.request.put (req);
      ff_dm_sys_req.deq;
   endrule

   rule rl_sb_req (ff_dm_sys_req.first matches tagged SB .req);
      mem.request.put (req);
      ff_dm_sys_req.deq;
   endrule

   rule rl_gpr_req (ff_dm_sys_req.first matches tagged GPR .req);
      gpr.request.put (req);
      ff_dm_sys_req.deq;
   endrule

   rule rl_csr_req (ff_dm_sys_req.first matches tagged CSR .req);
      csr.request.put (req);
      ff_dm_sys_req.deq;
   endrule

`ifdef ISA_F
   rule rl_fpr_req (ff_dm_sys_req.first matches tagged FPR .req);
      fpr.request.put (req);
      ff_dm_sys_req.deq;
   endrule
`endif

   // sbus response
   rule rl_sbus_rsp;
      let rsp <- mem.response.get ();
      ff_dm_sys_rsp.enq (tagged SB rsp);
   endrule

   // gpr response
   rule rl_gpr_rsp;
      let rsp <- gpr.response.get ();
      ff_dm_sys_rsp.enq (tagged GPR rsp);
   endrule

   // csr response
   rule rl_csr_rsp;
      let rsp <- csr.response.get ();
      ff_dm_sys_rsp.enq (tagged CSR rsp);
   endrule

`ifdef ISA_F
   // fpr response
   rule rl_fpr_rsp;
      let rsp <- fpr.response.get ();
      ff_dm_sys_rsp.enq (tagged FPR rsp);
   endrule
`endif

   // reset response:
   rule rl_reset_rsp;
      let rsp <- reset_svr.response.get;
      ff_dm_sys_rsp.enq (tagged RST rsp);
   endrule

   // run/halt response:
   rule rl_runhalt_rsp;
      let rsp <- runHalt_svr.response.get ();
      ff_dm_sys_rsp.enq (tagged HLT rsp);
   endrule

   interface server = toGPServer (ff_dm_sys_req, ff_dm_sys_rsp);
endmodule

endpackage
