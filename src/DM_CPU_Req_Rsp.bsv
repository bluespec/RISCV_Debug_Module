// Copyright (c) 2017-2021 Bluespec, Inc. All Rights Reserved.

package DM_CPU_Req_Rsp;

// ================================================================
// This package defines types for register access request and response

// ================================================================
// BSV library imports

// None

// ================================================================
// Project imports

// None

// ================================================================
// Requests and responses

typedef struct {
   Bool      write;
   Bit #(a)  address;
   Bit #(d)  data;
} DM_CPU_Req #(numeric type a, numeric type d)
deriving (Bits, Eq, FShow);

typedef struct {
   Bool     ok;
   Bit #(d) data;
} DM_CPU_Rsp #(numeric type d)
deriving (Bits, Eq, FShow);

// ================================================================
//
// DM system request and reponse types
//
typedef struct {
   Bool        read_not_write;
   DM_Word     wdata;
   DM_sbaccess size;
`ifdef RV32
   Bit #(32)   addr;
`endif
`ifdef RV64
   Bit #(64)   addr;
`endif
} SB_Sys_Req deriving (Bits, FShow);

typedef struct {
   DM_Word     rdata;
   Bool        read_not_write;
   Bool        err;
} SB_Sys_Rsp deriving (Bits, FShow);

typedef union tagged {
   Bool                    RST;
   Bool                    HLT;
   DM_CPU_Req #(5, XLEN)   GPR; 
   DM_CPU_Req #(12, XLEN)  CSR; 
`ifdef ISA_F
   DM_CPU_Req #(5, FLEN)   FPR; 
`endif
   SB_Sys_Req              SB;
} DM_Sys_Req deriving (Bits, FShow);

typedef union tagged {
   Bool                    RST;
   Bool                    HLT;
   DM_CPU_Rsp #(XLEN)      GPR; 
   DM_CPU_Rsp #(XLEN)      CSR; 
`ifdef ISA_F
   DM_CPU_Rsp #(FLEN)      FPR; 
`endif
   SB_Sys_Rsp              SB;
} DM_Sys_Rsp deriving (Bits, FShow);

endpackage
