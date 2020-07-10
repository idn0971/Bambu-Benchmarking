// 
// Politecnico di Milano
// Code created using PandA - Version: PandA 0.9.6 - Revision 5e5e306b86383a7d85274d64977a3d71fdcff4fe-master - Date 2020-06-29T15:52:39
// bambu executed with: bambu --compiler=I386_CLANG6 -O3 --device-name=EP2C70F896C6 ../cpu.c 
// 
// Send any bug to: panda-info@polimi.it
// ************************************************************************
// The following text holds for all the components tagged with PANDA_LGPLv3.
// They are all part of the BAMBU/PANDA IP LIBRARY.
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with the PandA framework; see the files COPYING.LIB
// If not, see <http://www.gnu.org/licenses/>.
// ************************************************************************

`ifdef __ICARUS__
  `define _SIM_HAVE_CLOG2
`endif
`ifdef VERILATOR
  `define _SIM_HAVE_CLOG2
`endif
`ifdef MODEL_TECH
  `define _SIM_HAVE_CLOG2
`endif
`ifdef VCS
  `define _SIM_HAVE_CLOG2
`endif
`ifdef NCVERILOG
  `define _SIM_HAVE_CLOG2
`endif
`ifdef XILINX_SIMULATOR
  `define _SIM_HAVE_CLOG2
`endif
`ifdef XILINX_ISIM
  `define _SIM_HAVE_CLOG2
`endif

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>, Christian Pilato <christian.pilato@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module constant_value(out1);
  parameter BITSIZE_out1=1, value=1'b0;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = value;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module register_SE(clock, reset, in1, wenable, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input [BITSIZE_in1-1:0] in1;
  input wenable;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  reg [BITSIZE_out1-1:0] reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock)
    if (wenable)
      reg_out1 <= in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module register_STD(clock, reset, in1, wenable, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input [BITSIZE_in1-1:0] in1;
  input wenable;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  reg [BITSIZE_out1-1:0] reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock)
    reg_out1 <= in1;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ADDRESS_DECODING_LOGIC_NN(clock, reset, in1, in2, in3, out1, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, Sout_Rdata_ram, S_data_ram_size, Sin_DataRdy, Sout_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1, dout_a, dout_b, memory_addr_a, memory_addr_b, din_value_aggregated_swapped, be_swapped, bram_write);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, USE_SPARSE_MEMORY=1, HIGH_LATENCY=0, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2, BITSIZE_dout_a=1, PORTSIZE_dout_a=2, BITSIZE_dout_b=1, PORTSIZE_dout_b=2, BITSIZE_memory_addr_a=1, PORTSIZE_memory_addr_a=2, BITSIZE_memory_addr_b=1, PORTSIZE_memory_addr_b=2, BITSIZE_din_value_aggregated_swapped=1, PORTSIZE_din_value_aggregated_swapped=2, BITSIZE_be_swapped=1, PORTSIZE_be_swapped=2, BITSIZE_bram_write=1, PORTSIZE_bram_write=2, nbit_read_addr=32, n_byte_on_databus=4, n_mem_elements=4, max_n_reads=2, max_n_writes=2, max_n_rw=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  input [(PORTSIZE_dout_a*BITSIZE_dout_a)+(-1):0] dout_a;
  input [(PORTSIZE_dout_b*BITSIZE_dout_b)+(-1):0] dout_b;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  output [(PORTSIZE_memory_addr_a*BITSIZE_memory_addr_a)+(-1):0] memory_addr_a;
  output [(PORTSIZE_memory_addr_b*BITSIZE_memory_addr_b)+(-1):0] memory_addr_b;
  output [(PORTSIZE_din_value_aggregated_swapped*BITSIZE_din_value_aggregated_swapped)+(-1):0] din_value_aggregated_swapped;
  output [(PORTSIZE_be_swapped*BITSIZE_be_swapped)+(-1):0] be_swapped;
  output [PORTSIZE_bram_write-1:0] bram_write;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_addr = BITSIZE_S_addr_ram/*n_bytes ==  1 ? 1 : $clog2(n_bytes)*/;
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 1 : $clog2(n_byte_on_databus);
    parameter nbits_address_space_rangesize = $clog2(address_space_rangesize);
  `else
    parameter nbit_addr = BITSIZE_S_addr_ram/*n_bytes ==  1 ? 1 : log2(n_bytes)*/;
    parameter nbits_address_space_rangesize = log2(address_space_rangesize);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 1 : log2(n_byte_on_databus);
  `endif
   parameter memory_bitsize = 2*BRAM_BITSIZE;
  
  function [n_byte_on_databus*max_n_writes-1:0] CONV;
    input [n_byte_on_databus*max_n_writes-1:0] po2;
  begin
    case (po2)
      1:CONV=(1<<1)-1;
      2:CONV=(1<<2)-1;
      4:CONV=(1<<4)-1;
      8:CONV=(1<<8)-1;
      16:CONV=(1<<16)-1;
      32:CONV=(1<<32)-1;
      default:CONV=-1;
    endcase
  end
  endfunction
  
  wire [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] tmp_addr;
  wire [n_byte_on_databus*max_n_writes-1:0] conv_in;
  wire [n_byte_on_databus*max_n_writes-1:0] conv_out;
  wire [PORTSIZE_S_addr_ram-1:0] cs;
  wire [PORTSIZE_S_oe_ram-1:0] oe_ram_cs;
  wire [PORTSIZE_S_we_ram-1:0] we_ram_cs;
  wire [nbit_addr*max_n_rw-1:0] relative_addr;
  wire [memory_bitsize*max_n_writes-1:0] din_value_aggregated;
  wire [memory_bitsize*PORTSIZE_S_Wdata_ram-1:0] S_Wdata_ram_int;
  wire [memory_bitsize*max_n_reads-1:0] out1_shifted;
  wire [memory_bitsize*max_n_reads-1:0] dout;
  wire [nbits_byte_offset*max_n_rw-1:0] byte_offset;
  wire [n_byte_on_databus*max_n_writes-1:0] be;
  
  reg [PORTSIZE_S_we_ram-1:0] we_ram_cs_delayed =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed_registered =0;
  reg [PORTSIZE_S_oe_ram-1:0] oe_ram_cs_delayed_registered1 =0;
  reg [max_n_reads-1:0] delayed_swapped_bit =0;
  reg [max_n_reads-1:0] delayed_swapped_bit_registered =0;
  reg [max_n_reads-1:0] delayed_swapped_bit_registered1 =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset_registered =0;
  reg [nbits_byte_offset*max_n_reads-1:0] delayed_byte_offset_registered1 =0;
  
  generate
  genvar ind2;
  for (ind2=0; ind2<PORTSIZE_in2; ind2=ind2+1)
    begin : Lind2
      assign tmp_addr[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2] = (proxy_sel_LOAD[ind2]||proxy_sel_STORE[ind2]) ? proxy_in2[(ind2+1)*BITSIZE_proxy_in2-1:ind2*BITSIZE_proxy_in2] : in2[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2];
    end
  endgenerate
  
  generate
  genvar i2;
    for (i2=0;i2<max_n_reads;i2=i2+1)
    begin : L_copy
        assign dout[(memory_bitsize/2)+memory_bitsize*i2-1:memory_bitsize*i2] = delayed_swapped_bit[i2] ? dout_a[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2] : dout_b[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2];
        assign dout[memory_bitsize*(i2+1)-1:memory_bitsize*i2+(memory_bitsize/2)] = delayed_swapped_bit[i2] ? dout_b[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2] : dout_a[(memory_bitsize/2)*(i2+1)-1:(memory_bitsize/2)*i2];
        always @(posedge clock)
        begin
          if(HIGH_LATENCY == 0)
            delayed_swapped_bit[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
          else if(HIGH_LATENCY == 1)
          begin
            delayed_swapped_bit_registered[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
            delayed_swapped_bit[i2] <= delayed_swapped_bit_registered[i2];
          end
          else
          begin
            delayed_swapped_bit_registered1[i2] <= !relative_addr[nbits_byte_offset+i2*nbit_addr-1];
            delayed_swapped_bit_registered[i2] <= delayed_swapped_bit_registered1[i2];
            delayed_swapped_bit[i2] <= delayed_swapped_bit_registered[i2];
          end
        end
    end
  endgenerate
  
  generate
  genvar i3;
    for (i3=0; i3<PORTSIZE_S_addr_ram; i3=i3+1)
    begin : L3
      if(PRIVATE_MEMORY==0 && USE_SPARSE_MEMORY==0)
        assign cs[i3] = (S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram] >= (address_space_begin)) && (S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram] < (address_space_begin+address_space_rangesize));
      else if(PRIVATE_MEMORY==0)
        assign cs[i3] = S_addr_ram[(i3+1)*BITSIZE_S_addr_ram-1:i3*BITSIZE_S_addr_ram+nbits_address_space_rangesize] == address_space_begin[nbit_addr-1:nbits_address_space_rangesize];
      else
        assign cs[i3] = 1'b0;
    end
  endgenerate
  
  generate
  genvar i4;
    for (i4=0; i4<PORTSIZE_S_oe_ram; i4=i4+1)
    begin : L4
      assign oe_ram_cs[i4] = S_oe_ram[i4] & cs[i4];
    end
  endgenerate
  
  generate
  genvar i5;
    for (i5=0; i5<PORTSIZE_S_we_ram; i5=i5+1)
    begin : L5
      assign we_ram_cs[i5] = S_we_ram[i5] & cs[i5];
    end
  endgenerate
  
  generate
  genvar i6;
    for (i6=0; i6<max_n_rw; i6=i6+1)
    begin : L6
      if(PRIVATE_MEMORY==0 && USE_SPARSE_MEMORY==0 && i6< PORTSIZE_S_addr_ram)
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = ((i6 < max_n_writes && (sel_STORE[i6]==1'b1 || proxy_sel_STORE[i6]==1'b1)) || (i6 < max_n_reads && (sel_LOAD[i6]==1'b1 || proxy_sel_LOAD[i6]==1'b1))) ? tmp_addr[(i6+1)*BITSIZE_in2-1:i6*BITSIZE_in2]-address_space_begin: S_addr_ram[(i6+1)*BITSIZE_S_addr_ram-1:i6*BITSIZE_S_addr_ram]-address_space_begin;
      else if(PRIVATE_MEMORY==0 && i6< PORTSIZE_S_addr_ram)
        assign relative_addr[(i6)*nbit_addr+nbits_address_space_rangesize-1:i6*nbit_addr] = ((i6 < max_n_writes && (sel_STORE[i6]==1'b1 || proxy_sel_STORE[i6]==1'b1)) || (i6 < max_n_reads && (sel_LOAD[i6]==1'b1 || proxy_sel_LOAD[i6]==1'b1))) ? tmp_addr[(i6)*BITSIZE_in2+nbits_address_space_rangesize-1:i6*BITSIZE_in2] : S_addr_ram[(i6)*BITSIZE_S_addr_ram+nbits_address_space_rangesize-1:i6*BITSIZE_S_addr_ram];
      else if(USE_SPARSE_MEMORY==1)
        assign relative_addr[(i6)*nbit_addr+nbits_address_space_rangesize-1:i6*nbit_addr] = tmp_addr[(i6)*BITSIZE_in2+nbits_address_space_rangesize-1:i6*BITSIZE_in2];
      else
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*BITSIZE_in2-1:i6*BITSIZE_in2]-address_space_begin;
    end
  endgenerate
  
  generate
  genvar i7;
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_A
      if (n_mem_elements==1)
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = !relative_addr[nbits_byte_offset+i7*nbit_addr-1] ? relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr] : (relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr-1]+ 1'b1) >> 1;
    end
  endgenerate
  
  generate
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_B
      if (n_mem_elements==1)
        assign memory_addr_b[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_b[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = !relative_addr[nbits_byte_offset+i7*nbit_addr-1] ? (relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr-1] + 1'b1) >> 1 : relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr];
    end
  endgenerate
  
  generate
  genvar i8;
    for (i8=0; i8<max_n_rw; i8=i8+1)
    begin : L8
      if (n_byte_on_databus==2)
        assign byte_offset[(i8+1)*nbits_byte_offset-1:i8*nbits_byte_offset] = {nbits_byte_offset{1'b0}};
      else
        assign byte_offset[(i8+1)*nbits_byte_offset-1:i8*nbits_byte_offset] = {1'b0, relative_addr[nbits_byte_offset+i8*nbit_addr-2:i8*nbit_addr]};
    end
  endgenerate
  
  generate
  genvar i9, i10;
    for (i9=0; i9<max_n_writes; i9=i9+1)
    begin : byte_enable
      if(PRIVATE_MEMORY==0 && i9 < PORTSIZE_S_data_ram_size)
      begin
        assign conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = proxy_sel_STORE[i9] ? proxy_in3[BITSIZE_proxy_in3+BITSIZE_proxy_in3*i9-1:3+BITSIZE_proxy_in3*i9] : (sel_STORE[i9] ? in3[BITSIZE_in3+BITSIZE_in3*i9-1:3+BITSIZE_in3*i9] : S_data_ram_size[BITSIZE_S_data_ram_size+BITSIZE_S_data_ram_size*i9-1:3+BITSIZE_S_data_ram_size*i9]);
        assign conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = CONV(conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus]);
        assign be[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] << byte_offset[(i9+1)*nbits_byte_offset-1:i9*nbits_byte_offset];
      end
      else
      begin
        assign conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = proxy_sel_STORE[i9] ? proxy_in3[BITSIZE_proxy_in3+BITSIZE_proxy_in3*i9-1:3+BITSIZE_proxy_in3*i9] : in3[BITSIZE_in3+BITSIZE_in3*i9-1:3+BITSIZE_in3*i9];
        assign conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = CONV(conv_in[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus]);
        assign be[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] = conv_out[(i9+1)*n_byte_on_databus-1:i9*n_byte_on_databus] << byte_offset[(i9+1)*nbits_byte_offset-1:i9*nbits_byte_offset];
      end
    end
  endgenerate
  
  generate
    for (i9=0; i9<max_n_writes; i9=i9+1)
    begin : L9_swapped
      for (i10=0; i10<n_byte_on_databus/2; i10=i10+1)
      begin  : byte_enable_swapped
        assign be_swapped[i10+i9*n_byte_on_databus] = !relative_addr[nbits_byte_offset+i9*nbit_addr-1] ? be[i10+i9*n_byte_on_databus] : be[i10+i9*n_byte_on_databus+n_byte_on_databus/2];
        assign be_swapped[i10+i9*n_byte_on_databus+n_byte_on_databus/2] =  !relative_addr[nbits_byte_offset+i9*nbit_addr-1] ? be[i10+i9*n_byte_on_databus+n_byte_on_databus/2] : be[i10+i9*n_byte_on_databus];
      end
    end
  endgenerate
    
  generate
  genvar i13;
    for (i13=0; i13<PORTSIZE_S_Wdata_ram; i13=i13+1)
    begin : L13
      if (BITSIZE_S_Wdata_ram < memory_bitsize)
        assign S_Wdata_ram_int[memory_bitsize*(i13+1)-1:memory_bitsize*i13] = {{memory_bitsize-BITSIZE_S_Wdata_ram{1'b0}}, S_Wdata_ram[(i13+1)*BITSIZE_S_Wdata_ram-1:BITSIZE_S_Wdata_ram*i13]};
      else
        assign S_Wdata_ram_int[memory_bitsize*(i13+1)-1:memory_bitsize*i13] = S_Wdata_ram[memory_bitsize+BITSIZE_S_Wdata_ram*i13-1:BITSIZE_S_Wdata_ram*i13];
    end
  endgenerate
  
  generate
  genvar i14;
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14
      if(PRIVATE_MEMORY==0 && i14 < PORTSIZE_S_Wdata_ram)
        assign din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : (sel_STORE[i14] ? in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : S_Wdata_ram_int[memory_bitsize*(i14+1)-1:memory_bitsize*i14] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8);
      else
        assign din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8 : in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1] << byte_offset[(i14+1)*nbits_byte_offset-1:i14*nbits_byte_offset]*8;
    end
  endgenerate
  
  generate
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14_swapped
      assign din_value_aggregated_swapped[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize] = !relative_addr[nbits_byte_offset+i14*nbit_addr-1] ? din_value_aggregated[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize] : din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2];
      assign din_value_aggregated_swapped[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2] = !relative_addr[nbits_byte_offset+i14*nbit_addr-1] ?  din_value_aggregated[(i14+1)*memory_bitsize-1:i14*memory_bitsize+memory_bitsize/2] : din_value_aggregated[(i14)*memory_bitsize+memory_bitsize/2-1:i14*memory_bitsize];
    end
  endgenerate
  
  generate
  genvar i15;
    for (i15=0; i15<max_n_reads; i15=i15+1)
    begin : L15
      assign out1_shifted[(i15+1)*memory_bitsize-1:i15*memory_bitsize] = dout[(i15+1)*memory_bitsize-1:i15*memory_bitsize] >> delayed_byte_offset[(i15+1)*nbits_byte_offset-1:i15*nbits_byte_offset]*8;
    end
  endgenerate
  
  generate
  genvar i20;
    for (i20=0; i20<max_n_reads; i20=i20+1)
    begin : L20
      assign out1[(i20+1)*BITSIZE_out1-1:i20*BITSIZE_out1] = out1_shifted[i20*memory_bitsize+BITSIZE_out1-1:i20*memory_bitsize];
      assign proxy_out1[(i20+1)*BITSIZE_proxy_out1-1:i20*BITSIZE_proxy_out1] = out1_shifted[i20*memory_bitsize+BITSIZE_proxy_out1-1:i20*memory_bitsize];
    end
  endgenerate
  
  generate
  genvar i16;
    for (i16=0; i16<PORTSIZE_S_oe_ram; i16=i16+1)
    begin : L16
      always @(posedge clock )
      begin
        if(reset == 1'b0)
          begin
            oe_ram_cs_delayed[i16] <= 1'b0;
            if(HIGH_LATENCY != 0) oe_ram_cs_delayed_registered[i16] <= 1'b0;
            if(HIGH_LATENCY == 2) oe_ram_cs_delayed_registered1[i16] <= 1'b0;
          end
        else
          if(HIGH_LATENCY == 0)
          begin
            oe_ram_cs_delayed[i16] <= oe_ram_cs[i16] & (!oe_ram_cs_delayed[i16] | BUS_PIPELINED);
          end
          else if(HIGH_LATENCY == 1)
          begin
            oe_ram_cs_delayed_registered[i16] <= oe_ram_cs[i16] & ((!oe_ram_cs_delayed_registered[i16] & !oe_ram_cs_delayed[i16]) | BUS_PIPELINED);
            oe_ram_cs_delayed[i16] <= oe_ram_cs_delayed_registered[i16];
          end
          else
          begin
            oe_ram_cs_delayed_registered1[i16] <= oe_ram_cs[i16] & ((!oe_ram_cs_delayed_registered1[i16] & !oe_ram_cs_delayed_registered[i16] & !oe_ram_cs_delayed[i16]) | BUS_PIPELINED);
            oe_ram_cs_delayed_registered[i16] <= oe_ram_cs_delayed_registered1[i16];
            oe_ram_cs_delayed[i16] <= oe_ram_cs_delayed_registered[i16];
          end
        end
      end
  endgenerate
  
  always @(posedge clock)
  begin
    if(HIGH_LATENCY == 0)
      delayed_byte_offset <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
    else if(HIGH_LATENCY == 1)
    begin
      delayed_byte_offset_registered <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
      delayed_byte_offset <= delayed_byte_offset_registered;
    end
    else
    begin
      delayed_byte_offset_registered1 <= byte_offset[nbits_byte_offset*max_n_reads-1:0];
      delayed_byte_offset_registered <= delayed_byte_offset_registered1;
      delayed_byte_offset <= delayed_byte_offset_registered;
    end
  end
  
  
  generate
  genvar i17;
    for (i17=0; i17<PORTSIZE_S_we_ram; i17=i17+1)
    begin : L17
      always @(posedge clock )
      begin
        if(reset == 1'b0)
          we_ram_cs_delayed[i17] <= 1'b0;
        else
          we_ram_cs_delayed[i17] <= we_ram_cs[i17] & !we_ram_cs_delayed[i17];
      end
    end
  endgenerate
  
  generate
  genvar i18;
    for (i18=0; i18<PORTSIZE_Sout_Rdata_ram; i18=i18+1)
    begin : L18
      if(PRIVATE_MEMORY==1)
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
      else if (BITSIZE_Sout_Rdata_ram <= memory_bitsize)
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = oe_ram_cs_delayed[i18] ? out1_shifted[BITSIZE_Sout_Rdata_ram+i18*memory_bitsize-1:i18*memory_bitsize] : Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
      else
        assign Sout_Rdata_ram[(i18+1)*BITSIZE_Sout_Rdata_ram-1:i18*BITSIZE_Sout_Rdata_ram] = oe_ram_cs_delayed[i18] ? {{BITSIZE_S_Wdata_ram-memory_bitsize{1'b0}}, out1_shifted[(i18+1)*memory_bitsize-1:i18*memory_bitsize]} : Sin_Rdata_ram[(i18+1)*BITSIZE_Sin_Rdata_ram-1:i18*BITSIZE_Sin_Rdata_ram];
    end
  endgenerate
  
  generate
  genvar i19;
    for (i19=0; i19<PORTSIZE_Sout_DataRdy; i19=i19+1)
    begin : L19
      if(PRIVATE_MEMORY==0)
        assign Sout_DataRdy[i19] = (i19 < PORTSIZE_S_oe_ram && oe_ram_cs_delayed[i19]) | Sin_DataRdy[i19] | (i19 < PORTSIZE_S_we_ram && we_ram_cs_delayed[i19]);
      else
        assign Sout_DataRdy[i19] = Sin_DataRdy[i19];
    end
  endgenerate
  
  generate
  genvar i21;
    for (i21=0; i21<PORTSIZE_bram_write; i21=i21+1)
    begin : L21
      if(i21 < PORTSIZE_S_we_ram)
        assign bram_write[i21] = (sel_STORE[i21] || proxy_sel_STORE[i21] || we_ram_cs[i21]);
      else
        assign bram_write[i21] = (sel_STORE[i21] || proxy_sel_STORE[i21]);
    end
    endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module BRAM_MEMORY_NN_SV(clock, bram_write, memory_addr_a, din_value_aggregated_swapped, be_swapped, dout_a);
  parameter BITSIZE_bram_write=1, PORTSIZE_bram_write=2, BITSIZE_dout_a=1, PORTSIZE_dout_a=2, BITSIZE_memory_addr_a=1, PORTSIZE_memory_addr_a=2, BITSIZE_din_value_aggregated_swapped=1, PORTSIZE_din_value_aggregated_swapped=2, BITSIZE_be_swapped=1, PORTSIZE_be_swapped=2, MEMORY_INIT_file="array.mem", BRAM_BITSIZE=32, nbit_read_addr=32, n_byte_on_databus=4, n_mem_elements=4, max_n_reads=2, max_n_writes=2, memory_offset=16, n_byte_on_databus_offset=2, HIGH_LATENCY=0;
  // IN
  input clock;
  input [PORTSIZE_bram_write-1:0] bram_write;
  input [(PORTSIZE_memory_addr_a*BITSIZE_memory_addr_a)+(-1):0] memory_addr_a;
  input [(PORTSIZE_din_value_aggregated_swapped*BITSIZE_din_value_aggregated_swapped)+(-1):0] din_value_aggregated_swapped;
  input [(PORTSIZE_be_swapped*BITSIZE_be_swapped)+(-1):0] be_swapped;
  // OUT
  output [(PORTSIZE_dout_a*BITSIZE_dout_a)+(-1):0] dout_a;
  
  reg [PORTSIZE_bram_write-1:0] bram_write1 =0;
  reg [(PORTSIZE_memory_addr_a*BITSIZE_memory_addr_a)+(-1):0] memory_addr_a1 =0;
  reg [(PORTSIZE_din_value_aggregated_swapped*BITSIZE_din_value_aggregated_swapped)+(-1):0] din_value_aggregated_swapped1 =0;
  reg [(PORTSIZE_be_swapped*BITSIZE_be_swapped)+(-1):0] be_swapped1 =0;
  
  generate
    if(HIGH_LATENCY==2)
    begin
      always @ (posedge clock)
      begin
         memory_addr_a1 <= memory_addr_a;
         bram_write1 <= bram_write;
         be_swapped1 <= be_swapped;
         din_value_aggregated_swapped1 <= din_value_aggregated_swapped;
      end
    end
  endgenerate
  
  
  generate
  if(BRAM_BITSIZE == 8)
  begin
    reg [(n_byte_on_databus/2)*8-1:0] dout_a_tmp =0;
    reg [(n_byte_on_databus/2)*8-1:0] dout_b_tmp =0;
    reg [(n_byte_on_databus/2)*8-1:0] dout_a_registered =0;
    reg [(n_byte_on_databus/2)*8-1:0] dout_b_registered =0;
    reg [(n_byte_on_databus/2)*8-1:0] memory [n_mem_elements-1:0]/* synthesis syn_ramstyle = "no_rw_check" */ ;
    assign dout_a[BRAM_BITSIZE-1:0] = dout_a_tmp;
    assign dout_a[2*BRAM_BITSIZE-1:BRAM_BITSIZE] = dout_b_tmp;
    initial
    begin
      $readmemb(MEMORY_INIT_file, memory, 0, n_mem_elements-1);
    end
    if(n_mem_elements == 1)
    begin
      always @(posedge clock)
      begin
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[0] && be_swapped[n_byte_on_databus_offset])
            memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]] <= din_value_aggregated_swapped[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset];
        end
        else
        begin
          if (bram_write1[0] && be_swapped1[n_byte_on_databus_offset])
            memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]] <= din_value_aggregated_swapped1[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset];
        end
        if(HIGH_LATENCY == 0)
          dout_a_tmp <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_a_registered <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
        else
        begin
          dout_a_registered <= memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[1] && be_swapped[n_byte_on_databus+n_byte_on_databus_offset])
            memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]] <= din_value_aggregated_swapped[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset];
        end
        else
        begin
          if (bram_write1[1] && be_swapped1[n_byte_on_databus+n_byte_on_databus_offset])
            memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]] <= din_value_aggregated_swapped1[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset];
        end
        if(HIGH_LATENCY == 0)
          dout_b_tmp <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_b_registered <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
        else
        begin
          dout_b_registered <= memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
      end
    end
    else
    begin
      always @(posedge clock)
      begin
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[0] && be_swapped[n_byte_on_databus_offset])
            memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]] <= din_value_aggregated_swapped[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset];
        end
        else
        begin
          if (bram_write1[0] && be_swapped1[n_byte_on_databus_offset])
            memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]] <= din_value_aggregated_swapped1[(n_byte_on_databus/2)*8+memory_offset-1:memory_offset];
        end
        if(HIGH_LATENCY == 0)
          dout_a_tmp <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_a_registered <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
        else
        begin
          dout_a_registered <= memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
      end
      always @(posedge clock)
      begin
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[1] && be_swapped[n_byte_on_databus+n_byte_on_databus_offset])
            memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]] <= din_value_aggregated_swapped[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset];
        end
        else
        begin
          if (bram_write1[1] && be_swapped1[n_byte_on_databus+n_byte_on_databus_offset])
            memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]] <= din_value_aggregated_swapped1[2*BRAM_BITSIZE+(n_byte_on_databus/2)*8+memory_offset-1:2*BRAM_BITSIZE+memory_offset];
        end
        if(HIGH_LATENCY == 0)
          dout_b_tmp <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_b_registered <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
        else
        begin
          dout_b_registered <= memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
      end
    end
  end
  else
  begin
    if(n_mem_elements == 1)
    begin
      reg [(n_byte_on_databus/2)*8-1:0] dout_a_tmp =0;
      reg [(n_byte_on_databus/2)*8-1:0] dout_b_tmp =0;
      reg [(n_byte_on_databus/2)*8-1:0] dout_a_registered =0;
      reg [(n_byte_on_databus/2)*8-1:0] dout_b_registered =0;
      reg [(n_byte_on_databus/2)*8-1:0] memory [n_mem_elements-1:0]/* synthesis syn_ramstyle = "no_rw_check" */ ;
      assign dout_a[BRAM_BITSIZE-1:0] = dout_a_tmp;
      assign dout_a[2*BRAM_BITSIZE-1:BRAM_BITSIZE] = dout_b_tmp;
      initial
      begin
        $readmemb(MEMORY_INIT_file, memory, 0, n_mem_elements-1);
      end
      always @(posedge clock)
      begin
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[0])
          begin : L11_write
            integer i11;
            for (i11=0; i11<n_byte_on_databus/2; i11=i11+1)
            begin
              if(be_swapped[i11+n_byte_on_databus_offset])
                memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]][i11*8+:8] <= din_value_aggregated_swapped[i11*8+memory_offset+:8];
            end
          end
        end
        else
        begin
          if (bram_write1[0])
          begin : L11_write1
            integer i11;
            for (i11=0; i11<n_byte_on_databus/2; i11=i11+1)
            begin
              if(be_swapped1[i11+n_byte_on_databus_offset])
                memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]][i11*8+:8] <= din_value_aggregated_swapped1[i11*8+memory_offset+:8];
            end
          end
        end
        if(HIGH_LATENCY == 0)
          dout_a_tmp <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_a_registered <= memory[memory_addr_a[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
        else
        begin
          dout_a_registered <= memory[memory_addr_a1[BITSIZE_memory_addr_a-1:0]];
          dout_a_tmp <= dout_a_registered;
        end
        if(HIGH_LATENCY == 0||HIGH_LATENCY == 1)
        begin
          if (bram_write[1])
          begin : L22_write
            integer i22;
            for (i22=0; i22<n_byte_on_databus/2; i22=i22+1)
            begin
              if(be_swapped[i22+n_byte_on_databus+n_byte_on_databus_offset])
                memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]][i22*8+:8] <= din_value_aggregated_swapped[2*BRAM_BITSIZE+memory_offset+i22*8+:8];
            end
          end
        end
        else
        begin
          if (bram_write1[1])
          begin : L22_write1
            integer i22;
            for (i22=0; i22<n_byte_on_databus/2; i22=i22+1)
            begin
              if(be_swapped1[i22+n_byte_on_databus+n_byte_on_databus_offset])
                memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]][i22*8+:8] <= din_value_aggregated_swapped1[2*BRAM_BITSIZE+memory_offset+i22*8+:8];
            end
          end
        end
        if(HIGH_LATENCY == 0)
          dout_b_tmp <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
        else if(HIGH_LATENCY == 1)
        begin
          dout_b_registered <= memory[memory_addr_a[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
        else
        begin
          dout_b_registered <= memory[memory_addr_a1[2*BITSIZE_memory_addr_a-1:BITSIZE_memory_addr_a]];
          dout_b_tmp <= dout_b_registered;
        end
      end
    end
    else
      BRAM_MEMORY_NN_SV_CORE #(.PORTSIZE_bram_write(PORTSIZE_bram_write), .BITSIZE_bram_write(BITSIZE_bram_write), .BITSIZE_dout_a(BITSIZE_dout_a), .PORTSIZE_dout_a(PORTSIZE_dout_a), .BITSIZE_memory_addr_a(BITSIZE_memory_addr_a), .PORTSIZE_memory_addr_a(PORTSIZE_memory_addr_a), .BITSIZE_din_value_aggregated_swapped(BITSIZE_din_value_aggregated_swapped), .PORTSIZE_din_value_aggregated_swapped(PORTSIZE_din_value_aggregated_swapped), .BITSIZE_be_swapped(BITSIZE_be_swapped), .PORTSIZE_be_swapped(PORTSIZE_be_swapped), .MEMORY_INIT_file(MEMORY_INIT_file), .BRAM_BITSIZE(BRAM_BITSIZE), .nbit_read_addr(nbit_read_addr), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .memory_offset(memory_offset), .n_byte_on_databus_offset(n_byte_on_databus_offset), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_NN_CORE_instance_a(.clock(clock), .bram_write(bram_write), .memory_addr_a(memory_addr_a), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .dout_a(dout_a));
  
  end
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN_SV(clock, reset, in1, in2, in3, out1, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, Sout_Rdata_ram, S_data_ram_size, Sin_DataRdy, Sout_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, MEMORY_INIT_file_a="array_a.mem", MEMORY_INIT_file_b="array_b.mem", n_elements=1, data_size=32, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, USE_SPARSE_MEMORY=1, HIGH_LATENCY=0, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  parameter n_byte_data = data_size/8;
  parameter n_bytes = n_elements*n_byte_data;
  parameter memory_bitsize = 2*BRAM_BITSIZE;
  parameter n_byte_on_databus = memory_bitsize/8;
  parameter n_mem_elements = n_bytes/(n_byte_on_databus) + (n_bytes%(n_byte_on_databus) == 0 ? 0 : 1);
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : $clog2(n_mem_elements);
  `else
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : log2(n_mem_elements);
  `endif
  parameter max_n_writes = PORTSIZE_sel_STORE > PORTSIZE_S_we_ram ? PORTSIZE_sel_STORE : PORTSIZE_S_we_ram;
  parameter max_n_reads = PORTSIZE_sel_LOAD > PORTSIZE_S_oe_ram ? PORTSIZE_sel_LOAD : PORTSIZE_S_oe_ram;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  
  wire [max_n_writes-1:0] bram_write;
  
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a;
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_b;
  wire [n_byte_on_databus*max_n_writes-1:0] be_swapped;
  
  wire [memory_bitsize*max_n_writes-1:0] din_value_aggregated_swapped;
  wire [(memory_bitsize/2)*max_n_reads-1:0] dout_a;
  wire [(memory_bitsize/2)*max_n_reads-1:0] dout_b;
  
  
  BRAM_MEMORY_NN_SV #(.PORTSIZE_bram_write(max_n_writes), .BITSIZE_bram_write(1), .BITSIZE_dout_a(memory_bitsize/2), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .MEMORY_INIT_file(MEMORY_INIT_file_a), .BRAM_BITSIZE(BRAM_BITSIZE), .nbit_read_addr(nbit_read_addr), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .memory_offset(0), .n_byte_on_databus_offset(0), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_NN_instance_a(.clock(clock), .bram_write(bram_write), .memory_addr_a(memory_addr_a), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .dout_a(dout_a));
  
  generate
    if (n_bytes > BRAM_BITSIZE/8)
    begin : SECOND_MEMORY
      BRAM_MEMORY_NN_SV #(.PORTSIZE_bram_write(max_n_writes), .BITSIZE_bram_write(1), .BITSIZE_dout_a((memory_bitsize/2)), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .MEMORY_INIT_file(MEMORY_INIT_file_b), .BRAM_BITSIZE(BRAM_BITSIZE), .nbit_read_addr(nbit_read_addr), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .memory_offset(memory_bitsize/2), .n_byte_on_databus_offset(n_byte_on_databus/2), .HIGH_LATENCY(HIGH_LATENCY)) BRAM_MEMORY_NN_instance_b(.clock(clock), .bram_write(bram_write), .memory_addr_a(memory_addr_b), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .dout_a(dout_b));
    end
  else
    assign dout_b = {(memory_bitsize/2)*max_n_reads{1'b0}};
  endgenerate
  
  ADDRESS_DECODING_LOGIC_NN #(.BITSIZE_in1(BITSIZE_in1), .PORTSIZE_in1(PORTSIZE_in1), .BITSIZE_in2(BITSIZE_in2), .PORTSIZE_in2(PORTSIZE_in2), .BITSIZE_in3(BITSIZE_in3), .PORTSIZE_in3(PORTSIZE_in3), .BITSIZE_sel_LOAD(BITSIZE_sel_LOAD), .PORTSIZE_sel_LOAD(PORTSIZE_sel_LOAD), .BITSIZE_sel_STORE(BITSIZE_sel_STORE), .PORTSIZE_sel_STORE(PORTSIZE_sel_STORE), .BITSIZE_out1(BITSIZE_out1), .PORTSIZE_out1(PORTSIZE_out1), .BITSIZE_S_oe_ram(BITSIZE_S_oe_ram), .PORTSIZE_S_oe_ram(PORTSIZE_S_oe_ram), .BITSIZE_S_we_ram(BITSIZE_S_we_ram), .PORTSIZE_S_we_ram(PORTSIZE_S_we_ram), .BITSIZE_Sin_DataRdy(BITSIZE_Sin_DataRdy), .PORTSIZE_Sin_DataRdy(PORTSIZE_Sin_DataRdy), .BITSIZE_Sout_DataRdy(BITSIZE_Sout_DataRdy), .PORTSIZE_Sout_DataRdy(PORTSIZE_Sout_DataRdy), .BITSIZE_S_addr_ram(BITSIZE_S_addr_ram), .PORTSIZE_S_addr_ram(PORTSIZE_S_addr_ram), .BITSIZE_S_Wdata_ram(BITSIZE_S_Wdata_ram), .PORTSIZE_S_Wdata_ram(PORTSIZE_S_Wdata_ram), .BITSIZE_Sin_Rdata_ram(BITSIZE_Sin_Rdata_ram), .PORTSIZE_Sin_Rdata_ram(PORTSIZE_Sin_Rdata_ram), .BITSIZE_Sout_Rdata_ram(BITSIZE_Sout_Rdata_ram), .PORTSIZE_Sout_Rdata_ram(PORTSIZE_Sout_Rdata_ram), .BITSIZE_S_data_ram_size(BITSIZE_S_data_ram_size), .PORTSIZE_S_data_ram_size(PORTSIZE_S_data_ram_size), .address_space_begin(address_space_begin), .address_space_rangesize(address_space_rangesize), .BUS_PIPELINED(BUS_PIPELINED), .BRAM_BITSIZE(BRAM_BITSIZE), .PRIVATE_MEMORY(PRIVATE_MEMORY), .USE_SPARSE_MEMORY(USE_SPARSE_MEMORY), .HIGH_LATENCY(HIGH_LATENCY), .BITSIZE_proxy_in1(BITSIZE_proxy_in1), .PORTSIZE_proxy_in1(PORTSIZE_proxy_in1), .BITSIZE_proxy_in2(BITSIZE_proxy_in2), .PORTSIZE_proxy_in2(PORTSIZE_proxy_in2), .BITSIZE_proxy_in3(BITSIZE_proxy_in3), .PORTSIZE_proxy_in3(PORTSIZE_proxy_in3), .BITSIZE_proxy_sel_LOAD(BITSIZE_proxy_sel_LOAD), .PORTSIZE_proxy_sel_LOAD(PORTSIZE_proxy_sel_LOAD), .BITSIZE_proxy_sel_STORE(BITSIZE_proxy_sel_STORE), .PORTSIZE_proxy_sel_STORE(PORTSIZE_proxy_sel_STORE), .BITSIZE_proxy_out1(BITSIZE_proxy_out1), .PORTSIZE_proxy_out1(PORTSIZE_proxy_out1), .BITSIZE_dout_a(memory_bitsize/2), .PORTSIZE_dout_a(max_n_reads), .BITSIZE_dout_b(memory_bitsize/2), .PORTSIZE_dout_b(max_n_reads), .BITSIZE_memory_addr_a(nbit_read_addr), .PORTSIZE_memory_addr_a(max_n_rw), .BITSIZE_memory_addr_b(nbit_read_addr), .PORTSIZE_memory_addr_b(max_n_rw), .BITSIZE_din_value_aggregated_swapped(memory_bitsize), .PORTSIZE_din_value_aggregated_swapped(max_n_writes), .BITSIZE_be_swapped(n_byte_on_databus), .PORTSIZE_be_swapped(max_n_writes), .BITSIZE_bram_write(1), .PORTSIZE_bram_write(max_n_writes), .nbit_read_addr(nbit_read_addr), .n_byte_on_databus(n_byte_on_databus), .n_mem_elements(n_mem_elements), .max_n_reads(max_n_reads), .max_n_writes(max_n_writes), .max_n_rw(max_n_rw)) ADDRESS_DECODING_LOGIC_NN_instance (.clock(clock), .reset(reset), .in1(in1), .in2(in2), .in3(in3), .out1(out1), .sel_LOAD(sel_LOAD), .sel_STORE(sel_STORE), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .Sout_Rdata_ram(Sout_Rdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_DataRdy(Sin_DataRdy), .Sout_DataRdy(Sout_DataRdy), .proxy_in1(proxy_in1), .proxy_in2(proxy_in2), .proxy_in3(proxy_in3), .proxy_sel_LOAD(proxy_sel_LOAD), .proxy_sel_STORE(proxy_sel_STORE), .proxy_out1(proxy_out1), .dout_a(dout_a), .dout_b(dout_b), .memory_addr_a(memory_addr_a), .memory_addr_b(memory_addr_b), .din_value_aggregated_swapped(din_value_aggregated_swapped), .be_swapped(be_swapped), .bram_write(bram_write));
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN(clock, reset, in1, in2, in3, in4, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, S_data_ram_size, Sin_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, out1, Sout_Rdata_ram, Sout_DataRdy, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_in4=1, PORTSIZE_in4=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, MEMORY_INIT_file_a="array_b.data", MEMORY_INIT_file_b="array_b.data", n_elements=1, data_size=32, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, USE_SPARSE_MEMORY=1, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  ARRAY_1D_STD_BRAM_NN_SV #(.BITSIZE_in1(BITSIZE_in1), .PORTSIZE_in1(PORTSIZE_in1), .BITSIZE_in2(BITSIZE_in2), .PORTSIZE_in2(PORTSIZE_in2), .BITSIZE_in3(BITSIZE_in3), .PORTSIZE_in3(PORTSIZE_in3), .BITSIZE_sel_LOAD(BITSIZE_sel_LOAD), .PORTSIZE_sel_LOAD(PORTSIZE_sel_LOAD), .BITSIZE_sel_STORE(BITSIZE_sel_STORE), .PORTSIZE_sel_STORE(PORTSIZE_sel_STORE), .BITSIZE_S_oe_ram(BITSIZE_S_oe_ram), .PORTSIZE_S_oe_ram(PORTSIZE_S_oe_ram), .BITSIZE_S_we_ram(BITSIZE_S_we_ram), .PORTSIZE_S_we_ram(PORTSIZE_S_we_ram), .BITSIZE_out1(BITSIZE_out1), .PORTSIZE_out1(PORTSIZE_out1), .BITSIZE_S_addr_ram(BITSIZE_S_addr_ram), .PORTSIZE_S_addr_ram(PORTSIZE_S_addr_ram), .BITSIZE_S_Wdata_ram(BITSIZE_S_Wdata_ram), .PORTSIZE_S_Wdata_ram(PORTSIZE_S_Wdata_ram), .BITSIZE_Sin_Rdata_ram(BITSIZE_Sin_Rdata_ram), .PORTSIZE_Sin_Rdata_ram(PORTSIZE_Sin_Rdata_ram), .BITSIZE_Sout_Rdata_ram(BITSIZE_Sout_Rdata_ram), .PORTSIZE_Sout_Rdata_ram(PORTSIZE_Sout_Rdata_ram), .BITSIZE_S_data_ram_size(BITSIZE_S_data_ram_size), .PORTSIZE_S_data_ram_size(PORTSIZE_S_data_ram_size), .BITSIZE_Sin_DataRdy(BITSIZE_Sin_DataRdy), .PORTSIZE_Sin_DataRdy(PORTSIZE_Sin_DataRdy), .BITSIZE_Sout_DataRdy(BITSIZE_Sout_DataRdy), .PORTSIZE_Sout_DataRdy(PORTSIZE_Sout_DataRdy), .MEMORY_INIT_file_a(MEMORY_INIT_file_a), .MEMORY_INIT_file_b(MEMORY_INIT_file_b), .n_elements(n_elements), .data_size(data_size), .address_space_begin(address_space_begin), .address_space_rangesize(address_space_rangesize), .BUS_PIPELINED(BUS_PIPELINED), .BRAM_BITSIZE(BRAM_BITSIZE), .PRIVATE_MEMORY(PRIVATE_MEMORY), .USE_SPARSE_MEMORY(USE_SPARSE_MEMORY), .BITSIZE_proxy_in1(BITSIZE_proxy_in1), .PORTSIZE_proxy_in1(PORTSIZE_proxy_in1), .BITSIZE_proxy_in2(BITSIZE_proxy_in2), .PORTSIZE_proxy_in2(PORTSIZE_proxy_in2), .BITSIZE_proxy_in3(BITSIZE_proxy_in3), .PORTSIZE_proxy_in3(PORTSIZE_proxy_in3), .BITSIZE_proxy_sel_LOAD(BITSIZE_proxy_sel_LOAD), .PORTSIZE_proxy_sel_LOAD(PORTSIZE_proxy_sel_LOAD), .BITSIZE_proxy_sel_STORE(BITSIZE_proxy_sel_STORE), .PORTSIZE_proxy_sel_STORE(PORTSIZE_proxy_sel_STORE), .BITSIZE_proxy_out1(BITSIZE_proxy_out1), .PORTSIZE_proxy_out1(PORTSIZE_proxy_out1), .HIGH_LATENCY(0)) ARRAY_1D_STD_BRAM_NN_instance (.out1(out1), .Sout_Rdata_ram(Sout_Rdata_ram), .Sout_DataRdy(Sout_DataRdy), .proxy_out1(proxy_out1), .clock(clock), .reset(reset), .in1(in1), .in2(in2), .in3(in3), .sel_LOAD(sel_LOAD & in4), .sel_STORE(sel_STORE & in4), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_DataRdy(Sin_DataRdy), .proxy_in1(proxy_in1), .proxy_in2(proxy_in2), .proxy_in3(proxy_in3), .proxy_sel_LOAD(proxy_sel_LOAD), .proxy_sel_STORE(proxy_sel_STORE));
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN_SDS_BASE(clock, reset, in1, in2, in3, out1, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, Sout_Rdata_ram, S_data_ram_size, Sin_DataRdy, Sout_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, MEMORY_INIT_file="array.mem", n_elements=1, data_size=32, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, READ_ONLY_MEMORY=0, USE_SPARSE_MEMORY=1, HIGH_LATENCY=0, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  parameter n_byte_data = BRAM_BITSIZE/8;
  parameter n_bytes = n_elements*n_byte_data;
  parameter n_byte_on_databus = BRAM_BITSIZE/8;
  parameter n_mem_elements = n_bytes/(n_byte_on_databus) + (n_bytes%(n_byte_on_databus) == 0 ? 0 : 1);
  parameter nbit_addr = BITSIZE_in2 > BITSIZE_proxy_in2 ? BITSIZE_in2 : BITSIZE_proxy_in2;
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : $clog2(n_mem_elements);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 0 : $clog2(n_byte_on_databus);
  `else
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : log2(n_mem_elements);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 0 : log2(n_byte_on_databus);
  `endif
  parameter max_n_writes = PORTSIZE_sel_STORE;
  parameter max_n_reads = PORTSIZE_sel_LOAD;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  
  wire [max_n_writes-1:0] bram_write;
  wire [max_n_writes-1:0] bram_write_temp;
  
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a;
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a_temp;
  
  wire [data_size*max_n_writes-1:0] din_value_aggregated;
  wire [data_size*max_n_writes-1:0] din_value_aggregated_temp;
  reg [data_size*max_n_reads-1:0] dout_a =0;
  reg [data_size*max_n_reads-1:0] dout_a_registered =0;
  reg [data_size-1:0] dout_a_registered_0 =0;
  reg [data_size-1:0] dout_a_registered_1 =0;
  wire [nbit_addr*max_n_rw-1:0] tmp_addr;
  wire [nbit_addr*max_n_rw-1:0] relative_addr;
  integer index2;
  
  reg [data_size-1:0] memory [0:n_mem_elements-1] /* synthesis syn_ramstyle = "no_rw_check" */;
  
  initial
  begin
    $readmemb(MEMORY_INIT_file, memory, 0, n_mem_elements-1);
  end
  
  generate
    if(HIGH_LATENCY==2)
    begin
      (* syn_keep=1 *)reg [nbit_read_addr*max_n_rw-1:0] memory_addr_a_reg =0;
      (* syn_keep=1 *)reg [data_size*max_n_writes-1:0] din_value_aggregated_reg =0;
      (* syn_keep=1 *)reg [max_n_writes-1:0] bram_write_reg =0;
      always @ (posedge clock)
      begin
         memory_addr_a_reg <= memory_addr_a;
         bram_write_reg <= bram_write;
         din_value_aggregated_reg <= din_value_aggregated;
      end
      assign memory_addr_a_temp = memory_addr_a_reg;
      assign bram_write_temp = bram_write_reg;
      assign din_value_aggregated_temp = din_value_aggregated_reg;
    end
    else
    begin
      assign memory_addr_a_temp = memory_addr_a;
      assign bram_write_temp = bram_write;
      assign din_value_aggregated_temp = din_value_aggregated;
    end
  endgenerate
  
  generate
  genvar ind2;
  for (ind2=0; ind2<max_n_rw; ind2=ind2+1)
    begin : Lind2
      assign tmp_addr[(ind2+1)*nbit_addr-1:ind2*nbit_addr] = (proxy_sel_LOAD[ind2]||proxy_sel_STORE[ind2]) ? proxy_in2[(ind2+1)*BITSIZE_proxy_in2-1:ind2*BITSIZE_proxy_in2] : in2[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2];
    end
  endgenerate
  
  generate
  genvar i6;
    for (i6=0; i6<max_n_rw; i6=i6+1)
    begin : L6
      if(USE_SPARSE_MEMORY==1)
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*nbit_addr-1:i6*nbit_addr];
      else
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*nbit_addr-1:i6*nbit_addr]-address_space_begin;
    end
  endgenerate
  
  generate
  genvar i7;
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_A
      if (n_mem_elements==1)
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr];
    end
  endgenerate
  
  generate
  genvar i14;
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14
      assign din_value_aggregated[(i14+1)*data_size-1:i14*data_size] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] : in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1];
    end
  endgenerate
  
  generate
  genvar i11, i12;
    if(n_mem_elements==1)
    begin : single_element
      always @(posedge clock)
      begin
        for (index2=0; index2<max_n_reads; index2=index2+1)
        begin : L12_single_read
          if(HIGH_LATENCY==0)
          begin
            dout_a[data_size*index2+:data_size] <= memory[memory_addr_a_temp[nbit_read_addr*index2+:nbit_read_addr]];
          end
          else
          begin
            dout_a_registered[data_size*index2+:data_size] <= memory[memory_addr_a_temp[nbit_read_addr*index2+:nbit_read_addr]];
            dout_a[data_size*index2+:data_size] <= dout_a_registered[data_size*index2+:data_size];
          end
        end
        for (index2=0; index2<max_n_writes; index2=index2+1)
        begin : L12_single_write
          if(READ_ONLY_MEMORY==0)
          begin
            if(bram_write_temp[index2])
              memory[memory_addr_a_temp[nbit_read_addr*index2+:nbit_read_addr]] <= din_value_aggregated_temp[data_size*index2+:data_size];
          end
        end
      end
    end
    else
    begin : multiple_elements
      if(max_n_rw ==2)
      begin
        always @(posedge clock)
        begin
          if(READ_ONLY_MEMORY==0)
          begin
            if(bram_write_temp[0])
              memory[memory_addr_a_temp[nbit_read_addr*(0+1)-1:nbit_read_addr*0]] <= din_value_aggregated_temp[data_size*0+:data_size];
          end
          if(HIGH_LATENCY==0)
          begin
            dout_a[data_size*(0+1)-1:data_size*0] <= memory[memory_addr_a_temp[nbit_read_addr*(0+1)-1:nbit_read_addr*0]];
          end
          else
          begin
            dout_a_registered_0 <= memory[memory_addr_a_temp[nbit_read_addr*(0+1)-1:nbit_read_addr*0]];
            dout_a[data_size*(0+1)-1:data_size*0] <= dout_a_registered_0;
          end
        end
        always @(posedge clock)
        begin
          if(READ_ONLY_MEMORY==0)
          begin
            if(bram_write_temp[1])
              memory[memory_addr_a_temp[nbit_read_addr*(1+1)-1:nbit_read_addr*1]] <= din_value_aggregated_temp[data_size*1+:data_size];
          end
          if(HIGH_LATENCY==0)
          begin
            dout_a[data_size*(1+1)-1:data_size*1] <= memory[memory_addr_a_temp[nbit_read_addr*(1+1)-1:nbit_read_addr*1]];
          end
          else
          begin
            dout_a_registered_1 <= memory[memory_addr_a_temp[nbit_read_addr*(1+1)-1:nbit_read_addr*1]];
            dout_a[data_size*(1+1)-1:data_size*1] <= dout_a_registered_1;
          end
        end
      end
      else
      begin
        //not supported
      end
    end    
  endgenerate
  
  generate
  genvar i21;
    for (i21=0; i21<max_n_writes; i21=i21+1)
    begin : L21
        assign bram_write[i21] = sel_STORE[i21] || proxy_sel_STORE[i21];
    end
  endgenerate
  
  generate
  genvar i20;
    for (i20=0; i20<max_n_reads; i20=i20+1)
    begin : L20
      assign out1[(i20+1)*BITSIZE_out1-1:i20*BITSIZE_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
      assign proxy_out1[(i20+1)*BITSIZE_proxy_out1-1:i20*BITSIZE_proxy_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
    end
  endgenerate
  assign Sout_Rdata_ram =Sin_Rdata_ram;
  assign Sout_DataRdy = Sin_DataRdy;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_BRAM_NN_SDS(clock, reset, in1, in2, in3, in4, out1, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, Sout_Rdata_ram, S_data_ram_size, Sin_DataRdy, Sout_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_in4=1, PORTSIZE_in4=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, MEMORY_INIT_file="array.mem", n_elements=1, data_size=32, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, READ_ONLY_MEMORY=0, USE_SPARSE_MEMORY=1, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  ARRAY_1D_STD_BRAM_NN_SDS_BASE #(.BITSIZE_in1(BITSIZE_in1), .PORTSIZE_in1(PORTSIZE_in1), .BITSIZE_in2(BITSIZE_in2), .PORTSIZE_in2(PORTSIZE_in2), .BITSIZE_in3(BITSIZE_in3), .PORTSIZE_in3(PORTSIZE_in3), .BITSIZE_sel_LOAD(BITSIZE_sel_LOAD), .PORTSIZE_sel_LOAD(PORTSIZE_sel_LOAD), .BITSIZE_sel_STORE(BITSIZE_sel_STORE), .PORTSIZE_sel_STORE(PORTSIZE_sel_STORE), .BITSIZE_S_oe_ram(BITSIZE_S_oe_ram), .PORTSIZE_S_oe_ram(PORTSIZE_S_oe_ram), .BITSIZE_S_we_ram(BITSIZE_S_we_ram), .PORTSIZE_S_we_ram(PORTSIZE_S_we_ram), .BITSIZE_out1(BITSIZE_out1), .PORTSIZE_out1(PORTSIZE_out1), .BITSIZE_S_addr_ram(BITSIZE_S_addr_ram), .PORTSIZE_S_addr_ram(PORTSIZE_S_addr_ram), .BITSIZE_S_Wdata_ram(BITSIZE_S_Wdata_ram), .PORTSIZE_S_Wdata_ram(PORTSIZE_S_Wdata_ram), .BITSIZE_Sin_Rdata_ram(BITSIZE_Sin_Rdata_ram), .PORTSIZE_Sin_Rdata_ram(PORTSIZE_Sin_Rdata_ram), .BITSIZE_Sout_Rdata_ram(BITSIZE_Sout_Rdata_ram), .PORTSIZE_Sout_Rdata_ram(PORTSIZE_Sout_Rdata_ram), .BITSIZE_S_data_ram_size(BITSIZE_S_data_ram_size), .PORTSIZE_S_data_ram_size(PORTSIZE_S_data_ram_size), .BITSIZE_Sin_DataRdy(BITSIZE_Sin_DataRdy), .PORTSIZE_Sin_DataRdy(PORTSIZE_Sin_DataRdy), .BITSIZE_Sout_DataRdy(BITSIZE_Sout_DataRdy), .PORTSIZE_Sout_DataRdy(PORTSIZE_Sout_DataRdy), .MEMORY_INIT_file(MEMORY_INIT_file), .n_elements(n_elements), .data_size(data_size), .address_space_begin(address_space_begin), .address_space_rangesize(address_space_rangesize), .BUS_PIPELINED(BUS_PIPELINED), .BRAM_BITSIZE(BRAM_BITSIZE), .PRIVATE_MEMORY(PRIVATE_MEMORY), .READ_ONLY_MEMORY(READ_ONLY_MEMORY), .USE_SPARSE_MEMORY(USE_SPARSE_MEMORY), .HIGH_LATENCY(0), .BITSIZE_proxy_in1(BITSIZE_proxy_in1), .PORTSIZE_proxy_in1(PORTSIZE_proxy_in1), .BITSIZE_proxy_in2(BITSIZE_proxy_in2), .PORTSIZE_proxy_in2(PORTSIZE_proxy_in2), .BITSIZE_proxy_in3(BITSIZE_proxy_in3), .PORTSIZE_proxy_in3(PORTSIZE_proxy_in3), .BITSIZE_proxy_sel_LOAD(BITSIZE_proxy_sel_LOAD), .PORTSIZE_proxy_sel_LOAD(PORTSIZE_proxy_sel_LOAD), .BITSIZE_proxy_sel_STORE(BITSIZE_proxy_sel_STORE), .PORTSIZE_proxy_sel_STORE(PORTSIZE_proxy_sel_STORE), .BITSIZE_proxy_out1(BITSIZE_proxy_out1), .PORTSIZE_proxy_out1(PORTSIZE_proxy_out1)) ARRAY_1D_STD_BRAM_NN_instance (.out1(out1), .Sout_Rdata_ram(Sout_Rdata_ram), .Sout_DataRdy(Sout_DataRdy), .proxy_out1(proxy_out1), .clock(clock), .reset(reset), .in1(in1), .in2(in2), .in3(in3), .sel_LOAD(sel_LOAD & in4), .sel_STORE(sel_STORE & in4), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .S_data_ram_size(S_data_ram_size ), .Sin_DataRdy(Sin_DataRdy), .proxy_in1(proxy_in1), .proxy_in2(proxy_in2), .proxy_in3(proxy_in3), .proxy_sel_LOAD(proxy_sel_LOAD), .proxy_sel_STORE(proxy_sel_STORE));
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ARRAY_1D_STD_DISTRAM_NN_SDS(clock, reset, in1, in2, in3, in4, out1, sel_LOAD, sel_STORE, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, Sin_Rdata_ram, Sout_Rdata_ram, S_data_ram_size, Sin_DataRdy, Sout_DataRdy, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_in4=1, PORTSIZE_in4=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_S_oe_ram=1, PORTSIZE_S_oe_ram=2, BITSIZE_S_we_ram=1, PORTSIZE_S_we_ram=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_S_addr_ram=1, PORTSIZE_S_addr_ram=2, BITSIZE_S_Wdata_ram=8, PORTSIZE_S_Wdata_ram=2, BITSIZE_Sin_Rdata_ram=8, PORTSIZE_Sin_Rdata_ram=2, BITSIZE_Sout_Rdata_ram=8, PORTSIZE_Sout_Rdata_ram=2, BITSIZE_S_data_ram_size=1, PORTSIZE_S_data_ram_size=2, BITSIZE_Sin_DataRdy=1, PORTSIZE_Sin_DataRdy=2, BITSIZE_Sout_DataRdy=1, PORTSIZE_Sout_DataRdy=2, MEMORY_INIT_file="array.mem", n_elements=1, data_size=32, address_space_begin=0, address_space_rangesize=4, BUS_PIPELINED=1, BRAM_BITSIZE=32, PRIVATE_MEMORY=0, READ_ONLY_MEMORY=0, USE_SPARSE_MEMORY=1, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input clock;
  input reset;
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [PORTSIZE_S_oe_ram-1:0] S_oe_ram;
  input [PORTSIZE_S_we_ram-1:0] S_we_ram;
  input [(PORTSIZE_S_addr_ram*BITSIZE_S_addr_ram)+(-1):0] S_addr_ram;
  input [(PORTSIZE_S_Wdata_ram*BITSIZE_S_Wdata_ram)+(-1):0] S_Wdata_ram;
  input [(PORTSIZE_Sin_Rdata_ram*BITSIZE_Sin_Rdata_ram)+(-1):0] Sin_Rdata_ram;
  input [(PORTSIZE_S_data_ram_size*BITSIZE_S_data_ram_size)+(-1):0] S_data_ram_size;
  input [PORTSIZE_Sin_DataRdy-1:0] Sin_DataRdy;
  input [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  input [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  input [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  input [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  input [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_Sout_Rdata_ram*BITSIZE_Sout_Rdata_ram)+(-1):0] Sout_Rdata_ram;
  output [PORTSIZE_Sout_DataRdy-1:0] Sout_DataRdy;
  output [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  `ifndef _SIM_HAVE_CLOG2
      function integer log2;
        input integer value;
        integer temp_value;
        begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
        end
      endfunction
  `endif
  parameter n_byte_data = BRAM_BITSIZE/8;
  parameter n_bytes = n_elements*n_byte_data;
  parameter n_byte_on_databus = BRAM_BITSIZE/8;
  parameter n_mem_elements = n_bytes/(n_byte_on_databus) + (n_bytes % (n_byte_on_databus) == 0 ? 0 : 1);
  parameter nbit_addr = BITSIZE_in2 > BITSIZE_proxy_in2 ? BITSIZE_in2 : BITSIZE_proxy_in2;
  `ifdef _SIM_HAVE_CLOG2
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : $clog2(n_mem_elements);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 0 : $clog2(n_byte_on_databus);
  `else
    parameter nbit_read_addr = n_mem_elements == 1 ? 1 : log2(n_mem_elements);
    parameter nbits_byte_offset = n_byte_on_databus==1 ? 0 : log2(n_byte_on_databus);
  `endif
  parameter max_n_writes = PORTSIZE_sel_STORE;
  parameter max_n_reads = PORTSIZE_sel_LOAD;
  parameter max_n_rw = max_n_writes > max_n_reads ? max_n_writes : max_n_reads;
  
  wire [max_n_writes-1:0] bram_write;
  
  wire [nbit_read_addr*max_n_rw-1:0] memory_addr_a;
  wire [nbit_read_addr-1:0] memory_addr_a_0;
  wire [nbit_read_addr-1:0] memory_addr_a_1;
  
  wire [data_size*max_n_writes-1:0] din_value_aggregated;
  wire [data_size*max_n_reads-1:0] dout_a;
  wire [nbit_addr*max_n_rw-1:0] tmp_addr;
  wire [nbit_addr*max_n_rw-1:0] relative_addr;
  wire [PORTSIZE_sel_LOAD-1:0] int_sel_LOAD;
  wire [PORTSIZE_sel_STORE-1:0] int_sel_STORE;
  integer index2;
  
  reg [data_size-1:0] memory [0:n_elements-1] /* synthesis syn_ramstyle = "no_rw_check" */;
  
  initial
  begin
    $readmemb(MEMORY_INIT_file, memory, 0, n_elements-1);
  end
  
  generate
  genvar ind2;
  for (ind2=0; ind2<max_n_rw; ind2=ind2+1)
    begin : Lind2
      assign tmp_addr[(ind2+1)*nbit_addr-1:ind2*nbit_addr] = (proxy_sel_LOAD[ind2]||proxy_sel_STORE[ind2]) ? proxy_in2[(ind2+1)*BITSIZE_proxy_in2-1:ind2*BITSIZE_proxy_in2] : in2[(ind2+1)*BITSIZE_in2-1:ind2*BITSIZE_in2];
    end
  endgenerate
  
  generate
  genvar i6;
    for (i6=0; i6<max_n_rw; i6=i6+1)
    begin : L6
      if(USE_SPARSE_MEMORY==1)
        assign relative_addr[(i6)*nbit_addr+nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6)*nbit_addr+nbit_addr-1:i6*nbit_addr];
      else
        assign relative_addr[(i6+1)*nbit_addr-1:i6*nbit_addr] = tmp_addr[(i6+1)*nbit_addr-1:i6*nbit_addr]-address_space_begin;
    end
  endgenerate
  
  generate
  genvar i7;
    for (i7=0; i7<max_n_rw; i7=i7+1)
    begin : L7_A
      if (n_mem_elements==1)
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = {nbit_read_addr{1'b0}};
      else
        assign memory_addr_a[(i7+1)*nbit_read_addr-1:i7*nbit_read_addr] = relative_addr[nbit_read_addr+nbits_byte_offset-1+i7*nbit_addr:nbits_byte_offset+i7*nbit_addr];
    end
  endgenerate
  
  generate
  genvar i14;
    for (i14=0; i14<max_n_writes; i14=i14+1)
    begin : L14
      assign din_value_aggregated[(i14+1)*data_size-1:i14*data_size] = proxy_sel_STORE[i14] ? proxy_in1[(i14+1)*BITSIZE_proxy_in1-1:i14*BITSIZE_proxy_in1] : in1[(i14+1)*BITSIZE_in1-1:i14*BITSIZE_in1];
    end
  endgenerate
  
  generate
  genvar i11;
    for (i11=0; i11<max_n_reads; i11=i11+1)
    begin : asynchronous_read
      assign dout_a[data_size*i11+:data_size] = memory[memory_addr_a[nbit_read_addr*i11+:nbit_read_addr]];
    end
  endgenerate
  
  assign memory_addr_a_0 = memory_addr_a[nbit_read_addr*0+:nbit_read_addr];
  assign memory_addr_a_1 = memory_addr_a[nbit_read_addr*1+:nbit_read_addr];
  
  generate if(READ_ONLY_MEMORY==0)
    always @(posedge clock)
    begin
      if(bram_write[0])
        memory[memory_addr_a_0] <= din_value_aggregated[data_size*0+:data_size];
      if(bram_write[1])
        memory[memory_addr_a_1] <= din_value_aggregated[data_size*1+:data_size];
    end
  endgenerate
  
  generate
  genvar i21;
    for (i21=0; i21<max_n_writes; i21=i21+1)
    begin : L21
        assign bram_write[i21] = int_sel_STORE[i21] || proxy_sel_STORE[i21];
    end
  endgenerate
  
  generate
  genvar i20;
    for (i20=0; i20<max_n_reads; i20=i20+1)
    begin : L20
      assign out1[(i20+1)*BITSIZE_out1-1:i20*BITSIZE_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
      assign proxy_out1[(i20+1)*BITSIZE_proxy_out1-1:i20*BITSIZE_proxy_out1] = dout_a[(i20+1)*data_size-1:i20*data_size];
    end
  endgenerate
  assign Sout_Rdata_ram =Sin_Rdata_ram;
  assign Sout_DataRdy = Sin_DataRdy;
  assign int_sel_LOAD = sel_LOAD & in4;
  assign int_sel_STORE = sel_STORE & in4;
  
  assign Sout_DataRdy = Sin_DataRdy;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module addr_expr_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_extract_bit_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output out1;
  assign out1 = (in1 >> in2)&1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module lut_expr_FU(in1, in2, in3, in4, in5, in6, in7, in8, in9, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input in2;
  input in3;
  input in4;
  input in5;
  input in6;
  input in7;
  input in8;
  input in9;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  reg[7:0] cleaned_in0;
  wire [7:0] in0;
  wire[BITSIZE_in1-1:0] shifted_s;
  
  assign in0 = {in9, in8, in7, in6, in5, in4, in3, in2};
  generate
  genvar i0;
  for (i0=0; i0<8; i0=i0+1)
  begin : L0
        always @(*)
        begin
           if (in0[i0] == 1'b1)
              cleaned_in0[i0] = 1'b1;
           else
              cleaned_in0[i0] = 1'b0;
        end
    end
  endgenerate
  assign shifted_s = in1 >> cleaned_in0;
  assign out1[0] = shifted_s[0];
  generate
   if(BITSIZE_out1 > 1)
     assign out1[BITSIZE_out1-1:1] = 0;
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module read_cond_FU(in1, out1);
  parameter BITSIZE_in1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output out1;
  assign out1 = in1 != {BITSIZE_in1{1'b0}};
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module UIdata_converter_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){1'b0}},in1};
  end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module IUdata_converter_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){in1[BITSIZE_in1-1]}},in1};
  end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ASSIGN_UNSIGNED_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module multi_read_cond_FU(in1, out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module UUdata_converter_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  generate
  if (BITSIZE_out1 <= BITSIZE_in1)
  begin
    assign out1 = in1[BITSIZE_out1-1:0];
  end
  else
  begin
    assign out1 = {{(BITSIZE_out1-BITSIZE_in1){1'b0}},in1};
  end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_view_convert_expr_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ge_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 >= in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module lt_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input signed [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 < in2;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module rshift_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1, PRECISION=1;
  // IN
  input signed [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output signed [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 >>> (in2[arg2_bitsize-1:0]);
    else
      assign out1 = in1 >>> in2;
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_bit_and_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 & in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2016-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_bit_ior_concat_expr_FU(in1, in2, in3, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_in3=1, BITSIZE_out1=1, OFFSET_PARAMETER=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  input [BITSIZE_in3-1:0] in3;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  parameter nbit_out = BITSIZE_out1 > OFFSET_PARAMETER ? BITSIZE_out1 : 1+OFFSET_PARAMETER;
  wire [nbit_out-1:0] tmp_in1;
  wire [OFFSET_PARAMETER-1:0] tmp_in2;
  generate
    if(BITSIZE_in1 >= nbit_out)
      assign tmp_in1=in1[nbit_out-1:0];
    else
      assign tmp_in1={{(nbit_out-BITSIZE_in1){1'b0}},in1};
  endgenerate
  generate
    if(BITSIZE_in2 >= OFFSET_PARAMETER)
      assign tmp_in2=in2[OFFSET_PARAMETER-1:0];
    else
      assign tmp_in2={{(OFFSET_PARAMETER-BITSIZE_in2){1'b0}},in2};
  endgenerate
  assign out1 = {tmp_in1[nbit_out-1:OFFSET_PARAMETER] , tmp_in2};
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_bit_ior_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 | in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_bit_xor_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 ^ in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_cond_expr_FU(in1, in2, in3, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_in3=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  input [BITSIZE_in3-1:0] in3;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 != 0 ? in2 : in3;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_eq_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 == in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_ge_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 >= in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_lshift_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1, PRECISION=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 << in2[arg2_bitsize-1:0];
    else
      assign out1 = in1 << in2;
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_lt_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 < in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_minus_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 - in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_mult_expr_FU(clock, in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1, PIPE_PARAMETER=0;
  // IN
  input clock;
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  generate
    if(PIPE_PARAMETER==1)
    begin
      reg signed [BITSIZE_out1-1:0] out1_reg;
      assign out1 = out1_reg;
      always @(posedge clock)
      begin
        out1_reg <= in1 * in2;
      end
    end
    else if(PIPE_PARAMETER>1)
    begin
      reg [BITSIZE_in1-1:0] in1_in;
      reg [BITSIZE_in2-1:0] in2_in;
      wire [BITSIZE_out1-1:0] mult_res;
      reg [BITSIZE_out1-1:0] mul [PIPE_PARAMETER-2:0];
      integer i;
      assign mult_res = in1_in * in2_in;
      always @(posedge clock)
      begin
        in1_in <= in1;
        in2_in <= in2;
        mul[PIPE_PARAMETER-2] <= mult_res;
        for (i=0; i<PIPE_PARAMETER-2; i=i+1)
          mul[i] <= mul[i+1];
      end
      assign out1 = mul[0];
    end
    else
    begin
      assign out1 = in1 * in2;
    end
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_ne_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 != in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_plus_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1 + in2;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_pointer_plus_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1, LSB_PARAMETER=-1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  wire [BITSIZE_out1-1:0] in1_tmp;
  wire [BITSIZE_out1-1:0] in2_tmp;
  assign in1_tmp = in1;
  assign in2_tmp = in2;generate if (BITSIZE_out1 > LSB_PARAMETER) assign out1[BITSIZE_out1-1:LSB_PARAMETER] = (in1_tmp[BITSIZE_out1-1:LSB_PARAMETER] + in2_tmp[BITSIZE_out1-1:LSB_PARAMETER]); else assign out1 = 0; endgenerate
  generate if (LSB_PARAMETER != 0 && BITSIZE_out1 > LSB_PARAMETER) assign out1[LSB_PARAMETER-1:0] = 0; endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ui_rshift_expr_FU(in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1, PRECISION=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  `ifndef _SIM_HAVE_CLOG2
    function integer log2;
       input integer value;
       integer temp_value;
      begin
        temp_value = value-1;
        for (log2=0; temp_value>0; log2=log2+1)
          temp_value = temp_value>>1;
      end
    endfunction
  `endif
  `ifdef _SIM_HAVE_CLOG2
    parameter arg2_bitsize = $clog2(PRECISION);
  `else
    parameter arg2_bitsize = log2(PRECISION);
  `endif
  generate
    if(BITSIZE_in2 > arg2_bitsize)
      assign out1 = in1 >> (in2[arg2_bitsize-1:0]);
    else
      assign out1 = in1 >> in2;
  endgenerate

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>, Christian Pilato <christian.pilato@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module MUX_GATE(sel, in1, in2, out1);
  parameter BITSIZE_in1=1, BITSIZE_in2=1, BITSIZE_out1=1;
  // IN
  input sel;
  input [BITSIZE_in1-1:0] in1;
  input [BITSIZE_in2-1:0] in2;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = sel ? in1 : in2;
endmodule

// Datapath RTL description for aluDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath_aluDecode(clock, reset, in_port_opcode, in_port_funct3, in_port_funct7, return_port, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE, selector_MUX_9_gimple_return_FU_15_i0_0_0_0, selector_MUX_9_gimple_return_FU_15_i0_0_0_1, wrenable_reg_0, wrenable_reg_1, wrenable_reg_2, OUT_MULTIIF_aluDecode_419512_422679);
  parameter MEM_var_419713_419512=32768, MEM_var_419737_419512=32768;
  // IN
  input clock;
  input reset;
  input [31:0] in_port_opcode;
  input [31:0] in_port_funct3;
  input [31:0] in_port_funct7;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE;
  input selector_MUX_9_gimple_return_FU_15_i0_0_0_0;
  input selector_MUX_9_gimple_return_FU_15_i0_0_0_1;
  input wrenable_reg_0;
  input wrenable_reg_1;
  input wrenable_reg_2;
  // OUT
  output [31:0] return_port;
  output [1:0] OUT_MULTIIF_aluDecode_419512_422679;
  // Component and signal declarations
  wire null_out_signal_array_419713_0_Sout_DataRdy_0;
  wire null_out_signal_array_419713_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_419713_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_419713_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_419713_0_out1_1;
  wire [31:0] null_out_signal_array_419713_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_419713_0_proxy_out1_1;
  wire null_out_signal_array_419737_0_Sout_DataRdy_0;
  wire null_out_signal_array_419737_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_419737_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_419737_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_419737_0_out1_1;
  wire [31:0] null_out_signal_array_419737_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_419737_0_proxy_out1_1;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_0_i0_array_419713_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_1_i0_array_419737_0;
  wire [31:0] out_MUX_9_gimple_return_FU_15_i0_0_0_0;
  wire [31:0] out_MUX_9_gimple_return_FU_15_i0_0_0_1;
  wire [16:0] out_addr_expr_FU_4_i0_fu_aluDecode_419512_422248;
  wire [16:0] out_addr_expr_FU_5_i0_fu_aluDecode_419512_422241;
  wire out_const_0;
  wire [6:0] out_const_1;
  wire [4:0] out_const_10;
  wire [5:0] out_const_11;
  wire [4:0] out_const_12;
  wire [4:0] out_const_13;
  wire [2:0] out_const_14;
  wire [3:0] out_const_15;
  wire [4:0] out_const_16;
  wire [1:0] out_const_17;
  wire [2:0] out_const_18;
  wire [3:0] out_const_19;
  wire out_const_2;
  wire [6:0] out_const_20;
  wire [5:0] out_const_21;
  wire [6:0] out_const_22;
  wire [3:0] out_const_23;
  wire [5:0] out_const_24;
  wire [6:0] out_const_25;
  wire [2:0] out_const_26;
  wire [3:0] out_const_27;
  wire [3:0] out_const_28;
  wire [4:0] out_const_29;
  wire [1:0] out_const_3;
  wire [15:0] out_const_30;
  wire [15:0] out_const_31;
  wire [2:0] out_const_4;
  wire [3:0] out_const_5;
  wire [4:0] out_const_6;
  wire [5:0] out_const_7;
  wire [6:0] out_const_8;
  wire [7:0] out_const_9;
  wire [5:0] out_conv_out_const_1_7_6;
  wire [31:0] out_conv_out_const_30_16_32;
  wire [31:0] out_conv_out_const_31_16_32;
  wire [16:0] out_conv_out_reg_0_reg_0_18_17;
  wire [16:0] out_conv_out_reg_1_reg_1_18_17;
  wire [31:0] out_conv_out_reg_2_reg_2_5_32;
  wire out_lut_expr_FU_10_i0_fu_aluDecode_419512_422663;
  wire out_lut_expr_FU_11_i0_fu_aluDecode_419512_422678;
  wire out_lut_expr_FU_12_i0_fu_aluDecode_419512_422682;
  wire out_lut_expr_FU_13_i0_fu_aluDecode_419512_422688;
  wire out_lut_expr_FU_6_i0_fu_aluDecode_419512_422643;
  wire out_lut_expr_FU_7_i0_fu_aluDecode_419512_422649;
  wire out_lut_expr_FU_8_i0_fu_aluDecode_419512_422653;
  wire out_lut_expr_FU_9_i0_fu_aluDecode_419512_422659;
  wire [1:0] out_multi_read_cond_FU_14_i0_fu_aluDecode_419512_422679;
  wire [17:0] out_reg_0_reg_0;
  wire [17:0] out_reg_1_reg_1;
  wire [4:0] out_reg_2_reg_2;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i0_fu_aluDecode_419512_419630;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i10_fu_aluDecode_419512_422616;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i11_fu_aluDecode_419512_422619;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i12_fu_aluDecode_419512_422622;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i13_fu_aluDecode_419512_422625;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i14_fu_aluDecode_419512_422628;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i15_fu_aluDecode_419512_422631;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i16_fu_aluDecode_419512_422634;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i17_fu_aluDecode_419512_422637;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i18_fu_aluDecode_419512_422689;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i19_fu_aluDecode_419512_422692;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i1_fu_aluDecode_419512_419632;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i20_fu_aluDecode_419512_422695;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i21_fu_aluDecode_419512_422698;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i22_fu_aluDecode_419512_422701;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i23_fu_aluDecode_419512_422705;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i2_fu_aluDecode_419512_419659;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i3_fu_aluDecode_419512_419661;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i4_fu_aluDecode_419512_422598;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i5_fu_aluDecode_419512_422601;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i6_fu_aluDecode_419512_422604;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i7_fu_aluDecode_419512_422607;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i8_fu_aluDecode_419512_422610;
  wire [4:0] out_ui_cond_expr_FU_8_8_8_8_16_i9_fu_aluDecode_419512_422613;
  wire out_ui_eq_expr_FU_32_0_32_17_i0_fu_aluDecode_419512_422024;
  wire out_ui_eq_expr_FU_32_0_32_17_i1_fu_aluDecode_419512_422034;
  wire out_ui_eq_expr_FU_32_0_32_17_i2_fu_aluDecode_419512_422231;
  wire out_ui_eq_expr_FU_32_0_32_17_i3_fu_aluDecode_419512_422234;
  wire out_ui_eq_expr_FU_32_0_32_18_i0_fu_aluDecode_419512_422027;
  wire out_ui_eq_expr_FU_32_0_32_18_i1_fu_aluDecode_419512_422237;
  wire out_ui_eq_expr_FU_32_0_32_19_i0_fu_aluDecode_419512_422030;
  wire out_ui_eq_expr_FU_32_0_32_19_i1_fu_aluDecode_419512_422052;
  wire out_ui_eq_expr_FU_32_0_32_20_i0_fu_aluDecode_419512_422037;
  wire out_ui_eq_expr_FU_32_0_32_21_i0_fu_aluDecode_419512_422040;
  wire out_ui_eq_expr_FU_32_0_32_22_i0_fu_aluDecode_419512_422043;
  wire out_ui_eq_expr_FU_32_0_32_23_i0_fu_aluDecode_419512_422046;
  wire out_ui_eq_expr_FU_32_0_32_24_i0_fu_aluDecode_419512_422049;
  wire out_ui_eq_expr_FU_32_0_32_24_i1_fu_aluDecode_419512_422071;
  wire out_ui_eq_expr_FU_32_0_32_25_i0_fu_aluDecode_419512_422055;
  wire out_ui_eq_expr_FU_32_0_32_25_i1_fu_aluDecode_419512_422228;
  wire out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059;
  wire out_ui_eq_expr_FU_32_0_32_27_i0_fu_aluDecode_419512_422062;
  wire out_ui_eq_expr_FU_32_0_32_28_i0_fu_aluDecode_419512_422065;
  wire out_ui_eq_expr_FU_32_0_32_29_i0_fu_aluDecode_419512_422068;
  wire out_ui_eq_expr_FU_32_0_32_30_i0_fu_aluDecode_419512_422074;
  wire out_ui_eq_expr_FU_32_0_32_31_i0_fu_aluDecode_419512_422077;
  wire out_ui_eq_expr_FU_32_0_32_32_i0_fu_aluDecode_419512_422080;
  wire out_ui_eq_expr_FU_32_0_32_33_i0_fu_aluDecode_419512_422083;
  wire [16:0] out_ui_lshift_expr_FU_32_0_32_34_i0_fu_aluDecode_419512_422250;
  wire out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_36_i0_fu_aluDecode_419512_419706;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_36_i1_fu_aluDecode_419512_419731;
  
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_9_gimple_return_FU_15_i0_0_0_0 (.out1(out_MUX_9_gimple_return_FU_15_i0_0_0_0), .sel(selector_MUX_9_gimple_return_FU_15_i0_0_0_0), .in1(out_conv_out_reg_2_reg_2_5_32), .in2(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_419713_0));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_9_gimple_return_FU_15_i0_0_0_1 (.out1(out_MUX_9_gimple_return_FU_15_i0_0_0_1), .sel(selector_MUX_9_gimple_return_FU_15_i0_0_0_1), .in1(out_ARRAY_1D_STD_BRAM_NN_1_i0_array_419737_0), .in2(out_MUX_9_gimple_return_FU_15_i0_0_0_0));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file_a("array_ref_419713.mem"), .MEMORY_INIT_file_b("0_array_ref_419713.mem"), .n_elements(8), .data_size(32), .address_space_begin(MEM_var_419713_419512), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(1), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_419713_0 (.out1({null_out_signal_array_419713_0_out1_1, out_ARRAY_1D_STD_BRAM_NN_0_i0_array_419713_0}), .Sout_Rdata_ram({null_out_signal_array_419713_0_Sout_Rdata_ram_1, null_out_signal_array_419713_0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal_array_419713_0_Sout_DataRdy_1, null_out_signal_array_419713_0_Sout_DataRdy_0}), .proxy_out1({null_out_signal_array_419713_0_proxy_out1_1, null_out_signal_array_419713_0_proxy_out1_0}), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .in2({17'b00000000000000000, out_conv_out_reg_0_reg_0_18_17}), .in3({6'b000000, out_conv_out_const_1_7_6}), .in4({1'b0, out_const_2}), .sel_LOAD({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD}), .sel_STORE({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE}), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_DataRdy({1'b0, 1'b0}), .proxy_in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .proxy_in2({17'b00000000000000000, 17'b00000000000000000}), .proxy_in3({6'b000000, 6'b000000}), .proxy_sel_LOAD({1'b0, 1'b0}), .proxy_sel_STORE({1'b0, 1'b0}));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file_a("array_ref_419737.mem"), .MEMORY_INIT_file_b("0_array_ref_419737.mem"), .n_elements(8), .data_size(32), .address_space_begin(MEM_var_419737_419512), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(1), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_419737_0 (.out1({null_out_signal_array_419737_0_out1_1, out_ARRAY_1D_STD_BRAM_NN_1_i0_array_419737_0}), .Sout_Rdata_ram({null_out_signal_array_419737_0_Sout_Rdata_ram_1, null_out_signal_array_419737_0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal_array_419737_0_Sout_DataRdy_1, null_out_signal_array_419737_0_Sout_DataRdy_0}), .proxy_out1({null_out_signal_array_419737_0_proxy_out1_1, null_out_signal_array_419737_0_proxy_out1_0}), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .in2({17'b00000000000000000, out_conv_out_reg_1_reg_1_18_17}), .in3({6'b000000, out_conv_out_const_1_7_6}), .in4({1'b0, out_const_2}), .sel_LOAD({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD}), .sel_STORE({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE}), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_DataRdy({1'b0, 1'b0}), .proxy_in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .proxy_in2({17'b00000000000000000, 17'b00000000000000000}), .proxy_in3({6'b000000, 6'b000000}), .proxy_sel_LOAD({1'b0, 1'b0}), .proxy_sel_STORE({1'b0, 1'b0}));
  constant_value #(.BITSIZE_out1(1), .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(7), .value(7'b0100000)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(5), .value(5'b10001)) const_10 (.out1(out_const_10));
  constant_value #(.BITSIZE_out1(6), .value(6'b100011)) const_11 (.out1(out_const_11));
  constant_value #(.BITSIZE_out1(5), .value(5'b10010)) const_12 (.out1(out_const_12));
  constant_value #(.BITSIZE_out1(5), .value(5'b10011)) const_13 (.out1(out_const_13));
  constant_value #(.BITSIZE_out1(3), .value(3'b101)) const_14 (.out1(out_const_14));
  constant_value #(.BITSIZE_out1(4), .value(4'b1010)) const_15 (.out1(out_const_15));
  constant_value #(.BITSIZE_out1(5), .value(5'b10111)) const_16 (.out1(out_const_16));
  constant_value #(.BITSIZE_out1(2), .value(2'b11)) const_17 (.out1(out_const_17));
  constant_value #(.BITSIZE_out1(3), .value(3'b110)) const_18 (.out1(out_const_18));
  constant_value #(.BITSIZE_out1(4), .value(4'b1100)) const_19 (.out1(out_const_19));
  constant_value #(.BITSIZE_out1(1), .value(1'b1)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(7), .value(7'b1100011)) const_20 (.out1(out_const_20));
  constant_value #(.BITSIZE_out1(6), .value(6'b110011)) const_21 (.out1(out_const_21));
  constant_value #(.BITSIZE_out1(7), .value(7'b1100111)) const_22 (.out1(out_const_22));
  constant_value #(.BITSIZE_out1(4), .value(4'b1101)) const_23 (.out1(out_const_23));
  constant_value #(.BITSIZE_out1(6), .value(6'b110111)) const_24 (.out1(out_const_24));
  constant_value #(.BITSIZE_out1(7), .value(7'b1101111)) const_25 (.out1(out_const_25));
  constant_value #(.BITSIZE_out1(3), .value(3'b111)) const_26 (.out1(out_const_26));
  constant_value #(.BITSIZE_out1(4), .value(4'b1110)) const_27 (.out1(out_const_27));
  constant_value #(.BITSIZE_out1(4), .value(4'b1111)) const_28 (.out1(out_const_28));
  constant_value #(.BITSIZE_out1(5), .value(5'b11111)) const_29 (.out1(out_const_29));
  constant_value #(.BITSIZE_out1(2), .value(2'b10)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(16), .value(MEM_var_419713_419512)) const_30 (.out1(out_const_30));
  constant_value #(.BITSIZE_out1(16), .value(MEM_var_419737_419512)) const_31 (.out1(out_const_31));
  constant_value #(.BITSIZE_out1(3), .value(3'b100)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(4), .value(4'b1000)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(5), .value(5'b10000)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(6), .value(6'b100000)) const_7 (.out1(out_const_7));
  constant_value #(.BITSIZE_out1(7), .value(7'b1000000)) const_8 (.out1(out_const_8));
  constant_value #(.BITSIZE_out1(8), .value(8'b10000000)) const_9 (.out1(out_const_9));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_const_1_7_6 (.out1(out_conv_out_const_1_7_6), .in1(out_const_1));
  UUdata_converter_FU #(.BITSIZE_in1(16), .BITSIZE_out1(32)) conv_out_const_30_16_32 (.out1(out_conv_out_const_30_16_32), .in1(out_const_30));
  UUdata_converter_FU #(.BITSIZE_in1(16), .BITSIZE_out1(32)) conv_out_const_31_16_32 (.out1(out_conv_out_const_31_16_32), .in1(out_const_31));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_0_reg_0_18_17 (.out1(out_conv_out_reg_0_reg_0_18_17), .in1(out_reg_0_reg_0));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_1_reg_1_18_17 (.out1(out_conv_out_reg_1_reg_1_18_17), .in1(out_reg_1_reg_1));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(32)) conv_out_reg_2_reg_2_5_32 (.out1(out_conv_out_reg_2_reg_2_5_32), .in1(out_reg_2_reg_2));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(3), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_419630 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i0_fu_aluDecode_419512_419630), .in1(out_ui_eq_expr_FU_32_0_32_25_i1_fu_aluDecode_419512_422228), .in2(out_const_26), .in3(out_const_29));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_419632 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i1_fu_aluDecode_419512_419632), .in1(out_ui_eq_expr_FU_32_0_32_17_i2_fu_aluDecode_419512_422231), .in2(out_const_2), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i0_fu_aluDecode_419512_419630));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_419659 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i2_fu_aluDecode_419512_419659), .in1(out_ui_eq_expr_FU_32_0_32_17_i3_fu_aluDecode_419512_422234), .in2(out_const_15), .in3(out_const_29));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_419661 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i3_fu_aluDecode_419512_419661), .in1(out_ui_eq_expr_FU_32_0_32_18_i1_fu_aluDecode_419512_422237), .in2(out_const_5), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i2_fu_aluDecode_419512_419659));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_aluDecode_419512_419706 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_36_i0_fu_aluDecode_419512_419706), .in1(out_addr_expr_FU_5_i0_fu_aluDecode_419512_422241), .in2(out_ui_lshift_expr_FU_32_0_32_34_i0_fu_aluDecode_419512_422250));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_aluDecode_419512_419731 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_36_i1_fu_aluDecode_419512_419731), .in1(out_addr_expr_FU_4_i0_fu_aluDecode_419512_422248), .in2(out_ui_lshift_expr_FU_32_0_32_34_i0_fu_aluDecode_419512_422250));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422024 (.out1(out_ui_eq_expr_FU_32_0_32_17_i0_fu_aluDecode_419512_422024), .in1(in_port_funct7), .in2(out_const_0));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(6), .BITSIZE_out1(1)) fu_aluDecode_419512_422027 (.out1(out_ui_eq_expr_FU_32_0_32_18_i0_fu_aluDecode_419512_422027), .in1(in_port_funct7), .in2(out_const_7));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422030 (.out1(out_ui_eq_expr_FU_32_0_32_19_i0_fu_aluDecode_419512_422030), .in1(in_port_funct7), .in2(out_const_2));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422034 (.out1(out_ui_eq_expr_FU_32_0_32_17_i1_fu_aluDecode_419512_422034), .in1(in_port_funct3), .in2(out_const_0));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422037 (.out1(out_ui_eq_expr_FU_32_0_32_20_i0_fu_aluDecode_419512_422037), .in1(in_port_funct3), .in2(out_const_26));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422040 (.out1(out_ui_eq_expr_FU_32_0_32_21_i0_fu_aluDecode_419512_422040), .in1(in_port_funct3), .in2(out_const_18));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422043 (.out1(out_ui_eq_expr_FU_32_0_32_22_i0_fu_aluDecode_419512_422043), .in1(in_port_funct3), .in2(out_const_4));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(1)) fu_aluDecode_419512_422046 (.out1(out_ui_eq_expr_FU_32_0_32_23_i0_fu_aluDecode_419512_422046), .in1(in_port_funct3), .in2(out_const_3));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(1)) fu_aluDecode_419512_422049 (.out1(out_ui_eq_expr_FU_32_0_32_24_i0_fu_aluDecode_419512_422049), .in1(in_port_funct3), .in2(out_const_17));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422052 (.out1(out_ui_eq_expr_FU_32_0_32_19_i1_fu_aluDecode_419512_422052), .in1(in_port_funct3), .in2(out_const_2));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422055 (.out1(out_ui_eq_expr_FU_32_0_32_25_i0_fu_aluDecode_419512_422055), .in1(in_port_funct3), .in2(out_const_14));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(6), .BITSIZE_out1(1)) fu_aluDecode_419512_422059 (.out1(out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059), .in1(in_port_opcode), .in2(out_const_21));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(1)) fu_aluDecode_419512_422062 (.out1(out_ui_eq_expr_FU_32_0_32_27_i0_fu_aluDecode_419512_422062), .in1(in_port_opcode), .in2(out_const_13));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(6), .BITSIZE_out1(1)) fu_aluDecode_419512_422065 (.out1(out_ui_eq_expr_FU_32_0_32_28_i0_fu_aluDecode_419512_422065), .in1(in_port_opcode), .in2(out_const_24));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(1)) fu_aluDecode_419512_422068 (.out1(out_ui_eq_expr_FU_32_0_32_29_i0_fu_aluDecode_419512_422068), .in1(in_port_opcode), .in2(out_const_16));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(1)) fu_aluDecode_419512_422071 (.out1(out_ui_eq_expr_FU_32_0_32_24_i1_fu_aluDecode_419512_422071), .in1(in_port_opcode), .in2(out_const_17));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(6), .BITSIZE_out1(1)) fu_aluDecode_419512_422074 (.out1(out_ui_eq_expr_FU_32_0_32_30_i0_fu_aluDecode_419512_422074), .in1(in_port_opcode), .in2(out_const_11));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(7), .BITSIZE_out1(1)) fu_aluDecode_419512_422077 (.out1(out_ui_eq_expr_FU_32_0_32_31_i0_fu_aluDecode_419512_422077), .in1(in_port_opcode), .in2(out_const_25));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(7), .BITSIZE_out1(1)) fu_aluDecode_419512_422080 (.out1(out_ui_eq_expr_FU_32_0_32_32_i0_fu_aluDecode_419512_422080), .in1(in_port_opcode), .in2(out_const_22));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(7), .BITSIZE_out1(1)) fu_aluDecode_419512_422083 (.out1(out_ui_eq_expr_FU_32_0_32_33_i0_fu_aluDecode_419512_422083), .in1(in_port_opcode), .in2(out_const_20));
  ui_lt_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(4), .BITSIZE_out1(1)) fu_aluDecode_419512_422216 (.out1(out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216), .in1(in_port_funct3), .in2(out_const_5));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422228 (.out1(out_ui_eq_expr_FU_32_0_32_25_i1_fu_aluDecode_419512_422228), .in1(in_port_funct3), .in2(out_const_14));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422231 (.out1(out_ui_eq_expr_FU_32_0_32_17_i2_fu_aluDecode_419512_422231), .in1(in_port_funct3), .in2(out_const_0));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_aluDecode_419512_422234 (.out1(out_ui_eq_expr_FU_32_0_32_17_i3_fu_aluDecode_419512_422234), .in1(in_port_funct7), .in2(out_const_0));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(6), .BITSIZE_out1(1)) fu_aluDecode_419512_422237 (.out1(out_ui_eq_expr_FU_32_0_32_18_i1_fu_aluDecode_419512_422237), .in1(in_port_funct7), .in2(out_const_7));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_aluDecode_419512_422241 (.out1(out_addr_expr_FU_5_i0_fu_aluDecode_419512_422241), .in1(out_conv_out_const_30_16_32));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_aluDecode_419512_422248 (.out1(out_addr_expr_FU_4_i0_fu_aluDecode_419512_422248), .in1(out_conv_out_const_31_16_32));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(17), .PRECISION(32)) fu_aluDecode_419512_422250 (.out1(out_ui_lshift_expr_FU_32_0_32_34_i0_fu_aluDecode_419512_422250), .in1(in_port_funct3), .in2(out_const_3));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422598 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i4_fu_aluDecode_419512_422598), .in1(out_ui_eq_expr_FU_32_0_32_28_i0_fu_aluDecode_419512_422065), .in2(out_const_27), .in3(out_const_29));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422601 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i5_fu_aluDecode_419512_422601), .in1(out_ui_eq_expr_FU_32_0_32_29_i0_fu_aluDecode_419512_422068), .in2(out_const_28), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i4_fu_aluDecode_419512_422598));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422604 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i6_fu_aluDecode_419512_422604), .in1(out_ui_eq_expr_FU_32_0_32_24_i1_fu_aluDecode_419512_422071), .in2(out_const_6), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i5_fu_aluDecode_419512_422601));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422607 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i7_fu_aluDecode_419512_422607), .in1(out_ui_eq_expr_FU_32_0_32_30_i0_fu_aluDecode_419512_422074), .in2(out_const_10), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i6_fu_aluDecode_419512_422604));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422610 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i8_fu_aluDecode_419512_422610), .in1(out_ui_eq_expr_FU_32_0_32_31_i0_fu_aluDecode_419512_422077), .in2(out_const_12), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i7_fu_aluDecode_419512_422607));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422613 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i9_fu_aluDecode_419512_422613), .in1(out_ui_eq_expr_FU_32_0_32_32_i0_fu_aluDecode_419512_422080), .in2(out_const_13), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i8_fu_aluDecode_419512_422610));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422616 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i10_fu_aluDecode_419512_422616), .in1(out_ui_eq_expr_FU_32_0_32_19_i0_fu_aluDecode_419512_422030), .in2(out_const_23), .in3(out_const_29));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422619 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i11_fu_aluDecode_419512_422619), .in1(out_ui_eq_expr_FU_32_0_32_17_i1_fu_aluDecode_419512_422034), .in2(in_port_funct3), .in3(out_const_29));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(2), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422622 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i12_fu_aluDecode_419512_422622), .in1(out_ui_eq_expr_FU_32_0_32_20_i0_fu_aluDecode_419512_422037), .in2(out_const_3), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i11_fu_aluDecode_419512_422619));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(2), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422625 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i13_fu_aluDecode_419512_422625), .in1(out_ui_eq_expr_FU_32_0_32_21_i0_fu_aluDecode_419512_422040), .in2(out_const_17), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i12_fu_aluDecode_419512_422622));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(3), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422628 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i14_fu_aluDecode_419512_422628), .in1(out_ui_eq_expr_FU_32_0_32_22_i0_fu_aluDecode_419512_422043), .in2(out_const_4), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i13_fu_aluDecode_419512_422625));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(3), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422631 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i15_fu_aluDecode_419512_422631), .in1(out_ui_eq_expr_FU_32_0_32_23_i0_fu_aluDecode_419512_422046), .in2(out_const_14), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i14_fu_aluDecode_419512_422628));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(3), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422634 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i16_fu_aluDecode_419512_422634), .in1(out_ui_eq_expr_FU_32_0_32_24_i0_fu_aluDecode_419512_422049), .in2(out_const_18), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i15_fu_aluDecode_419512_422631));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(4), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422637 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i17_fu_aluDecode_419512_422637), .in1(out_ui_eq_expr_FU_32_0_32_19_i1_fu_aluDecode_419512_422052), .in2(out_const_19), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i16_fu_aluDecode_419512_422634));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_aluDecode_419512_422643 (.out1(out_lut_expr_FU_6_i0_fu_aluDecode_419512_422643), .in1(out_const_5), .in2(out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216), .in3(out_ui_eq_expr_FU_32_0_32_33_i0_fu_aluDecode_419512_422083), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_aluDecode_419512_422649 (.out1(out_lut_expr_FU_7_i0_fu_aluDecode_419512_422649), .in1(out_const_4), .in2(out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216), .in3(out_ui_eq_expr_FU_32_0_32_33_i0_fu_aluDecode_419512_422083), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_aluDecode_419512_422653 (.out1(out_lut_expr_FU_8_i0_fu_aluDecode_419512_422653), .in1(out_const_5), .in2(out_ui_eq_expr_FU_32_0_32_27_i0_fu_aluDecode_419512_422062), .in3(out_ui_eq_expr_FU_32_0_32_25_i0_fu_aluDecode_419512_422055), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(2), .BITSIZE_out1(1)) fu_aluDecode_419512_422659 (.out1(out_lut_expr_FU_9_i0_fu_aluDecode_419512_422659), .in1(out_const_3), .in2(out_ui_eq_expr_FU_32_0_32_27_i0_fu_aluDecode_419512_422062), .in3(out_ui_eq_expr_FU_32_0_32_25_i0_fu_aluDecode_419512_422055), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_aluDecode_419512_422663 (.out1(out_lut_expr_FU_10_i0_fu_aluDecode_419512_422663), .in1(out_const_5), .in2(out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059), .in3(out_ui_eq_expr_FU_32_0_32_18_i0_fu_aluDecode_419512_422027), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(2), .BITSIZE_out1(1)) fu_aluDecode_419512_422678 (.out1(out_lut_expr_FU_11_i0_fu_aluDecode_419512_422678), .in1(out_const_3), .in2(out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059), .in3(out_ui_eq_expr_FU_32_0_32_18_i0_fu_aluDecode_419512_422027), .in4(out_ui_eq_expr_FU_32_0_32_17_i0_fu_aluDecode_419512_422024), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  multi_read_cond_FU #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) fu_aluDecode_419512_422679 (.out1(out_multi_read_cond_FU_14_i0_fu_aluDecode_419512_422679), .in1({out_lut_expr_FU_6_i0_fu_aluDecode_419512_422643, out_lut_expr_FU_12_i0_fu_aluDecode_419512_422682}));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_aluDecode_419512_422682 (.out1(out_lut_expr_FU_12_i0_fu_aluDecode_419512_422682), .in1(out_const_9), .in2(out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216), .in3(out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059), .in4(out_ui_eq_expr_FU_32_0_32_17_i0_fu_aluDecode_419512_422024), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(7), .BITSIZE_out1(1)) fu_aluDecode_419512_422688 (.out1(out_lut_expr_FU_13_i0_fu_aluDecode_419512_422688), .in1(out_const_8), .in2(out_ui_lt_expr_FU_32_0_32_35_i0_fu_aluDecode_419512_422216), .in3(out_ui_eq_expr_FU_32_0_32_26_i0_fu_aluDecode_419512_422059), .in4(out_ui_eq_expr_FU_32_0_32_17_i0_fu_aluDecode_419512_422024), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422689 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i18_fu_aluDecode_419512_422689), .in1(out_lut_expr_FU_10_i0_fu_aluDecode_419512_422663), .in2(out_ui_cond_expr_FU_8_8_8_8_16_i1_fu_aluDecode_419512_419632), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i9_fu_aluDecode_419512_422613));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422692 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i19_fu_aluDecode_419512_422692), .in1(out_lut_expr_FU_8_i0_fu_aluDecode_419512_422653), .in2(out_ui_cond_expr_FU_8_8_8_8_16_i3_fu_aluDecode_419512_419661), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i18_fu_aluDecode_419512_422689));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422695 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i20_fu_aluDecode_419512_422695), .in1(out_lut_expr_FU_7_i0_fu_aluDecode_419512_422649), .in2(out_const_29), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i19_fu_aluDecode_419512_422692));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422698 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i21_fu_aluDecode_419512_422698), .in1(out_lut_expr_FU_9_i0_fu_aluDecode_419512_422659), .in2(out_ui_cond_expr_FU_8_8_8_8_16_i17_fu_aluDecode_419512_422637), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i20_fu_aluDecode_419512_422695));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422701 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i22_fu_aluDecode_419512_422701), .in1(out_lut_expr_FU_11_i0_fu_aluDecode_419512_422678), .in2(out_ui_cond_expr_FU_8_8_8_8_16_i10_fu_aluDecode_419512_422616), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i21_fu_aluDecode_419512_422698));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(5), .BITSIZE_in3(5), .BITSIZE_out1(5)) fu_aluDecode_419512_422705 (.out1(out_ui_cond_expr_FU_8_8_8_8_16_i23_fu_aluDecode_419512_422705), .in1(out_lut_expr_FU_13_i0_fu_aluDecode_419512_422688), .in2(out_const_29), .in3(out_ui_cond_expr_FU_8_8_8_8_16_i22_fu_aluDecode_419512_422701));
  register_STD #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_0 (.out1(out_reg_0_reg_0), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_36_i0_fu_aluDecode_419512_419706), .wenable(wrenable_reg_0));
  register_STD #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_1 (.out1(out_reg_1_reg_1), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_36_i1_fu_aluDecode_419512_419731), .wenable(wrenable_reg_1));
  register_STD #(.BITSIZE_in1(5), .BITSIZE_out1(5)) reg_2 (.out1(out_reg_2_reg_2), .clock(clock), .reset(reset), .in1(out_ui_cond_expr_FU_8_8_8_8_16_i23_fu_aluDecode_419512_422705), .wenable(wrenable_reg_2));
  // io-signal post fix
  assign return_port = out_MUX_9_gimple_return_FU_15_i0_0_0_1;
  assign OUT_MULTIIF_aluDecode_419512_422679 = out_multi_read_cond_FU_14_i0_fu_aluDecode_419512_422679;

endmodule

// FSM based controller description for aluDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller_aluDecode(done_port, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE, selector_MUX_9_gimple_return_FU_15_i0_0_0_0, selector_MUX_9_gimple_return_FU_15_i0_0_0_1, wrenable_reg_0, wrenable_reg_1, wrenable_reg_2, OUT_MULTIIF_aluDecode_419512_422679, clock, reset, start_port);
  // IN
  input [1:0] OUT_MULTIIF_aluDecode_419512_422679;
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE;
  output selector_MUX_9_gimple_return_FU_15_i0_0_0_0;
  output selector_MUX_9_gimple_return_FU_15_i0_0_0_1;
  output wrenable_reg_0;
  output wrenable_reg_1;
  output wrenable_reg_2;
  parameter [2:0] S_0 = 3'd0,
    S_2 = 3'd2,
    S_3 = 3'd3,
    S_4 = 3'd4,
    S_5 = 3'd5,
    S_1 = 3'd1;
  reg [2:0] _present_state, _next_state;
  reg done_port;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE;
  reg selector_MUX_9_gimple_return_FU_15_i0_0_0_0;
  reg selector_MUX_9_gimple_return_FU_15_i0_0_0_1;
  reg wrenable_reg_0;
  reg wrenable_reg_1;
  reg wrenable_reg_2;
  
  always @(posedge clock)
    if (reset == 1'b0) _present_state <= S_0;
    else _present_state <= _next_state;
  
  always @(*)
  begin
    done_port = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE = 1'b0;
    selector_MUX_9_gimple_return_FU_15_i0_0_0_0 = 1'b0;
    selector_MUX_9_gimple_return_FU_15_i0_0_0_1 = 1'b0;
    wrenable_reg_0 = 1'b0;
    wrenable_reg_1 = 1'b0;
    wrenable_reg_2 = 1'b0;
    case (_present_state)
      S_0 :
        if(start_port == 1'b1)
        begin
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_2 = 1'b1;
          if (OUT_MULTIIF_aluDecode_419512_422679[1] == 1'b1)
            begin
              _next_state = S_4;
              wrenable_reg_0 = 1'b0;
              wrenable_reg_2 = 1'b0;
            end
          else if (OUT_MULTIIF_aluDecode_419512_422679[0] == 1'b1)
            begin
              _next_state = S_2;
              wrenable_reg_1 = 1'b0;
              wrenable_reg_2 = 1'b0;
            end
          else
            begin
              _next_state = S_1;
              done_port = 1'b1;
              wrenable_reg_0 = 1'b0;
              wrenable_reg_1 = 1'b0;
            end
        end
        else
        begin
          selector_MUX_9_gimple_return_FU_15_i0_0_0_0 = 1'bX;
          selector_MUX_9_gimple_return_FU_15_i0_0_0_1 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_2 = 1'bX;
          _next_state = S_0;
        end
      S_2 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          _next_state = S_3;
          done_port = 1'b1;
        end
      S_3 :
        begin
          _next_state = S_0;
        end
      S_4 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD = 1'b1;
          _next_state = S_5;
          done_port = 1'b1;
        end
      S_5 :
        begin
          selector_MUX_9_gimple_return_FU_15_i0_0_0_1 = 1'b1;
          _next_state = S_0;
        end
      S_1 :
        begin
          selector_MUX_9_gimple_return_FU_15_i0_0_0_0 = 1'b1;
          _next_state = S_0;
        end
      default :
        begin
          _next_state = S_0;
          selector_MUX_9_gimple_return_FU_15_i0_0_0_0 = 1'bX;
          selector_MUX_9_gimple_return_FU_15_i0_0_0_1 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_2 = 1'bX;
        end
    endcase
  end
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Marco Lattuada <marco.lattuada@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module flipflop_AR(clock, reset, in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input clock;
  input reset;
  input in1;
  // OUT
  output out1;
  
  reg reg_out1 =0;
  assign out1 = reg_out1;
  always @(posedge clock )
    if (reset == 1'b0)
      reg_out1 <= {BITSIZE_out1{1'b0}};
    else
      reg_out1 <= in1;
endmodule

// Top component for aluDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module aluDecode(clock, reset, start_port, done_port, opcode, funct3, funct7, return_port);
  parameter MEM_var_419713_419512=32768, MEM_var_419737_419512=32768;
  // IN
  input clock;
  input reset;
  input start_port;
  input [31:0] opcode;
  input [31:0] funct3;
  input [31:0] funct7;
  // OUT
  output done_port;
  output [31:0] return_port;
  // Component and signal declarations
  wire [1:0] OUT_MULTIIF_aluDecode_419512_422679;
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE;
  wire selector_MUX_9_gimple_return_FU_15_i0_0_0_0;
  wire selector_MUX_9_gimple_return_FU_15_i0_0_0_1;
  wire wrenable_reg_0;
  wire wrenable_reg_1;
  wire wrenable_reg_2;
  
  controller_aluDecode Controller_i (.done_port(done_delayed_REG_signal_in), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE), .selector_MUX_9_gimple_return_FU_15_i0_0_0_0(selector_MUX_9_gimple_return_FU_15_i0_0_0_0), .selector_MUX_9_gimple_return_FU_15_i0_0_0_1(selector_MUX_9_gimple_return_FU_15_i0_0_0_1), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_2(wrenable_reg_2), .OUT_MULTIIF_aluDecode_419512_422679(OUT_MULTIIF_aluDecode_419512_422679), .clock(clock), .reset(reset), .start_port(start_port));
  datapath_aluDecode #(.MEM_var_419713_419512(MEM_var_419713_419512), .MEM_var_419737_419512(MEM_var_419737_419512)) Datapath_i (.return_port(return_port), .OUT_MULTIIF_aluDecode_419512_422679(OUT_MULTIIF_aluDecode_419512_422679), .clock(clock), .reset(reset), .in_port_opcode(opcode), .in_port_funct3(funct3), .in_port_funct7(funct7), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_1_i0_STORE), .selector_MUX_9_gimple_return_FU_15_i0_0_0_0(selector_MUX_9_gimple_return_FU_15_i0_0_0_0), .selector_MUX_9_gimple_return_FU_15_i0_0_0_1(selector_MUX_9_gimple_return_FU_15_i0_0_0_1), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_2(wrenable_reg_2));
  flipflop_AR #(.BITSIZE_in1(1), .BITSIZE_out1(1)) done_delayed_REG (.out1(done_delayed_REG_signal_out), .clock(clock), .reset(reset), .in1(done_delayed_REG_signal_in));
  // io-signal post fix
  assign done_port = done_delayed_REG_signal_out;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2013-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module PROXY_CTRLN(in1, in2, in3, in4, sel_LOAD, sel_STORE, out1, proxy_in1, proxy_in2, proxy_in3, proxy_sel_LOAD, proxy_sel_STORE, proxy_out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_in2=1, PORTSIZE_in2=2, BITSIZE_in3=1, PORTSIZE_in3=2, BITSIZE_in4=1, PORTSIZE_in4=2, BITSIZE_sel_LOAD=1, PORTSIZE_sel_LOAD=2, BITSIZE_sel_STORE=1, PORTSIZE_sel_STORE=2, BITSIZE_out1=1, PORTSIZE_out1=2, BITSIZE_proxy_in1=1, PORTSIZE_proxy_in1=2, BITSIZE_proxy_in2=1, PORTSIZE_proxy_in2=2, BITSIZE_proxy_in3=1, PORTSIZE_proxy_in3=2, BITSIZE_proxy_sel_LOAD=1, PORTSIZE_proxy_sel_LOAD=2, BITSIZE_proxy_sel_STORE=1, PORTSIZE_proxy_sel_STORE=2, BITSIZE_proxy_out1=1, PORTSIZE_proxy_out1=2;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  input [(PORTSIZE_in2*BITSIZE_in2)+(-1):0] in2;
  input [(PORTSIZE_in3*BITSIZE_in3)+(-1):0] in3;
  input [PORTSIZE_in4-1:0] in4;
  input [PORTSIZE_sel_LOAD-1:0] sel_LOAD;
  input [PORTSIZE_sel_STORE-1:0] sel_STORE;
  input [(PORTSIZE_proxy_out1*BITSIZE_proxy_out1)+(-1):0] proxy_out1;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  output [(PORTSIZE_proxy_in1*BITSIZE_proxy_in1)+(-1):0] proxy_in1;
  output [(PORTSIZE_proxy_in2*BITSIZE_proxy_in2)+(-1):0] proxy_in2;
  output [(PORTSIZE_proxy_in3*BITSIZE_proxy_in3)+(-1):0] proxy_in3;
  output [PORTSIZE_proxy_sel_LOAD-1:0] proxy_sel_LOAD;
  output [PORTSIZE_proxy_sel_STORE-1:0] proxy_sel_STORE;
  
  wire [PORTSIZE_sel_STORE-1:0] int_sel_STORE;
  wire [PORTSIZE_sel_LOAD-1:0] int_sel_LOAD;
  assign int_sel_STORE = sel_STORE & in4;
  assign int_sel_LOAD = sel_LOAD & in4;
  generate
  genvar i0;
  for (i0=0; i0<PORTSIZE_out1; i0=i0+1)
    begin : L0
      assign out1[(i0+1)*BITSIZE_out1-1:i0*BITSIZE_out1] = proxy_out1[(i0+1)*BITSIZE_proxy_out1-1:i0*BITSIZE_proxy_out1];
    end
  endgenerate
  generate
  genvar i1;
  for (i1=0; i1<PORTSIZE_in1; i1=i1+1)
    begin : L1
      assign proxy_in1[(i1+1)*BITSIZE_proxy_in1-1:i1*BITSIZE_proxy_in1] = int_sel_STORE[i1] ? in1[(i1+1)*BITSIZE_in1-1:i1*BITSIZE_in1] : 0;
    end
  endgenerate
  generate
  genvar i2;
  for (i2=0; i2<PORTSIZE_in2; i2=i2+1)
    begin : L2
      assign proxy_in2[(i2+1)*BITSIZE_proxy_in2-1:i2*BITSIZE_proxy_in2] = int_sel_LOAD[i2]|int_sel_STORE[i2] ? in2[(i2+1)*BITSIZE_in2-1:i2*BITSIZE_in2] : 0;
    end
  endgenerate
  generate
  genvar i3;
  for (i3=0; i3<PORTSIZE_in3; i3=i3+1)
    begin : L3
      assign proxy_in3[(i3+1)*BITSIZE_proxy_in3-1:i3*BITSIZE_proxy_in3] = int_sel_LOAD[i3]|int_sel_STORE[i3] ? in3[(i3+1)*BITSIZE_in3-1:i3*BITSIZE_in3] : 0;
    end
  endgenerate
  assign proxy_sel_LOAD = int_sel_LOAD;
  assign proxy_sel_STORE = int_sel_STORE;

endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2013-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module bus_merger(in1, out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  function [BITSIZE_out1-1:0] merge;
    input [BITSIZE_in1*PORTSIZE_in1-1:0] m;
    reg [BITSIZE_out1-1:0] res;
    integer i1;
  begin
    res={BITSIZE_in1{1'b0}};
    for(i1 = 0; i1 < PORTSIZE_in1; i1 = i1 + 1)
    begin
      res = res | m[i1*BITSIZE_in1 +:BITSIZE_in1];
    end
    merge = res;
  end
  endfunction
  
  assign out1 = merge(in1);
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module join_signal(in1, out1);
  parameter BITSIZE_in1=1, PORTSIZE_in1=2, BITSIZE_out1=1;
  // IN
  input [(PORTSIZE_in1*BITSIZE_in1)+(-1):0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  
  generate
  genvar i1;
  for (i1=0; i1<PORTSIZE_in1; i1=i1+1)
    begin : L1
      assign out1[(i1+1)*(BITSIZE_out1/PORTSIZE_in1)-1:i1*(BITSIZE_out1/PORTSIZE_in1)] = in1[(i1+1)*BITSIZE_in1-1:i1*BITSIZE_in1];
    end
  endgenerate
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module split_signal(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1, PORTSIZE_out1=2;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [(PORTSIZE_out1*BITSIZE_out1)+(-1):0] out1;
  assign out1 = in1;
endmodule

// This component is part of the BAMBU/PANDA IP LIBRARY
// Copyright (C) 2004-2020 Politecnico di Milano
// Author(s): Fabrizio Ferrandi <fabrizio.ferrandi@polimi.it>
// License: PANDA_LGPLv3
`timescale 1ns / 1ps
module ASSIGN_VECTOR_BOOL_FU(in1, out1);
  parameter BITSIZE_in1=1, BITSIZE_out1=1;
  // IN
  input [BITSIZE_in1-1:0] in1;
  // OUT
  output [BITSIZE_out1-1:0] out1;
  assign out1 = in1;
endmodule

// Datapath RTL description for instDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath_instDecode(clock, reset, in_port_P0, in_port_P1, proxy_out1_420170, proxy_in1_420170, proxy_in2_420170, proxy_in3_420170, proxy_sel_LOAD_420170, proxy_sel_STORE_420170, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2, selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0, selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2, selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0, selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1, selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3, selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3, selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0, selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1, selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0, fuselector_PROXY_CTRLN_0_i0_LOAD, fuselector_PROXY_CTRLN_0_i0_STORE, fuselector_PROXY_CTRLN_0_i1_LOAD, fuselector_PROXY_CTRLN_0_i1_STORE, wrenable_reg_0, wrenable_reg_1, wrenable_reg_10, wrenable_reg_11, wrenable_reg_12, wrenable_reg_13, wrenable_reg_14, wrenable_reg_15, wrenable_reg_16, wrenable_reg_17, wrenable_reg_18, wrenable_reg_19, wrenable_reg_2, wrenable_reg_20, wrenable_reg_21, wrenable_reg_3, wrenable_reg_4, wrenable_reg_5, wrenable_reg_6, wrenable_reg_7, wrenable_reg_8, wrenable_reg_9);
  // IN
  input clock;
  input reset;
  input [31:0] in_port_P0;
  input [31:0] in_port_P1;
  input [63:0] proxy_out1_420170;
  input selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0;
  input selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1;
  input selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2;
  input selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0;
  input selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1;
  input selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0;
  input selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1;
  input selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2;
  input selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0;
  input selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1;
  input selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0;
  input selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0;
  input selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1;
  input selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0;
  input fuselector_PROXY_CTRLN_0_i0_LOAD;
  input fuselector_PROXY_CTRLN_0_i0_STORE;
  input fuselector_PROXY_CTRLN_0_i1_LOAD;
  input fuselector_PROXY_CTRLN_0_i1_STORE;
  input wrenable_reg_0;
  input wrenable_reg_1;
  input wrenable_reg_10;
  input wrenable_reg_11;
  input wrenable_reg_12;
  input wrenable_reg_13;
  input wrenable_reg_14;
  input wrenable_reg_15;
  input wrenable_reg_16;
  input wrenable_reg_17;
  input wrenable_reg_18;
  input wrenable_reg_19;
  input wrenable_reg_2;
  input wrenable_reg_20;
  input wrenable_reg_21;
  input wrenable_reg_3;
  input wrenable_reg_4;
  input wrenable_reg_5;
  input wrenable_reg_6;
  input wrenable_reg_7;
  input wrenable_reg_8;
  input wrenable_reg_9;
  // OUT
  output [63:0] proxy_in1_420170;
  output [33:0] proxy_in2_420170;
  output [11:0] proxy_in3_420170;
  output [1:0] proxy_sel_LOAD_420170;
  output [1:0] proxy_sel_STORE_420170;
  // Component and signal declarations
  wire [31:0] null_out_signal_PROXY_CTRLN_0_i0_out1_0;
  wire [31:0] null_out_signal_PROXY_CTRLN_0_i0_out1_1;
  wire [31:0] out_IUdata_converter_FU_14_i0_fu_instDecode_419513_422266;
  wire [31:0] out_IUdata_converter_FU_16_i0_fu_instDecode_419513_422260;
  wire [31:0] out_IUdata_converter_FU_18_i0_fu_instDecode_419513_422257;
  wire [31:0] out_IUdata_converter_FU_20_i0_fu_instDecode_419513_422254;
  wire [31:0] out_IUdata_converter_FU_22_i0_fu_instDecode_419513_422263;
  wire [31:0] out_IUdata_converter_FU_23_i0_fu_instDecode_419513_422322;
  wire [31:0] out_MUX_10_PROXY_CTRLN_0_i1_0_0_0;
  wire [31:0] out_MUX_10_PROXY_CTRLN_0_i1_0_0_1;
  wire [31:0] out_MUX_10_PROXY_CTRLN_0_i1_0_0_2;
  wire [31:0] out_MUX_10_PROXY_CTRLN_0_i1_0_1_0;
  wire [31:0] out_MUX_10_PROXY_CTRLN_0_i1_0_1_1;
  wire [16:0] out_MUX_11_PROXY_CTRLN_0_i1_1_0_0;
  wire [16:0] out_MUX_11_PROXY_CTRLN_0_i1_1_0_1;
  wire [16:0] out_MUX_11_PROXY_CTRLN_0_i1_1_0_2;
  wire [16:0] out_MUX_11_PROXY_CTRLN_0_i1_1_1_0;
  wire [16:0] out_MUX_11_PROXY_CTRLN_0_i1_1_1_1;
  wire [6:0] out_MUX_12_PROXY_CTRLN_0_i1_2_0_0;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_0_0;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_0_1;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_0_2;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_0_3;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_1_0;
  wire [31:0] out_MUX_6_PROXY_CTRLN_0_i0_0_1_1;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_0_0;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_0_1;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_0_2;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_0_3;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_1_0;
  wire [16:0] out_MUX_7_PROXY_CTRLN_0_i0_1_1_1;
  wire [6:0] out_MUX_8_PROXY_CTRLN_0_i0_2_0_0;
  wire signed [31:0] out_UIdata_converter_FU_13_i0_fu_instDecode_419513_422308;
  wire signed [31:0] out_UIdata_converter_FU_15_i0_fu_instDecode_419513_422317;
  wire signed [31:0] out_UIdata_converter_FU_17_i0_fu_instDecode_419513_422314;
  wire signed [31:0] out_UIdata_converter_FU_19_i0_fu_instDecode_419513_422311;
  wire signed [31:0] out_UIdata_converter_FU_21_i0_fu_instDecode_419513_422320;
  wire signed [30:0] out_UIdata_converter_FU_3_i0_fu_instDecode_419513_422305;
  wire out_UUdata_converter_FU_37_i0_fu_instDecode_419513_422814;
  wire out_UUdata_converter_FU_40_i0_fu_instDecode_419513_422818;
  wire out_UUdata_converter_FU_42_i0_fu_instDecode_419513_422816;
  wire out_UUdata_converter_FU_53_i0_fu_instDecode_419513_422993;
  wire out_UUdata_converter_FU_55_i0_fu_instDecode_419513_422989;
  wire out_UUdata_converter_FU_57_i0_fu_instDecode_419513_422987;
  wire out_const_0;
  wire [4:0] out_const_1;
  wire [8:0] out_const_10;
  wire [11:0] out_const_11;
  wire [12:0] out_const_12;
  wire [13:0] out_const_13;
  wire [31:0] out_const_14;
  wire [5:0] out_const_15;
  wire [11:0] out_const_16;
  wire [3:0] out_const_17;
  wire [4:0] out_const_18;
  wire [2:0] out_const_19;
  wire [6:0] out_const_2;
  wire [4:0] out_const_20;
  wire [6:0] out_const_21;
  wire [3:0] out_const_22;
  wire [1:0] out_const_23;
  wire [2:0] out_const_24;
  wire [3:0] out_const_25;
  wire [4:0] out_const_26;
  wire [4:0] out_const_27;
  wire [2:0] out_const_28;
  wire [3:0] out_const_29;
  wire out_const_3;
  wire [4:0] out_const_30;
  wire [4:0] out_const_31;
  wire [7:0] out_const_32;
  wire [3:0] out_const_33;
  wire [4:0] out_const_34;
  wire [15:0] out_const_35;
  wire [4:0] out_const_36;
  wire [10:0] out_const_37;
  wire [7:0] out_const_38;
  wire [10:0] out_const_39;
  wire [1:0] out_const_4;
  wire [11:0] out_const_40;
  wire [6:0] out_const_41;
  wire [19:0] out_const_42;
  wire [10:0] out_const_43;
  wire [31:0] out_const_44;
  wire [31:0] out_const_45;
  wire [31:0] out_const_46;
  wire [2:0] out_const_5;
  wire [3:0] out_const_6;
  wire [4:0] out_const_7;
  wire [5:0] out_const_8;
  wire [7:0] out_const_9;
  wire [16:0] out_conv_in_port_P0_32_17;
  wire [5:0] out_conv_out_MUX_12_PROXY_CTRLN_0_i1_2_0_0_7_6;
  wire [5:0] out_conv_out_MUX_8_PROXY_CTRLN_0_i0_2_0_0_7_6;
  wire [6:0] out_conv_out_const_1_5_7;
  wire [31:0] out_conv_out_reg_0_reg_0_5_32;
  wire [16:0] out_conv_out_reg_10_reg_10_18_17;
  wire [16:0] out_conv_out_reg_11_reg_11_18_17;
  wire [16:0] out_conv_out_reg_12_reg_12_18_17;
  wire [16:0] out_conv_out_reg_13_reg_13_18_17;
  wire [16:0] out_conv_out_reg_14_reg_14_18_17;
  wire [16:0] out_conv_out_reg_15_reg_15_18_17;
  wire [16:0] out_conv_out_reg_16_reg_16_18_17;
  wire [16:0] out_conv_out_reg_17_reg_17_18_17;
  wire [16:0] out_conv_out_reg_18_reg_18_18_17;
  wire [16:0] out_conv_out_reg_19_reg_19_18_17;
  wire [31:0] out_conv_out_reg_1_reg_1_3_32;
  wire [16:0] out_conv_out_reg_20_reg_20_18_17;
  wire [31:0] out_conv_out_reg_2_reg_2_7_32;
  wire [7:0] out_conv_out_reg_3_reg_3_1_8;
  wire [7:0] out_conv_out_reg_4_reg_4_1_8;
  wire [7:0] out_conv_out_reg_6_reg_6_1_8;
  wire [7:0] out_conv_out_reg_7_reg_7_1_8;
  wire [7:0] out_conv_out_reg_8_reg_8_1_8;
  wire [7:0] out_conv_out_reg_9_reg_9_1_8;
  wire [31:0] out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826_5_32;
  wire [31:0] out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828_5_32;
  wire [16:0] out_conv_out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140_18_17;
  wire [31:0] out_conv_out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2_8_32;
  wire [31:0] out_conv_out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3_8_32;
  wire [31:0] out_conv_out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4_8_32;
  wire [31:0] out_conv_out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10_8_32;
  wire [31:0] out_conv_out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11_8_32;
  wire [31:0] out_conv_out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12_8_32;
  wire out_lut_expr_FU_11_i0_fu_instDecode_419513_423503;
  wire out_lut_expr_FU_12_i0_fu_instDecode_419513_422722;
  wire out_lut_expr_FU_25_i0_fu_instDecode_419513_423507;
  wire out_lut_expr_FU_26_i0_fu_instDecode_419513_423510;
  wire out_lut_expr_FU_27_i0_fu_instDecode_419513_423513;
  wire out_lut_expr_FU_28_i0_fu_instDecode_419513_422838;
  wire out_lut_expr_FU_29_i0_fu_instDecode_419513_423517;
  wire out_lut_expr_FU_30_i0_fu_instDecode_419513_422894;
  wire out_lut_expr_FU_31_i0_fu_instDecode_419513_422842;
  wire out_lut_expr_FU_32_i0_fu_instDecode_419513_422924;
  wire out_lut_expr_FU_33_i0_fu_instDecode_419513_423521;
  wire out_lut_expr_FU_34_i0_fu_instDecode_419513_423525;
  wire out_lut_expr_FU_35_i0_fu_instDecode_419513_423529;
  wire out_lut_expr_FU_36_i0_fu_instDecode_419513_423223;
  wire out_lut_expr_FU_38_i0_fu_instDecode_419513_422939;
  wire out_lut_expr_FU_39_i0_fu_instDecode_419513_423209;
  wire out_lut_expr_FU_41_i0_fu_instDecode_419513_423216;
  wire out_lut_expr_FU_43_i0_fu_instDecode_419513_422828;
  wire out_lut_expr_FU_44_i0_fu_instDecode_419513_423539;
  wire out_lut_expr_FU_45_i0_fu_instDecode_419513_422948;
  wire out_lut_expr_FU_46_i0_fu_instDecode_419513_423544;
  wire out_lut_expr_FU_47_i0_fu_instDecode_419513_422969;
  wire out_lut_expr_FU_48_i0_fu_instDecode_419513_423550;
  wire out_lut_expr_FU_49_i0_fu_instDecode_419513_423553;
  wire out_lut_expr_FU_50_i0_fu_instDecode_419513_423556;
  wire out_lut_expr_FU_51_i0_fu_instDecode_419513_423559;
  wire out_lut_expr_FU_52_i0_fu_instDecode_419513_423290;
  wire out_lut_expr_FU_54_i0_fu_instDecode_419513_423304;
  wire out_lut_expr_FU_56_i0_fu_instDecode_419513_423311;
  wire [4:0] out_reg_0_reg_0;
  wire [17:0] out_reg_10_reg_10;
  wire [17:0] out_reg_11_reg_11;
  wire [17:0] out_reg_12_reg_12;
  wire [17:0] out_reg_13_reg_13;
  wire [17:0] out_reg_14_reg_14;
  wire [17:0] out_reg_15_reg_15;
  wire [17:0] out_reg_16_reg_16;
  wire [17:0] out_reg_17_reg_17;
  wire [17:0] out_reg_18_reg_18;
  wire [17:0] out_reg_19_reg_19;
  wire [2:0] out_reg_1_reg_1;
  wire [17:0] out_reg_20_reg_20;
  wire [31:0] out_reg_21_reg_21;
  wire [6:0] out_reg_2_reg_2;
  wire out_reg_3_reg_3;
  wire out_reg_4_reg_4;
  wire [31:0] out_reg_5_reg_5;
  wire out_reg_6_reg_6;
  wire out_reg_7_reg_7;
  wire out_reg_8_reg_8;
  wire out_reg_9_reg_9;
  wire signed [5:0] out_rshift_expr_FU_32_0_32_59_i0_fu_instDecode_419513_419831;
  wire signed [6:0] out_rshift_expr_FU_32_0_32_59_i1_fu_instDecode_419513_420004;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_60_i0_fu_instDecode_419513_419882;
  wire signed [12:0] out_rshift_expr_FU_32_0_32_61_i0_fu_instDecode_419513_419898;
  wire signed [11:0] out_rshift_expr_FU_32_0_32_62_i0_fu_instDecode_419513_419967;
  wire signed [20:0] out_rshift_expr_FU_32_0_32_63_i0_fu_instDecode_419513_420063;
  wire [12:0] out_ui_bit_and_expr_FU_16_0_16_64_i0_fu_instDecode_419513_419920;
  wire [10:0] out_ui_bit_and_expr_FU_16_0_16_65_i0_fu_instDecode_419513_419978;
  wire [10:0] out_ui_bit_and_expr_FU_16_0_16_66_i0_fu_instDecode_419513_419992;
  wire [11:0] out_ui_bit_and_expr_FU_16_0_16_67_i0_fu_instDecode_419513_420046;
  wire [10:0] out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058;
  wire [0:0] out_ui_bit_and_expr_FU_1_0_1_69_i0_fu_instDecode_419513_420001;
  wire [30:0] out_ui_bit_and_expr_FU_32_0_32_70_i0_fu_instDecode_419513_419880;
  wire [19:0] out_ui_bit_and_expr_FU_32_0_32_71_i0_fu_instDecode_419513_420054;
  wire [19:0] out_ui_bit_and_expr_FU_32_0_32_72_i0_fu_instDecode_419513_420088;
  wire [4:0] out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826;
  wire [4:0] out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828;
  wire [4:0] out_ui_bit_and_expr_FU_8_0_8_73_i2_fu_instDecode_419513_419830;
  wire [2:0] out_ui_bit_and_expr_FU_8_0_8_74_i0_fu_instDecode_419513_419833;
  wire [6:0] out_ui_bit_and_expr_FU_8_0_8_75_i0_fu_instDecode_419513_419834;
  wire [4:0] out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999;
  wire [4:0] out_ui_bit_and_expr_FU_8_0_8_76_i1_fu_instDecode_419513_420060;
  wire [10:0] out_ui_bit_ior_expr_FU_0_16_16_77_i0_fu_instDecode_419513_419895;
  wire [12:0] out_ui_bit_ior_expr_FU_0_16_16_78_i0_fu_instDecode_419513_419922;
  wire [12:0] out_ui_bit_ior_expr_FU_0_16_16_79_i0_fu_instDecode_419513_419924;
  wire [10:0] out_ui_bit_ior_expr_FU_0_16_16_80_i0_fu_instDecode_419513_419982;
  wire [10:0] out_ui_bit_ior_expr_FU_0_16_16_81_i0_fu_instDecode_419513_419987;
  wire [10:0] out_ui_bit_ior_expr_FU_0_16_16_82_i0_fu_instDecode_419513_420016;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_83_i0_fu_instDecode_419513_420031;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_84_i0_fu_instDecode_419513_420036;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_85_i0_fu_instDecode_419513_420041;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_86_i0_fu_instDecode_419513_420076;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_87_i0_fu_instDecode_419513_420080;
  wire [19:0] out_ui_bit_ior_expr_FU_0_32_32_88_i0_fu_instDecode_419513_420084;
  wire [12:0] out_ui_bit_ior_expr_FU_16_0_16_89_i0_fu_instDecode_419513_419896;
  wire [11:0] out_ui_bit_ior_expr_FU_16_0_16_90_i0_fu_instDecode_419513_419975;
  wire [10:0] out_ui_bit_ior_expr_FU_16_0_16_91_i0_fu_instDecode_419513_420022;
  wire [20:0] out_ui_bit_ior_expr_FU_32_0_32_92_i0_fu_instDecode_419513_420072;
  wire [6:0] out_ui_bit_ior_expr_FU_8_0_8_93_i0_fu_instDecode_419513_420013;
  wire [30:0] out_ui_cond_expr_FU_32_32_32_32_94_i0_fu_instDecode_419513_422901;
  wire [30:0] out_ui_cond_expr_FU_32_32_32_32_94_i1_fu_instDecode_419513_422916;
  wire [30:0] out_ui_cond_expr_FU_32_32_32_32_94_i2_fu_instDecode_419513_422931;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i3_fu_instDecode_419513_422940;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i4_fu_instDecode_419513_422949;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i5_fu_instDecode_419513_422953;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i6_fu_instDecode_419513_422976;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i7_fu_instDecode_419513_422983;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_94_i8_fu_instDecode_419513_422991;
  wire out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339;
  wire out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446;
  wire out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315;
  wire out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319;
  wire out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323;
  wire out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327;
  wire out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331;
  wire out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_100_i0_fu_instDecode_419513_420069;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_101_i0_fu_instDecode_419513_423027;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_101_i1_fu_instDecode_419513_423037;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_101_i2_fu_instDecode_419513_423048;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_95_i0_fu_instDecode_419513_419881;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_96_i0_fu_instDecode_419513_419897;
  wire [13:0] out_ui_lshift_expr_FU_32_0_32_97_i0_fu_instDecode_419513_419919;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_98_i0_fu_instDecode_419513_419972;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_99_i0_fu_instDecode_419513_420010;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_103_i0_fu_instDecode_419513_420142;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_104_i0_fu_instDecode_419513_420144;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_105_i0_fu_instDecode_419513_420146;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_106_i0_fu_instDecode_419513_420148;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_107_i0_fu_instDecode_419513_420150;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_108_i0_fu_instDecode_419513_420152;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_109_i0_fu_instDecode_419513_420154;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_110_i0_fu_instDecode_419513_420156;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_111_i0_fu_instDecode_419513_420158;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_112_i0_fu_instDecode_419513_420160;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_113_i0_fu_instDecode_419513_420162;
  wire [11:0] out_ui_rshift_expr_FU_16_0_16_114_i0_fu_instDecode_419513_423022;
  wire [5:0] out_ui_rshift_expr_FU_32_0_32_115_i0_fu_instDecode_419513_419825;
  wire [11:0] out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827;
  wire [5:0] out_ui_rshift_expr_FU_32_0_32_117_i0_fu_instDecode_419513_419829;
  wire [3:0] out_ui_rshift_expr_FU_32_0_32_118_i0_fu_instDecode_419513_419832;
  wire [10:0] out_ui_rshift_expr_FU_32_0_32_119_i0_fu_instDecode_419513_419997;
  wire [12:0] out_ui_rshift_expr_FU_32_0_32_120_i0_fu_instDecode_419513_420051;
  wire [30:0] out_ui_rshift_expr_FU_32_0_32_121_i0_fu_instDecode_419513_423018;
  wire [30:0] out_ui_rshift_expr_FU_32_0_32_121_i1_fu_instDecode_419513_423030;
  wire [30:0] out_ui_rshift_expr_FU_32_0_32_121_i2_fu_instDecode_419513_423033;
  wire [18:0] out_ui_rshift_expr_FU_32_0_32_121_i3_fu_instDecode_419513_423040;
  wire [30:0] out_ui_rshift_expr_FU_32_0_32_121_i4_fu_instDecode_419513_423044;
  wire [31:0] out_vb_assign_conn_obj_0_ASSIGN_VECTOR_BOOL_FU_vb_assign_0;
  wire [7:0] out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2;
  wire [7:0] out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3;
  wire [7:0] out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4;
  wire [31:0] out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_1;
  wire [31:0] out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_5;
  wire [31:0] out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_6;
  wire [31:0] out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_7;
  wire [31:0] out_vb_assign_conn_obj_5_ASSIGN_VECTOR_BOOL_FU_vb_assign_8;
  wire [31:0] out_vb_assign_conn_obj_6_ASSIGN_VECTOR_BOOL_FU_vb_assign_9;
  wire [7:0] out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10;
  wire [7:0] out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11;
  wire [7:0] out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12;
  wire [63:0] sig_in_bus_mergerproxy_in1_4201700_0;
  wire [33:0] sig_in_bus_mergerproxy_in2_4201701_0;
  wire [11:0] sig_in_bus_mergerproxy_in3_4201702_0;
  wire [1:0] sig_in_bus_mergerproxy_sel_LOAD_4201703_0;
  wire [1:0] sig_in_bus_mergerproxy_sel_STORE_4201704_0;
  wire [63:0] sig_in_vector_bus_mergerproxy_in1_4201700_0;
  wire [33:0] sig_in_vector_bus_mergerproxy_in2_4201701_0;
  wire [11:0] sig_in_vector_bus_mergerproxy_in3_4201702_0;
  wire [1:0] sig_in_vector_bus_mergerproxy_sel_LOAD_4201703_0;
  wire [1:0] sig_in_vector_bus_mergerproxy_sel_STORE_4201704_0;
  wire [63:0] sig_out_bus_mergerproxy_in1_4201700_;
  wire [33:0] sig_out_bus_mergerproxy_in2_4201701_;
  wire [11:0] sig_out_bus_mergerproxy_in3_4201702_;
  wire [1:0] sig_out_bus_mergerproxy_sel_LOAD_4201703_;
  wire [1:0] sig_out_bus_mergerproxy_sel_STORE_4201704_;
  
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_0 (.out1(out_vb_assign_conn_obj_0_ASSIGN_VECTOR_BOOL_FU_vb_assign_0), .in1(out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826_5_32));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_1 (.out1(out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_1), .in1(out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828_5_32));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_10 (.out1(out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10), .in1(out_conv_out_reg_4_reg_4_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_11 (.out1(out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11), .in1(out_conv_out_reg_6_reg_6_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_12 (.out1(out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12), .in1(out_conv_out_reg_3_reg_3_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_2 (.out1(out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2), .in1(out_conv_out_reg_8_reg_8_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_3 (.out1(out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3), .in1(out_conv_out_reg_7_reg_7_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(8), .BITSIZE_out1(8)) ASSIGN_VECTOR_BOOL_FU_vb_assign_4 (.out1(out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4), .in1(out_conv_out_reg_9_reg_9_1_8));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_5 (.out1(out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_5), .in1(out_conv_out_reg_0_reg_0_5_32));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_6 (.out1(out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_6), .in1(out_reg_21_reg_21));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_7 (.out1(out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_7), .in1(out_conv_out_reg_1_reg_1_3_32));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_8 (.out1(out_vb_assign_conn_obj_5_ASSIGN_VECTOR_BOOL_FU_vb_assign_8), .in1(out_conv_out_reg_2_reg_2_7_32));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_9 (.out1(out_vb_assign_conn_obj_6_ASSIGN_VECTOR_BOOL_FU_vb_assign_9), .in1(out_reg_5_reg_5));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_10_PROXY_CTRLN_0_i1_0_0_0 (.out1(out_MUX_10_PROXY_CTRLN_0_i1_0_0_0), .sel(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0), .in1(out_vb_assign_conn_obj_0_ASSIGN_VECTOR_BOOL_FU_vb_assign_0), .in2(out_conv_out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3_8_32));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_10_PROXY_CTRLN_0_i1_0_0_1 (.out1(out_MUX_10_PROXY_CTRLN_0_i1_0_0_1), .sel(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1), .in1(out_vb_assign_conn_obj_2_ASSIGN_VECTOR_BOOL_FU_vb_assign_5), .in2(out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_6));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_10_PROXY_CTRLN_0_i1_0_0_2 (.out1(out_MUX_10_PROXY_CTRLN_0_i1_0_0_2), .sel(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2), .in1(out_vb_assign_conn_obj_5_ASSIGN_VECTOR_BOOL_FU_vb_assign_8), .in2(out_conv_out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11_8_32));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_10_PROXY_CTRLN_0_i1_0_1_0 (.out1(out_MUX_10_PROXY_CTRLN_0_i1_0_1_0), .sel(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0), .in1(out_MUX_10_PROXY_CTRLN_0_i1_0_0_0), .in2(out_MUX_10_PROXY_CTRLN_0_i1_0_0_1));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_10_PROXY_CTRLN_0_i1_0_1_1 (.out1(out_MUX_10_PROXY_CTRLN_0_i1_0_1_1), .sel(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1), .in1(out_MUX_10_PROXY_CTRLN_0_i1_0_0_2), .in2(out_MUX_10_PROXY_CTRLN_0_i1_0_1_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_PROXY_CTRLN_0_i1_1_0_0 (.out1(out_MUX_11_PROXY_CTRLN_0_i1_1_0_0), .sel(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0), .in1(out_conv_out_reg_19_reg_19_18_17), .in2(out_conv_out_reg_16_reg_16_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_PROXY_CTRLN_0_i1_1_0_1 (.out1(out_MUX_11_PROXY_CTRLN_0_i1_1_0_1), .sel(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1), .in1(out_conv_out_reg_13_reg_13_18_17), .in2(out_conv_out_reg_11_reg_11_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_PROXY_CTRLN_0_i1_1_0_2 (.out1(out_MUX_11_PROXY_CTRLN_0_i1_1_0_2), .sel(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2), .in1(out_conv_out_reg_10_reg_10_18_17), .in2(out_conv_in_port_P0_32_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_PROXY_CTRLN_0_i1_1_1_0 (.out1(out_MUX_11_PROXY_CTRLN_0_i1_1_1_0), .sel(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0), .in1(out_MUX_11_PROXY_CTRLN_0_i1_1_0_0), .in2(out_MUX_11_PROXY_CTRLN_0_i1_1_0_1));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_PROXY_CTRLN_0_i1_1_1_1 (.out1(out_MUX_11_PROXY_CTRLN_0_i1_1_1_1), .sel(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1), .in1(out_MUX_11_PROXY_CTRLN_0_i1_1_0_2), .in2(out_MUX_11_PROXY_CTRLN_0_i1_1_1_0));
  MUX_GATE #(.BITSIZE_in1(7), .BITSIZE_in2(7), .BITSIZE_out1(7)) MUX_12_PROXY_CTRLN_0_i1_2_0_0 (.out1(out_MUX_12_PROXY_CTRLN_0_i1_2_0_0), .sel(selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0), .in1(out_conv_out_const_1_5_7), .in2(out_const_2));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_0_0 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_0), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0), .in1(out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_1), .in2(out_conv_out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2_8_32));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_0_1 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_1), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1), .in1(out_conv_out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4_8_32), .in2(out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_7));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_0_2 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_2), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2), .in1(out_vb_assign_conn_obj_6_ASSIGN_VECTOR_BOOL_FU_vb_assign_9), .in2(out_conv_out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10_8_32));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_0_3 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_3), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3), .in1(out_conv_out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12_8_32), .in2(out_MUX_6_PROXY_CTRLN_0_i0_0_0_0));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_1_0 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_1_0), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0), .in1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_1), .in2(out_MUX_6_PROXY_CTRLN_0_i0_0_0_2));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_PROXY_CTRLN_0_i0_0_1_1 (.out1(out_MUX_6_PROXY_CTRLN_0_i0_0_1_1), .sel(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1), .in1(out_MUX_6_PROXY_CTRLN_0_i0_0_0_3), .in2(out_MUX_6_PROXY_CTRLN_0_i0_0_1_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_0_0 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_0), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0), .in1(out_conv_out_reg_20_reg_20_18_17), .in2(out_conv_out_reg_18_reg_18_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_0_1 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_1), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1), .in1(out_conv_out_reg_17_reg_17_18_17), .in2(out_conv_out_reg_15_reg_15_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_0_2 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_2), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2), .in1(out_conv_out_reg_14_reg_14_18_17), .in2(out_conv_out_reg_12_reg_12_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_0_3 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_3), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3), .in1(out_conv_out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140_18_17), .in2(out_MUX_7_PROXY_CTRLN_0_i0_1_0_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_1_0 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_1_0), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0), .in1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_1), .in2(out_MUX_7_PROXY_CTRLN_0_i0_1_0_2));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_PROXY_CTRLN_0_i0_1_1_1 (.out1(out_MUX_7_PROXY_CTRLN_0_i0_1_1_1), .sel(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1), .in1(out_MUX_7_PROXY_CTRLN_0_i0_1_0_3), .in2(out_MUX_7_PROXY_CTRLN_0_i0_1_1_0));
  MUX_GATE #(.BITSIZE_in1(7), .BITSIZE_in2(7), .BITSIZE_out1(7)) MUX_8_PROXY_CTRLN_0_i0_2_0_0 (.out1(out_MUX_8_PROXY_CTRLN_0_i0_2_0_0), .sel(selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0), .in1(out_conv_out_const_1_5_7), .in2(out_const_2));
  PROXY_CTRLN #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) PROXY_CTRLN_0_i0 (.out1({null_out_signal_PROXY_CTRLN_0_i0_out1_1, null_out_signal_PROXY_CTRLN_0_i0_out1_0}), .proxy_in1(sig_in_vector_bus_mergerproxy_in1_4201700_0), .proxy_in2(sig_in_vector_bus_mergerproxy_in2_4201701_0), .proxy_in3(sig_in_vector_bus_mergerproxy_in3_4201702_0), .proxy_sel_LOAD(sig_in_vector_bus_mergerproxy_sel_LOAD_4201703_0), .proxy_sel_STORE(sig_in_vector_bus_mergerproxy_sel_STORE_4201704_0), .in1({out_MUX_10_PROXY_CTRLN_0_i1_0_1_1, out_MUX_6_PROXY_CTRLN_0_i0_0_1_1}), .in2({out_MUX_11_PROXY_CTRLN_0_i1_1_1_1, out_MUX_7_PROXY_CTRLN_0_i0_1_1_1}), .in3({out_conv_out_MUX_12_PROXY_CTRLN_0_i1_2_0_0_7_6, out_conv_out_MUX_8_PROXY_CTRLN_0_i0_2_0_0_7_6}), .in4({out_const_3, out_const_3}), .sel_LOAD({fuselector_PROXY_CTRLN_0_i1_LOAD, fuselector_PROXY_CTRLN_0_i0_LOAD}), .sel_STORE({fuselector_PROXY_CTRLN_0_i1_STORE, fuselector_PROXY_CTRLN_0_i0_STORE}), .proxy_out1(proxy_out1_420170));
  bus_merger #(.BITSIZE_in1(64), .PORTSIZE_in1(1), .BITSIZE_out1(64)) bus_mergerproxy_in1_4201700_ (.out1(sig_out_bus_mergerproxy_in1_4201700_), .in1({sig_in_bus_mergerproxy_in1_4201700_0}));
  bus_merger #(.BITSIZE_in1(34), .PORTSIZE_in1(1), .BITSIZE_out1(34)) bus_mergerproxy_in2_4201701_ (.out1(sig_out_bus_mergerproxy_in2_4201701_), .in1({sig_in_bus_mergerproxy_in2_4201701_0}));
  bus_merger #(.BITSIZE_in1(12), .PORTSIZE_in1(1), .BITSIZE_out1(12)) bus_mergerproxy_in3_4201702_ (.out1(sig_out_bus_mergerproxy_in3_4201702_), .in1({sig_in_bus_mergerproxy_in3_4201702_0}));
  bus_merger #(.BITSIZE_in1(2), .PORTSIZE_in1(1), .BITSIZE_out1(2)) bus_mergerproxy_sel_LOAD_4201703_ (.out1(sig_out_bus_mergerproxy_sel_LOAD_4201703_), .in1({sig_in_bus_mergerproxy_sel_LOAD_4201703_0}));
  bus_merger #(.BITSIZE_in1(2), .PORTSIZE_in1(1), .BITSIZE_out1(2)) bus_mergerproxy_sel_STORE_4201704_ (.out1(sig_out_bus_mergerproxy_sel_STORE_4201704_), .in1({sig_in_bus_mergerproxy_sel_STORE_4201704_0}));
  constant_value #(.BITSIZE_out1(1), .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(5), .value(5'b01000)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(9), .value(9'b100000000)) const_10 (.out1(out_const_10));
  constant_value #(.BITSIZE_out1(12), .value(12'b100000000000)) const_11 (.out1(out_const_11));
  constant_value #(.BITSIZE_out1(13), .value(13'b1000000000000)) const_12 (.out1(out_const_12));
  constant_value #(.BITSIZE_out1(14), .value(14'b10000000000000)) const_13 (.out1(out_const_13));
  constant_value #(.BITSIZE_out1(32), .value(32'b10000000000011111111000000000000)) const_14 (.out1(out_const_14));
  constant_value #(.BITSIZE_out1(6), .value(6'b100001)) const_15 (.out1(out_const_15));
  constant_value #(.BITSIZE_out1(12), .value(12'b100011111111)) const_16 (.out1(out_const_16));
  constant_value #(.BITSIZE_out1(4), .value(4'b1001)) const_17 (.out1(out_const_17));
  constant_value #(.BITSIZE_out1(5), .value(5'b10011)) const_18 (.out1(out_const_18));
  constant_value #(.BITSIZE_out1(3), .value(3'b101)) const_19 (.out1(out_const_19));
  constant_value #(.BITSIZE_out1(7), .value(7'b0100000)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(5), .value(5'b10100)) const_20 (.out1(out_const_20));
  constant_value #(.BITSIZE_out1(7), .value(7'b1010001)) const_21 (.out1(out_const_21));
  constant_value #(.BITSIZE_out1(4), .value(4'b1011)) const_22 (.out1(out_const_22));
  constant_value #(.BITSIZE_out1(2), .value(2'b11)) const_23 (.out1(out_const_23));
  constant_value #(.BITSIZE_out1(3), .value(3'b110)) const_24 (.out1(out_const_24));
  constant_value #(.BITSIZE_out1(4), .value(4'b1100)) const_25 (.out1(out_const_25));
  constant_value #(.BITSIZE_out1(5), .value(5'b11000)) const_26 (.out1(out_const_26));
  constant_value #(.BITSIZE_out1(5), .value(5'b11001)) const_27 (.out1(out_const_27));
  constant_value #(.BITSIZE_out1(3), .value(3'b111)) const_28 (.out1(out_const_28));
  constant_value #(.BITSIZE_out1(4), .value(4'b1110)) const_29 (.out1(out_const_29));
  constant_value #(.BITSIZE_out1(1), .value(1'b1)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(5), .value(5'b11100)) const_30 (.out1(out_const_30));
  constant_value #(.BITSIZE_out1(5), .value(5'b11101)) const_31 (.out1(out_const_31));
  constant_value #(.BITSIZE_out1(8), .value(8'b11101100)) const_32 (.out1(out_const_32));
  constant_value #(.BITSIZE_out1(4), .value(4'b1111)) const_33 (.out1(out_const_33));
  constant_value #(.BITSIZE_out1(5), .value(5'b11110)) const_34 (.out1(out_const_34));
  constant_value #(.BITSIZE_out1(16), .value(16'b1111011100000000)) const_35 (.out1(out_const_35));
  constant_value #(.BITSIZE_out1(5), .value(5'b11111)) const_36 (.out1(out_const_36));
  constant_value #(.BITSIZE_out1(11), .value(11'b11111000000)) const_37 (.out1(out_const_37));
  constant_value #(.BITSIZE_out1(8), .value(8'b11111011)) const_38 (.out1(out_const_38));
  constant_value #(.BITSIZE_out1(11), .value(11'b11111100000)) const_39 (.out1(out_const_39));
  constant_value #(.BITSIZE_out1(2), .value(2'b10)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(12), .value(12'b111111000000)) const_40 (.out1(out_const_40));
  constant_value #(.BITSIZE_out1(7), .value(7'b1111111)) const_41 (.out1(out_const_41));
  constant_value #(.BITSIZE_out1(20), .value(20'b11111111000000000000)) const_42 (.out1(out_const_42));
  constant_value #(.BITSIZE_out1(11), .value(11'b11111111111)) const_43 (.out1(out_const_43));
  constant_value #(.BITSIZE_out1(32), .value(32'b11111111111100000000000000000000)) const_44 (.out1(out_const_44));
  constant_value #(.BITSIZE_out1(32), .value(32'b11111111111111111111000000000000)) const_45 (.out1(out_const_45));
  constant_value #(.BITSIZE_out1(32), .value(32'b11111111111111111111100000000000)) const_46 (.out1(out_const_46));
  constant_value #(.BITSIZE_out1(3), .value(3'b100)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(4), .value(4'b1000)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(5), .value(5'b10000)) const_7 (.out1(out_const_7));
  constant_value #(.BITSIZE_out1(6), .value(6'b100000)) const_8 (.out1(out_const_8));
  constant_value #(.BITSIZE_out1(8), .value(8'b10000000)) const_9 (.out1(out_const_9));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) conv_in_port_P0_32_17 (.out1(out_conv_in_port_P0_32_17), .in1(in_port_P0));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_MUX_12_PROXY_CTRLN_0_i1_2_0_0_7_6 (.out1(out_conv_out_MUX_12_PROXY_CTRLN_0_i1_2_0_0_7_6), .in1(out_MUX_12_PROXY_CTRLN_0_i1_2_0_0));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_MUX_8_PROXY_CTRLN_0_i0_2_0_0_7_6 (.out1(out_conv_out_MUX_8_PROXY_CTRLN_0_i0_2_0_0_7_6), .in1(out_MUX_8_PROXY_CTRLN_0_i0_2_0_0));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(7)) conv_out_const_1_5_7 (.out1(out_conv_out_const_1_5_7), .in1(out_const_1));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(32)) conv_out_reg_0_reg_0_5_32 (.out1(out_conv_out_reg_0_reg_0_5_32), .in1(out_reg_0_reg_0));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_10_reg_10_18_17 (.out1(out_conv_out_reg_10_reg_10_18_17), .in1(out_reg_10_reg_10));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_11_reg_11_18_17 (.out1(out_conv_out_reg_11_reg_11_18_17), .in1(out_reg_11_reg_11));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_12_reg_12_18_17 (.out1(out_conv_out_reg_12_reg_12_18_17), .in1(out_reg_12_reg_12));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_13_reg_13_18_17 (.out1(out_conv_out_reg_13_reg_13_18_17), .in1(out_reg_13_reg_13));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_14_reg_14_18_17 (.out1(out_conv_out_reg_14_reg_14_18_17), .in1(out_reg_14_reg_14));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_15_reg_15_18_17 (.out1(out_conv_out_reg_15_reg_15_18_17), .in1(out_reg_15_reg_15));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_16_reg_16_18_17 (.out1(out_conv_out_reg_16_reg_16_18_17), .in1(out_reg_16_reg_16));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_17_reg_17_18_17 (.out1(out_conv_out_reg_17_reg_17_18_17), .in1(out_reg_17_reg_17));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_18_reg_18_18_17 (.out1(out_conv_out_reg_18_reg_18_18_17), .in1(out_reg_18_reg_18));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_19_reg_19_18_17 (.out1(out_conv_out_reg_19_reg_19_18_17), .in1(out_reg_19_reg_19));
  UUdata_converter_FU #(.BITSIZE_in1(3), .BITSIZE_out1(32)) conv_out_reg_1_reg_1_3_32 (.out1(out_conv_out_reg_1_reg_1_3_32), .in1(out_reg_1_reg_1));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_20_reg_20_18_17 (.out1(out_conv_out_reg_20_reg_20_18_17), .in1(out_reg_20_reg_20));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(32)) conv_out_reg_2_reg_2_7_32 (.out1(out_conv_out_reg_2_reg_2_7_32), .in1(out_reg_2_reg_2));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_3_reg_3_1_8 (.out1(out_conv_out_reg_3_reg_3_1_8), .in1(out_reg_3_reg_3));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_4_reg_4_1_8 (.out1(out_conv_out_reg_4_reg_4_1_8), .in1(out_reg_4_reg_4));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_6_reg_6_1_8 (.out1(out_conv_out_reg_6_reg_6_1_8), .in1(out_reg_6_reg_6));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_7_reg_7_1_8 (.out1(out_conv_out_reg_7_reg_7_1_8), .in1(out_reg_7_reg_7));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_8_reg_8_1_8 (.out1(out_conv_out_reg_8_reg_8_1_8), .in1(out_reg_8_reg_8));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(8)) conv_out_reg_9_reg_9_1_8 (.out1(out_conv_out_reg_9_reg_9_1_8), .in1(out_reg_9_reg_9));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(32)) conv_out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826_5_32 (.out1(out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826_5_32), .in1(out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(32)) conv_out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828_5_32 (.out1(out_conv_out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828_5_32), .in1(out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140_18_17 (.out1(out_conv_out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140_18_17), .in1(out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2_8_32 (.out1(out_conv_out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2_8_32), .in1(out_vb_assign_conn_obj_10_ASSIGN_VECTOR_BOOL_FU_vb_assign_2));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3_8_32 (.out1(out_conv_out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3_8_32), .in1(out_vb_assign_conn_obj_11_ASSIGN_VECTOR_BOOL_FU_vb_assign_3));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4_8_32 (.out1(out_conv_out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4_8_32), .in1(out_vb_assign_conn_obj_12_ASSIGN_VECTOR_BOOL_FU_vb_assign_4));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10_8_32 (.out1(out_conv_out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10_8_32), .in1(out_vb_assign_conn_obj_7_ASSIGN_VECTOR_BOOL_FU_vb_assign_10));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11_8_32 (.out1(out_conv_out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11_8_32), .in1(out_vb_assign_conn_obj_8_ASSIGN_VECTOR_BOOL_FU_vb_assign_11));
  UUdata_converter_FU #(.BITSIZE_in1(8), .BITSIZE_out1(32)) conv_out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12_8_32 (.out1(out_conv_out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12_8_32), .in1(out_vb_assign_conn_obj_9_ASSIGN_VECTOR_BOOL_FU_vb_assign_12));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(4), .BITSIZE_out1(6), .PRECISION(32)) fu_instDecode_419513_419825 (.out1(out_ui_rshift_expr_FU_32_0_32_115_i0_fu_instDecode_419513_419825), .in1(in_port_P1), .in2(out_const_33));
  ui_bit_and_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(5), .BITSIZE_out1(5)) fu_instDecode_419513_419826 (.out1(out_ui_bit_and_expr_FU_8_0_8_73_i0_fu_instDecode_419513_419826), .in1(out_ui_rshift_expr_FU_32_0_32_115_i0_fu_instDecode_419513_419825), .in2(out_const_36));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(12), .PRECISION(32)) fu_instDecode_419513_419827 (.out1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in1(in_port_P1), .in2(out_const_20));
  ui_bit_and_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(5), .BITSIZE_out1(5)) fu_instDecode_419513_419828 (.out1(out_ui_bit_and_expr_FU_8_0_8_73_i1_fu_instDecode_419513_419828), .in1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in2(out_const_36));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(6), .PRECISION(32)) fu_instDecode_419513_419829 (.out1(out_ui_rshift_expr_FU_32_0_32_117_i0_fu_instDecode_419513_419829), .in1(in_port_P1), .in2(out_const_28));
  ui_bit_and_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(5), .BITSIZE_out1(5)) fu_instDecode_419513_419830 (.out1(out_ui_bit_and_expr_FU_8_0_8_73_i2_fu_instDecode_419513_419830), .in1(out_ui_rshift_expr_FU_32_0_32_117_i0_fu_instDecode_419513_419829), .in2(out_const_36));
  rshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(5), .BITSIZE_out1(6), .PRECISION(32)) fu_instDecode_419513_419831 (.out1(out_rshift_expr_FU_32_0_32_59_i0_fu_instDecode_419513_419831), .in1(out_UIdata_converter_FU_3_i0_fu_instDecode_419513_422305), .in2(out_const_27));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(4), .BITSIZE_out1(4), .PRECISION(32)) fu_instDecode_419513_419832 (.out1(out_ui_rshift_expr_FU_32_0_32_118_i0_fu_instDecode_419513_419832), .in1(in_port_P1), .in2(out_const_25));
  ui_bit_and_expr_FU #(.BITSIZE_in1(4), .BITSIZE_in2(3), .BITSIZE_out1(3)) fu_instDecode_419513_419833 (.out1(out_ui_bit_and_expr_FU_8_0_8_74_i0_fu_instDecode_419513_419833), .in1(out_ui_rshift_expr_FU_32_0_32_118_i0_fu_instDecode_419513_419832), .in2(out_const_28));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(7), .BITSIZE_out1(7)) fu_instDecode_419513_419834 (.out1(out_ui_bit_and_expr_FU_8_0_8_75_i0_fu_instDecode_419513_419834), .in1(in_port_P1), .in2(out_const_41));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(31)) fu_instDecode_419513_419880 (.out1(out_ui_bit_and_expr_FU_32_0_32_70_i0_fu_instDecode_419513_419880), .in1(in_port_P1), .in2(out_const_45));
  ui_lshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(1), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_419881 (.out1(out_ui_lshift_expr_FU_32_0_32_95_i0_fu_instDecode_419513_419881), .in1(out_ui_bit_and_expr_FU_32_0_32_70_i0_fu_instDecode_419513_419880), .in2(out_const_3));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_instDecode_419513_419882 (.out1(out_rshift_expr_FU_32_0_32_60_i0_fu_instDecode_419513_419882), .in1(out_UIdata_converter_FU_13_i0_fu_instDecode_419513_422308), .in2(out_const_3));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(11), .BITSIZE_out1(11)) fu_instDecode_419513_419895 (.out1(out_ui_bit_ior_expr_FU_0_16_16_77_i0_fu_instDecode_419513_419895), .in1(out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999), .in2(out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(32), .BITSIZE_out1(13)) fu_instDecode_419513_419896 (.out1(out_ui_bit_ior_expr_FU_16_0_16_89_i0_fu_instDecode_419513_419896), .in1(out_ui_bit_ior_expr_FU_0_16_16_77_i0_fu_instDecode_419513_419895), .in2(out_const_45));
  ui_lshift_expr_FU #(.BITSIZE_in1(13), .BITSIZE_in2(5), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_419897 (.out1(out_ui_lshift_expr_FU_32_0_32_96_i0_fu_instDecode_419513_419897), .in1(out_ui_bit_ior_expr_FU_16_0_16_89_i0_fu_instDecode_419513_419896), .in2(out_const_18));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(13), .PRECISION(32)) fu_instDecode_419513_419898 (.out1(out_rshift_expr_FU_32_0_32_61_i0_fu_instDecode_419513_419898), .in1(out_UIdata_converter_FU_19_i0_fu_instDecode_419513_422311), .in2(out_const_18));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3), .BITSIZE_out1(14), .PRECISION(32)) fu_instDecode_419513_419919 (.out1(out_ui_lshift_expr_FU_32_0_32_97_i0_fu_instDecode_419513_419919), .in1(in_port_P1), .in2(out_const_19));
  ui_bit_and_expr_FU #(.BITSIZE_in1(14), .BITSIZE_in2(13), .BITSIZE_out1(13)) fu_instDecode_419513_419920 (.out1(out_ui_bit_and_expr_FU_16_0_16_64_i0_fu_instDecode_419513_419920), .in1(out_ui_lshift_expr_FU_32_0_32_97_i0_fu_instDecode_419513_419919), .in2(out_const_12));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(13), .BITSIZE_in2(11), .BITSIZE_out1(13)) fu_instDecode_419513_419922 (.out1(out_ui_bit_ior_expr_FU_0_16_16_78_i0_fu_instDecode_419513_419922), .in1(out_ui_bit_and_expr_FU_16_0_16_64_i0_fu_instDecode_419513_419920), .in2(out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(13), .BITSIZE_in2(5), .BITSIZE_out1(13)) fu_instDecode_419513_419924 (.out1(out_ui_bit_ior_expr_FU_0_16_16_79_i0_fu_instDecode_419513_419924), .in1(out_ui_bit_ior_expr_FU_0_16_16_78_i0_fu_instDecode_419513_419922), .in2(out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(12), .PRECISION(32)) fu_instDecode_419513_419967 (.out1(out_rshift_expr_FU_32_0_32_62_i0_fu_instDecode_419513_419967), .in1(out_UIdata_converter_FU_21_i0_fu_instDecode_419513_422320), .in2(out_const_20));
  ui_lshift_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(5), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_419972 (.out1(out_ui_lshift_expr_FU_32_0_32_98_i0_fu_instDecode_419513_419972), .in1(out_ui_bit_ior_expr_FU_16_0_16_90_i0_fu_instDecode_419513_419975), .in2(out_const_20));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(32), .BITSIZE_out1(12)) fu_instDecode_419513_419975 (.out1(out_ui_bit_ior_expr_FU_16_0_16_90_i0_fu_instDecode_419513_419975), .in1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in2(out_const_46));
  ui_bit_and_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(11), .BITSIZE_out1(11)) fu_instDecode_419513_419978 (.out1(out_ui_bit_and_expr_FU_16_0_16_65_i0_fu_instDecode_419513_419978), .in1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in2(out_const_43));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(1), .BITSIZE_out1(11)) fu_instDecode_419513_419982 (.out1(out_ui_bit_ior_expr_FU_0_16_16_80_i0_fu_instDecode_419513_419982), .in1(out_ui_bit_ior_expr_FU_0_16_16_81_i0_fu_instDecode_419513_419987), .in2(out_ui_bit_and_expr_FU_1_0_1_69_i0_fu_instDecode_419513_420001));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(5), .BITSIZE_out1(11)) fu_instDecode_419513_419987 (.out1(out_ui_bit_ior_expr_FU_0_16_16_81_i0_fu_instDecode_419513_419987), .in1(out_ui_bit_and_expr_FU_16_0_16_66_i0_fu_instDecode_419513_419992), .in2(out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999));
  ui_bit_and_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(12), .BITSIZE_out1(11)) fu_instDecode_419513_419992 (.out1(out_ui_bit_and_expr_FU_16_0_16_66_i0_fu_instDecode_419513_419992), .in1(out_ui_rshift_expr_FU_32_0_32_119_i0_fu_instDecode_419513_419997), .in2(out_const_40));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(11), .PRECISION(32)) fu_instDecode_419513_419997 (.out1(out_ui_rshift_expr_FU_32_0_32_119_i0_fu_instDecode_419513_419997), .in1(in_port_P1), .in2(out_const_18));
  ui_bit_and_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(5), .BITSIZE_out1(5)) fu_instDecode_419513_419999 (.out1(out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999), .in1(out_ui_rshift_expr_FU_32_0_32_117_i0_fu_instDecode_419513_419829), .in2(out_const_34));
  ui_bit_and_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_instDecode_419513_420001 (.out1(out_ui_bit_and_expr_FU_1_0_1_69_i0_fu_instDecode_419513_420001), .in1(out_ui_rshift_expr_FU_32_0_32_117_i0_fu_instDecode_419513_419829), .in2(out_const_3));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5), .BITSIZE_out1(7), .PRECISION(32)) fu_instDecode_419513_420004 (.out1(out_rshift_expr_FU_32_0_32_59_i1_fu_instDecode_419513_420004), .in1(out_UIdata_converter_FU_15_i0_fu_instDecode_419513_422317), .in2(out_const_27));
  ui_lshift_expr_FU #(.BITSIZE_in1(7), .BITSIZE_in2(5), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_420010 (.out1(out_ui_lshift_expr_FU_32_0_32_99_i0_fu_instDecode_419513_420010), .in1(out_ui_bit_ior_expr_FU_8_0_8_93_i0_fu_instDecode_419513_420013), .in2(out_const_27));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(32), .BITSIZE_out1(7)) fu_instDecode_419513_420013 (.out1(out_ui_bit_ior_expr_FU_8_0_8_93_i0_fu_instDecode_419513_420013), .in1(out_ui_bit_ior_expr_FU_0_16_16_82_i0_fu_instDecode_419513_420016), .in2(out_const_46));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(11), .BITSIZE_in2(1), .BITSIZE_out1(11)) fu_instDecode_419513_420016 (.out1(out_ui_bit_ior_expr_FU_0_16_16_82_i0_fu_instDecode_419513_420016), .in1(out_ui_bit_ior_expr_FU_16_0_16_91_i0_fu_instDecode_419513_420022), .in2(out_ui_bit_and_expr_FU_1_0_1_69_i0_fu_instDecode_419513_420001));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(11), .BITSIZE_out1(11)) fu_instDecode_419513_420022 (.out1(out_ui_bit_ior_expr_FU_16_0_16_91_i0_fu_instDecode_419513_420022), .in1(out_ui_bit_and_expr_FU_8_0_8_76_i0_fu_instDecode_419513_419999), .in2(out_const_37));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(5), .BITSIZE_out1(20)) fu_instDecode_419513_420031 (.out1(out_ui_bit_ior_expr_FU_0_32_32_83_i0_fu_instDecode_419513_420031), .in1(out_ui_bit_ior_expr_FU_0_32_32_84_i0_fu_instDecode_419513_420036), .in2(out_ui_bit_and_expr_FU_8_0_8_76_i1_fu_instDecode_419513_420060));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(11), .BITSIZE_out1(20)) fu_instDecode_419513_420036 (.out1(out_ui_bit_ior_expr_FU_0_32_32_84_i0_fu_instDecode_419513_420036), .in1(out_ui_bit_ior_expr_FU_0_32_32_85_i0_fu_instDecode_419513_420041), .in2(out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(20), .BITSIZE_out1(20)) fu_instDecode_419513_420041 (.out1(out_ui_bit_ior_expr_FU_0_32_32_85_i0_fu_instDecode_419513_420041), .in1(out_ui_bit_and_expr_FU_16_0_16_67_i0_fu_instDecode_419513_420046), .in2(out_ui_bit_and_expr_FU_32_0_32_71_i0_fu_instDecode_419513_420054));
  ui_bit_and_expr_FU #(.BITSIZE_in1(13), .BITSIZE_in2(12), .BITSIZE_out1(12)) fu_instDecode_419513_420046 (.out1(out_ui_bit_and_expr_FU_16_0_16_67_i0_fu_instDecode_419513_420046), .in1(out_ui_rshift_expr_FU_32_0_32_120_i0_fu_instDecode_419513_420051), .in2(out_const_11));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(4), .BITSIZE_out1(13), .PRECISION(32)) fu_instDecode_419513_420051 (.out1(out_ui_rshift_expr_FU_32_0_32_120_i0_fu_instDecode_419513_420051), .in1(in_port_P1), .in2(out_const_17));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(20)) fu_instDecode_419513_420054 (.out1(out_ui_bit_and_expr_FU_32_0_32_71_i0_fu_instDecode_419513_420054), .in1(in_port_P1), .in2(out_const_14));
  ui_bit_and_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(11), .BITSIZE_out1(11)) fu_instDecode_419513_420058 (.out1(out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058), .in1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in2(out_const_39));
  ui_bit_and_expr_FU #(.BITSIZE_in1(12), .BITSIZE_in2(5), .BITSIZE_out1(5)) fu_instDecode_419513_420060 (.out1(out_ui_bit_and_expr_FU_8_0_8_76_i1_fu_instDecode_419513_420060), .in1(out_ui_rshift_expr_FU_32_0_32_116_i0_fu_instDecode_419513_419827), .in2(out_const_34));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(4), .BITSIZE_out1(21), .PRECISION(32)) fu_instDecode_419513_420063 (.out1(out_rshift_expr_FU_32_0_32_63_i0_fu_instDecode_419513_420063), .in1(out_UIdata_converter_FU_17_i0_fu_instDecode_419513_422314), .in2(out_const_22));
  ui_lshift_expr_FU #(.BITSIZE_in1(21), .BITSIZE_in2(4), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_420069 (.out1(out_ui_lshift_expr_FU_32_0_32_100_i0_fu_instDecode_419513_420069), .in1(out_ui_bit_ior_expr_FU_32_0_32_92_i0_fu_instDecode_419513_420072), .in2(out_const_22));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(32), .BITSIZE_out1(21)) fu_instDecode_419513_420072 (.out1(out_ui_bit_ior_expr_FU_32_0_32_92_i0_fu_instDecode_419513_420072), .in1(out_ui_bit_ior_expr_FU_0_32_32_86_i0_fu_instDecode_419513_420076), .in2(out_const_44));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(5), .BITSIZE_out1(20)) fu_instDecode_419513_420076 (.out1(out_ui_bit_ior_expr_FU_0_32_32_86_i0_fu_instDecode_419513_420076), .in1(out_ui_bit_ior_expr_FU_0_32_32_87_i0_fu_instDecode_419513_420080), .in2(out_ui_bit_and_expr_FU_8_0_8_76_i1_fu_instDecode_419513_420060));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(11), .BITSIZE_out1(20)) fu_instDecode_419513_420080 (.out1(out_ui_bit_ior_expr_FU_0_32_32_87_i0_fu_instDecode_419513_420080), .in1(out_ui_bit_ior_expr_FU_0_32_32_88_i0_fu_instDecode_419513_420084), .in2(out_ui_bit_and_expr_FU_16_0_16_68_i0_fu_instDecode_419513_420058));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(12), .BITSIZE_out1(20)) fu_instDecode_419513_420084 (.out1(out_ui_bit_ior_expr_FU_0_32_32_88_i0_fu_instDecode_419513_420084), .in1(out_ui_bit_and_expr_FU_32_0_32_72_i0_fu_instDecode_419513_420088), .in2(out_ui_bit_and_expr_FU_16_0_16_67_i0_fu_instDecode_419513_420046));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(20), .BITSIZE_out1(20)) fu_instDecode_419513_420088 (.out1(out_ui_bit_and_expr_FU_32_0_32_72_i0_fu_instDecode_419513_420088), .in1(in_port_P1), .in2(out_const_42));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(3), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420140 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_102_i0_fu_instDecode_419513_420140), .in1(out_conv_in_port_P0_32_17), .in2(out_const_5));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(4), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420142 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_103_i0_fu_instDecode_419513_420142), .in1(out_conv_in_port_P0_32_17), .in2(out_const_6));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(4), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420144 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_104_i0_fu_instDecode_419513_420144), .in1(out_conv_in_port_P0_32_17), .in2(out_const_25));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420146 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_105_i0_fu_instDecode_419513_420146), .in1(out_conv_in_port_P0_32_17), .in2(out_const_7));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420148 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_106_i0_fu_instDecode_419513_420148), .in1(out_conv_in_port_P0_32_17), .in2(out_const_20));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420150 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_107_i0_fu_instDecode_419513_420150), .in1(out_conv_in_port_P0_32_17), .in2(out_const_26));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420152 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_108_i0_fu_instDecode_419513_420152), .in1(out_conv_in_port_P0_32_17), .in2(out_const_30));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420154 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_109_i0_fu_instDecode_419513_420154), .in1(out_conv_in_port_P0_32_17), .in2(out_const_31));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420156 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_110_i0_fu_instDecode_419513_420156), .in1(out_conv_in_port_P0_32_17), .in2(out_const_34));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420158 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_111_i0_fu_instDecode_419513_420158), .in1(out_conv_in_port_P0_32_17), .in2(out_const_36));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(6), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420160 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_112_i0_fu_instDecode_419513_420160), .in1(out_conv_in_port_P0_32_17), .in2(out_const_8));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(6), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_instDecode_419513_420162 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_113_i0_fu_instDecode_419513_420162), .in1(out_conv_in_port_P0_32_17), .in2(out_const_15));
  IUdata_converter_FU #(.BITSIZE_in1(13), .BITSIZE_out1(32)) fu_instDecode_419513_422254 (.out1(out_IUdata_converter_FU_20_i0_fu_instDecode_419513_422254), .in1(out_rshift_expr_FU_32_0_32_61_i0_fu_instDecode_419513_419898));
  IUdata_converter_FU #(.BITSIZE_in1(21), .BITSIZE_out1(32)) fu_instDecode_419513_422257 (.out1(out_IUdata_converter_FU_18_i0_fu_instDecode_419513_422257), .in1(out_rshift_expr_FU_32_0_32_63_i0_fu_instDecode_419513_420063));
  IUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(32)) fu_instDecode_419513_422260 (.out1(out_IUdata_converter_FU_16_i0_fu_instDecode_419513_422260), .in1(out_rshift_expr_FU_32_0_32_59_i1_fu_instDecode_419513_420004));
  IUdata_converter_FU #(.BITSIZE_in1(12), .BITSIZE_out1(32)) fu_instDecode_419513_422263 (.out1(out_IUdata_converter_FU_22_i0_fu_instDecode_419513_422263), .in1(out_rshift_expr_FU_32_0_32_62_i0_fu_instDecode_419513_419967));
  IUdata_converter_FU #(.BITSIZE_in1(31), .BITSIZE_out1(32)) fu_instDecode_419513_422266 (.out1(out_IUdata_converter_FU_14_i0_fu_instDecode_419513_422266), .in1(out_rshift_expr_FU_32_0_32_60_i0_fu_instDecode_419513_419882));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(31)) fu_instDecode_419513_422305 (.out1(out_UIdata_converter_FU_3_i0_fu_instDecode_419513_422305), .in1(in_port_P1));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_instDecode_419513_422308 (.out1(out_UIdata_converter_FU_13_i0_fu_instDecode_419513_422308), .in1(out_ui_lshift_expr_FU_32_0_32_95_i0_fu_instDecode_419513_419881));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_instDecode_419513_422311 (.out1(out_UIdata_converter_FU_19_i0_fu_instDecode_419513_422311), .in1(out_ui_lshift_expr_FU_32_0_32_96_i0_fu_instDecode_419513_419897));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_instDecode_419513_422314 (.out1(out_UIdata_converter_FU_17_i0_fu_instDecode_419513_422314), .in1(out_ui_lshift_expr_FU_32_0_32_100_i0_fu_instDecode_419513_420069));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_instDecode_419513_422317 (.out1(out_UIdata_converter_FU_15_i0_fu_instDecode_419513_422317), .in1(out_ui_lshift_expr_FU_32_0_32_99_i0_fu_instDecode_419513_420010));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_instDecode_419513_422320 (.out1(out_UIdata_converter_FU_21_i0_fu_instDecode_419513_422320), .in1(out_ui_lshift_expr_FU_32_0_32_98_i0_fu_instDecode_419513_419972));
  IUdata_converter_FU #(.BITSIZE_in1(6), .BITSIZE_out1(32)) fu_instDecode_419513_422322 (.out1(out_IUdata_converter_FU_23_i0_fu_instDecode_419513_422322), .in1(out_rshift_expr_FU_32_0_32_59_i0_fu_instDecode_419513_419831));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_instDecode_419513_422722 (.out1(out_lut_expr_FU_12_i0_fu_instDecode_419513_422722), .in1(out_const_9), .in2(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in3(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in4(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in5(out_lut_expr_FU_11_i0_fu_instDecode_419513_423503), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422814 (.out1(out_UUdata_converter_FU_37_i0_fu_instDecode_419513_422814), .in1(out_lut_expr_FU_36_i0_fu_instDecode_419513_423223));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422816 (.out1(out_UUdata_converter_FU_42_i0_fu_instDecode_419513_422816), .in1(out_lut_expr_FU_41_i0_fu_instDecode_419513_423216));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422818 (.out1(out_UUdata_converter_FU_40_i0_fu_instDecode_419513_422818), .in1(out_lut_expr_FU_39_i0_fu_instDecode_419513_423209));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_422828 (.out1(out_lut_expr_FU_43_i0_fu_instDecode_419513_422828), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446), .in3(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_422838 (.out1(out_lut_expr_FU_28_i0_fu_instDecode_419513_422838), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446), .in3(out_lut_expr_FU_27_i0_fu_instDecode_419513_423513), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_instDecode_419513_422842 (.out1(out_lut_expr_FU_31_i0_fu_instDecode_419513_422842), .in1(out_const_6), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446), .in3(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(12), .BITSIZE_out1(1)) fu_instDecode_419513_422894 (.out1(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in1(out_const_11), .in2(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in3(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in4(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in5(out_lut_expr_FU_29_i0_fu_instDecode_419513_423517), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(31), .BITSIZE_in3(12), .BITSIZE_out1(31)) fu_instDecode_419513_422901 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i0_fu_instDecode_419513_422901), .in1(out_lut_expr_FU_31_i0_fu_instDecode_419513_422842), .in2(out_ui_rshift_expr_FU_32_0_32_121_i0_fu_instDecode_419513_423018), .in3(out_ui_rshift_expr_FU_16_0_16_114_i0_fu_instDecode_419513_423022));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(31), .BITSIZE_in3(31), .BITSIZE_out1(31)) fu_instDecode_419513_422916 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i1_fu_instDecode_419513_422916), .in1(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in2(out_ui_rshift_expr_FU_32_0_32_121_i1_fu_instDecode_419513_423030), .in3(out_ui_rshift_expr_FU_32_0_32_121_i2_fu_instDecode_419513_423033));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_instDecode_419513_422924 (.out1(out_lut_expr_FU_32_i0_fu_instDecode_419513_422924), .in1(out_const_29), .in2(out_lut_expr_FU_27_i0_fu_instDecode_419513_423513), .in3(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(19), .BITSIZE_in3(31), .BITSIZE_out1(31)) fu_instDecode_419513_422931 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i2_fu_instDecode_419513_422931), .in1(out_lut_expr_FU_28_i0_fu_instDecode_419513_422838), .in2(out_ui_rshift_expr_FU_32_0_32_121_i3_fu_instDecode_419513_423040), .in3(out_ui_rshift_expr_FU_32_0_32_121_i4_fu_instDecode_419513_423044));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_instDecode_419513_422939 (.out1(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in1(out_const_6), .in2(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in3(out_lut_expr_FU_34_i0_fu_instDecode_419513_423525), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(11), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_instDecode_419513_422940 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i3_fu_instDecode_419513_422940), .in1(out_lut_expr_FU_43_i0_fu_instDecode_419513_422828), .in2(out_ui_bit_ior_expr_FU_0_16_16_80_i0_fu_instDecode_419513_419982), .in3(out_IUdata_converter_FU_16_i0_fu_instDecode_419513_422260));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_instDecode_419513_422948 (.out1(out_lut_expr_FU_45_i0_fu_instDecode_419513_422948), .in1(out_const_32), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446), .in3(out_lut_expr_FU_12_i0_fu_instDecode_419513_422722), .in4(out_lut_expr_FU_44_i0_fu_instDecode_419513_423539), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_instDecode_419513_422949 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i4_fu_instDecode_419513_422949), .in1(out_lut_expr_FU_12_i0_fu_instDecode_419513_422722), .in2(out_IUdata_converter_FU_14_i0_fu_instDecode_419513_422266), .in3(out_IUdata_converter_FU_22_i0_fu_instDecode_419513_422263));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(11), .BITSIZE_out1(32)) fu_instDecode_419513_422953 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i5_fu_instDecode_419513_422953), .in1(out_lut_expr_FU_45_i0_fu_instDecode_419513_422948), .in2(out_ui_cond_expr_FU_32_32_32_32_94_i4_fu_instDecode_419513_422949), .in3(out_ui_bit_and_expr_FU_16_0_16_65_i0_fu_instDecode_419513_419978));
  lut_expr_FU #(.BITSIZE_in1(12), .BITSIZE_out1(1)) fu_instDecode_419513_422969 (.out1(out_lut_expr_FU_47_i0_fu_instDecode_419513_422969), .in1(out_const_16), .in2(out_lut_expr_FU_25_i0_fu_instDecode_419513_423507), .in3(out_lut_expr_FU_29_i0_fu_instDecode_419513_423517), .in4(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in5(out_lut_expr_FU_46_i0_fu_instDecode_419513_423544), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_instDecode_419513_422976 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i6_fu_instDecode_419513_422976), .in1(out_lut_expr_FU_32_i0_fu_instDecode_419513_422924), .in2(out_ui_lshift_expr_FU_32_0_32_101_i2_fu_instDecode_419513_423048), .in3(out_ui_cond_expr_FU_32_32_32_32_94_i5_fu_instDecode_419513_422953));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_instDecode_419513_422983 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i7_fu_instDecode_419513_422983), .in1(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in2(out_ui_cond_expr_FU_32_32_32_32_94_i3_fu_instDecode_419513_422940), .in3(out_ui_cond_expr_FU_32_32_32_32_94_i5_fu_instDecode_419513_422953));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422987 (.out1(out_UUdata_converter_FU_57_i0_fu_instDecode_419513_422987), .in1(out_lut_expr_FU_56_i0_fu_instDecode_419513_423311));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422989 (.out1(out_UUdata_converter_FU_55_i0_fu_instDecode_419513_422989), .in1(out_lut_expr_FU_54_i0_fu_instDecode_419513_423304));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_instDecode_419513_422991 (.out1(out_ui_cond_expr_FU_32_32_32_32_94_i8_fu_instDecode_419513_422991), .in1(out_lut_expr_FU_47_i0_fu_instDecode_419513_422969), .in2(out_ui_cond_expr_FU_32_32_32_32_94_i6_fu_instDecode_419513_422976), .in3(out_ui_cond_expr_FU_32_32_32_32_94_i7_fu_instDecode_419513_422983));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_422993 (.out1(out_UUdata_converter_FU_53_i0_fu_instDecode_419513_422993), .in1(out_lut_expr_FU_52_i0_fu_instDecode_419513_423290));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_instDecode_419513_423018 (.out1(out_ui_rshift_expr_FU_32_0_32_121_i0_fu_instDecode_419513_423018), .in1(out_IUdata_converter_FU_20_i0_fu_instDecode_419513_422254), .in2(out_const_3));
  ui_rshift_expr_FU #(.BITSIZE_in1(13), .BITSIZE_in2(1), .BITSIZE_out1(12), .PRECISION(32)) fu_instDecode_419513_423022 (.out1(out_ui_rshift_expr_FU_16_0_16_114_i0_fu_instDecode_419513_423022), .in1(out_ui_bit_ior_expr_FU_0_16_16_79_i0_fu_instDecode_419513_419924), .in2(out_const_3));
  ui_lshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(1), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_423027 (.out1(out_ui_lshift_expr_FU_32_0_32_101_i0_fu_instDecode_419513_423027), .in1(out_ui_cond_expr_FU_32_32_32_32_94_i0_fu_instDecode_419513_422901), .in2(out_const_3));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_instDecode_419513_423030 (.out1(out_ui_rshift_expr_FU_32_0_32_121_i1_fu_instDecode_419513_423030), .in1(out_ui_lshift_expr_FU_32_0_32_101_i0_fu_instDecode_419513_423027), .in2(out_const_3));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_instDecode_419513_423033 (.out1(out_ui_rshift_expr_FU_32_0_32_121_i2_fu_instDecode_419513_423033), .in1(out_IUdata_converter_FU_18_i0_fu_instDecode_419513_422257), .in2(out_const_3));
  ui_lshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(1), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_423037 (.out1(out_ui_lshift_expr_FU_32_0_32_101_i1_fu_instDecode_419513_423037), .in1(out_ui_cond_expr_FU_32_32_32_32_94_i1_fu_instDecode_419513_422916), .in2(out_const_3));
  ui_rshift_expr_FU #(.BITSIZE_in1(20), .BITSIZE_in2(1), .BITSIZE_out1(19), .PRECISION(32)) fu_instDecode_419513_423040 (.out1(out_ui_rshift_expr_FU_32_0_32_121_i3_fu_instDecode_419513_423040), .in1(out_ui_bit_ior_expr_FU_0_32_32_83_i0_fu_instDecode_419513_420031), .in2(out_const_3));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_instDecode_419513_423044 (.out1(out_ui_rshift_expr_FU_32_0_32_121_i4_fu_instDecode_419513_423044), .in1(out_ui_lshift_expr_FU_32_0_32_101_i1_fu_instDecode_419513_423037), .in2(out_const_3));
  ui_lshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(1), .BITSIZE_out1(32), .PRECISION(32)) fu_instDecode_419513_423048 (.out1(out_ui_lshift_expr_FU_32_0_32_101_i2_fu_instDecode_419513_423048), .in1(out_ui_cond_expr_FU_32_32_32_32_94_i2_fu_instDecode_419513_422931), .in2(out_const_3));
  lut_expr_FU #(.BITSIZE_in1(7), .BITSIZE_out1(1)) fu_instDecode_419513_423209 (.out1(out_lut_expr_FU_39_i0_fu_instDecode_419513_423209), .in1(out_const_21), .in2(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in3(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in4(out_lut_expr_FU_35_i0_fu_instDecode_419513_423529), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_423216 (.out1(out_lut_expr_FU_41_i0_fu_instDecode_419513_423216), .in1(out_const_5), .in2(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in3(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in4(out_lut_expr_FU_35_i0_fu_instDecode_419513_423529), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_423223 (.out1(out_lut_expr_FU_36_i0_fu_instDecode_419513_423223), .in1(out_const_5), .in2(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in3(out_lut_expr_FU_35_i0_fu_instDecode_419513_423529), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(2), .BITSIZE_out1(1)) fu_instDecode_419513_423290 (.out1(out_lut_expr_FU_52_i0_fu_instDecode_419513_423290), .in1(out_const_4), .in2(out_lut_expr_FU_48_i0_fu_instDecode_419513_423550), .in3(out_lut_expr_FU_51_i0_fu_instDecode_419513_423559), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(5), .BITSIZE_out1(1)) fu_instDecode_419513_423304 (.out1(out_lut_expr_FU_54_i0_fu_instDecode_419513_423304), .in1(out_const_18), .in2(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in3(out_lut_expr_FU_46_i0_fu_instDecode_419513_423544), .in4(out_lut_expr_FU_48_i0_fu_instDecode_419513_423550), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(2), .BITSIZE_out1(1)) fu_instDecode_419513_423311 (.out1(out_lut_expr_FU_56_i0_fu_instDecode_419513_423311), .in1(out_const_4), .in2(out_lut_expr_FU_46_i0_fu_instDecode_419513_423544), .in3(out_lut_expr_FU_48_i0_fu_instDecode_419513_423550), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2)) fu_instDecode_419513_423315 (.out1(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in1(in_port_P1), .in2(out_const_23));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3)) fu_instDecode_419513_423319 (.out1(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in1(in_port_P1), .in2(out_const_5));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3)) fu_instDecode_419513_423323 (.out1(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in1(in_port_P1), .in2(out_const_19));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(3)) fu_instDecode_419513_423327 (.out1(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in1(in_port_P1), .in2(out_const_24));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1)) fu_instDecode_419513_423331 (.out1(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in1(in_port_P1), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1)) fu_instDecode_419513_423335 (.out1(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in1(in_port_P1), .in2(out_const_3));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2)) fu_instDecode_419513_423339 (.out1(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in1(in_port_P1), .in2(out_const_4));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(5)) fu_instDecode_419513_423446 (.out1(out_ui_extract_bit_expr_FU_24_i0_fu_instDecode_419513_423446), .in1(in_port_P1), .in2(out_const_36));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_instDecode_419513_423503 (.out1(out_lut_expr_FU_11_i0_fu_instDecode_419513_423503), .in1(out_const_38), .in2(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in3(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in4(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_instDecode_419513_423507 (.out1(out_lut_expr_FU_25_i0_fu_instDecode_419513_423507), .in1(out_const_9), .in2(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in3(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in4(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_instDecode_419513_423510 (.out1(out_lut_expr_FU_26_i0_fu_instDecode_419513_423510), .in1(out_const_6), .in2(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in3(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(14), .BITSIZE_out1(1)) fu_instDecode_419513_423513 (.out1(out_lut_expr_FU_27_i0_fu_instDecode_419513_423513), .in1(out_const_13), .in2(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in3(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in4(out_lut_expr_FU_25_i0_fu_instDecode_419513_423507), .in5(out_lut_expr_FU_26_i0_fu_instDecode_419513_423510), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(13), .BITSIZE_out1(1)) fu_instDecode_419513_423517 (.out1(out_lut_expr_FU_29_i0_fu_instDecode_419513_423517), .in1(out_const_12), .in2(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in3(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in4(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in5(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_instDecode_419513_423521 (.out1(out_lut_expr_FU_33_i0_fu_instDecode_419513_423521), .in1(out_const_6), .in2(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in3(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in4(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(9), .BITSIZE_out1(1)) fu_instDecode_419513_423525 (.out1(out_lut_expr_FU_34_i0_fu_instDecode_419513_423525), .in1(out_const_10), .in2(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in3(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in4(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in5(out_lut_expr_FU_33_i0_fu_instDecode_419513_423521), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_423529 (.out1(out_lut_expr_FU_35_i0_fu_instDecode_419513_423529), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in3(out_lut_expr_FU_34_i0_fu_instDecode_419513_423525), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_423539 (.out1(out_lut_expr_FU_44_i0_fu_instDecode_419513_423539), .in1(out_const_3), .in2(out_lut_expr_FU_12_i0_fu_instDecode_419513_422722), .in3(out_lut_expr_FU_27_i0_fu_instDecode_419513_423513), .in4(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in5(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_423544 (.out1(out_lut_expr_FU_46_i0_fu_instDecode_419513_423544), .in1(out_const_3), .in2(out_lut_expr_FU_27_i0_fu_instDecode_419513_423513), .in3(out_lut_expr_FU_30_i0_fu_instDecode_419513_422894), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(16), .BITSIZE_out1(1)) fu_instDecode_419513_423550 (.out1(out_lut_expr_FU_48_i0_fu_instDecode_419513_423550), .in1(out_const_35), .in2(out_lut_expr_FU_25_i0_fu_instDecode_419513_423507), .in3(out_lut_expr_FU_29_i0_fu_instDecode_419513_423517), .in4(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in5(out_lut_expr_FU_46_i0_fu_instDecode_419513_423544), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_instDecode_419513_423553 (.out1(out_lut_expr_FU_49_i0_fu_instDecode_419513_423553), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_4_i0_fu_instDecode_419513_423315), .in3(out_ui_extract_bit_expr_FU_5_i0_fu_instDecode_419513_423319), .in4(out_ui_extract_bit_expr_FU_6_i0_fu_instDecode_419513_423323), .in5(out_ui_extract_bit_expr_FU_7_i0_fu_instDecode_419513_423327), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(12), .BITSIZE_out1(1)) fu_instDecode_419513_423556 (.out1(out_lut_expr_FU_50_i0_fu_instDecode_419513_423556), .in1(out_const_11), .in2(out_ui_extract_bit_expr_FU_8_i0_fu_instDecode_419513_423331), .in3(out_ui_extract_bit_expr_FU_9_i0_fu_instDecode_419513_423335), .in4(out_ui_extract_bit_expr_FU_10_i0_fu_instDecode_419513_423339), .in5(out_lut_expr_FU_49_i0_fu_instDecode_419513_423553), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_instDecode_419513_423559 (.out1(out_lut_expr_FU_51_i0_fu_instDecode_419513_423559), .in1(out_const_3), .in2(out_lut_expr_FU_12_i0_fu_instDecode_419513_422722), .in3(out_lut_expr_FU_38_i0_fu_instDecode_419513_422939), .in4(out_lut_expr_FU_35_i0_fu_instDecode_419513_423529), .in5(out_lut_expr_FU_50_i0_fu_instDecode_419513_423556), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  join_signal #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_out1(64)) join_signalbus_mergerproxy_in1_4201700_0 (.out1(sig_in_bus_mergerproxy_in1_4201700_0), .in1(sig_in_vector_bus_mergerproxy_in1_4201700_0));
  join_signal #(.BITSIZE_in1(17), .PORTSIZE_in1(2), .BITSIZE_out1(34)) join_signalbus_mergerproxy_in2_4201701_0 (.out1(sig_in_bus_mergerproxy_in2_4201701_0), .in1(sig_in_vector_bus_mergerproxy_in2_4201701_0));
  join_signal #(.BITSIZE_in1(6), .PORTSIZE_in1(2), .BITSIZE_out1(12)) join_signalbus_mergerproxy_in3_4201702_0 (.out1(sig_in_bus_mergerproxy_in3_4201702_0), .in1(sig_in_vector_bus_mergerproxy_in3_4201702_0));
  join_signal #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) join_signalbus_mergerproxy_sel_LOAD_4201703_0 (.out1(sig_in_bus_mergerproxy_sel_LOAD_4201703_0), .in1(sig_in_vector_bus_mergerproxy_sel_LOAD_4201703_0));
  join_signal #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) join_signalbus_mergerproxy_sel_STORE_4201704_0 (.out1(sig_in_bus_mergerproxy_sel_STORE_4201704_0), .in1(sig_in_vector_bus_mergerproxy_sel_STORE_4201704_0));
  register_STD #(.BITSIZE_in1(5), .BITSIZE_out1(5)) reg_0 (.out1(out_reg_0_reg_0), .clock(clock), .reset(reset), .in1(out_ui_bit_and_expr_FU_8_0_8_73_i2_fu_instDecode_419513_419830), .wenable(wrenable_reg_0));
  register_SE #(.BITSIZE_in1(3), .BITSIZE_out1(3)) reg_1 (.out1(out_reg_1_reg_1), .clock(clock), .reset(reset), .in1(out_ui_bit_and_expr_FU_8_0_8_74_i0_fu_instDecode_419513_419833), .wenable(wrenable_reg_1));
  register_STD #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_10 (.out1(out_reg_10_reg_10), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_103_i0_fu_instDecode_419513_420142), .wenable(wrenable_reg_10));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_11 (.out1(out_reg_11_reg_11), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_104_i0_fu_instDecode_419513_420144), .wenable(wrenable_reg_11));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_12 (.out1(out_reg_12_reg_12), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_105_i0_fu_instDecode_419513_420146), .wenable(wrenable_reg_12));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_13 (.out1(out_reg_13_reg_13), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_106_i0_fu_instDecode_419513_420148), .wenable(wrenable_reg_13));
  register_STD #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_14 (.out1(out_reg_14_reg_14), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_107_i0_fu_instDecode_419513_420150), .wenable(wrenable_reg_14));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_15 (.out1(out_reg_15_reg_15), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_108_i0_fu_instDecode_419513_420152), .wenable(wrenable_reg_15));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_16 (.out1(out_reg_16_reg_16), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_109_i0_fu_instDecode_419513_420154), .wenable(wrenable_reg_16));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_17 (.out1(out_reg_17_reg_17), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_110_i0_fu_instDecode_419513_420156), .wenable(wrenable_reg_17));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_18 (.out1(out_reg_18_reg_18), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_111_i0_fu_instDecode_419513_420158), .wenable(wrenable_reg_18));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_19 (.out1(out_reg_19_reg_19), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_112_i0_fu_instDecode_419513_420160), .wenable(wrenable_reg_19));
  register_SE #(.BITSIZE_in1(7), .BITSIZE_out1(7)) reg_2 (.out1(out_reg_2_reg_2), .clock(clock), .reset(reset), .in1(out_ui_bit_and_expr_FU_8_0_8_75_i0_fu_instDecode_419513_419834), .wenable(wrenable_reg_2));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_20 (.out1(out_reg_20_reg_20), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_113_i0_fu_instDecode_419513_420162), .wenable(wrenable_reg_20));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_21 (.out1(out_reg_21_reg_21), .clock(clock), .reset(reset), .in1(out_IUdata_converter_FU_23_i0_fu_instDecode_419513_422322), .wenable(wrenable_reg_21));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_3 (.out1(out_reg_3_reg_3), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_57_i0_fu_instDecode_419513_422987), .wenable(wrenable_reg_3));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_4 (.out1(out_reg_4_reg_4), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_55_i0_fu_instDecode_419513_422989), .wenable(wrenable_reg_4));
  register_STD #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_5 (.out1(out_reg_5_reg_5), .clock(clock), .reset(reset), .in1(out_ui_cond_expr_FU_32_32_32_32_94_i8_fu_instDecode_419513_422991), .wenable(wrenable_reg_5));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_6 (.out1(out_reg_6_reg_6), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_53_i0_fu_instDecode_419513_422993), .wenable(wrenable_reg_6));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_7 (.out1(out_reg_7_reg_7), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_37_i0_fu_instDecode_419513_422814), .wenable(wrenable_reg_7));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_8 (.out1(out_reg_8_reg_8), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_42_i0_fu_instDecode_419513_422816), .wenable(wrenable_reg_8));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_9 (.out1(out_reg_9_reg_9), .clock(clock), .reset(reset), .in1(out_UUdata_converter_FU_40_i0_fu_instDecode_419513_422818), .wenable(wrenable_reg_9));
  split_signal #(.BITSIZE_in1(64), .BITSIZE_out1(32), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in1_4201700_ (.out1(proxy_in1_420170), .in1(sig_out_bus_mergerproxy_in1_4201700_));
  split_signal #(.BITSIZE_in1(34), .BITSIZE_out1(17), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in2_4201701_ (.out1(proxy_in2_420170), .in1(sig_out_bus_mergerproxy_in2_4201701_));
  split_signal #(.BITSIZE_in1(12), .BITSIZE_out1(6), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in3_4201702_ (.out1(proxy_in3_420170), .in1(sig_out_bus_mergerproxy_in3_4201702_));
  split_signal #(.BITSIZE_in1(2), .BITSIZE_out1(1), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_sel_LOAD_4201703_ (.out1(proxy_sel_LOAD_420170), .in1(sig_out_bus_mergerproxy_sel_LOAD_4201703_));
  split_signal #(.BITSIZE_in1(2), .BITSIZE_out1(1), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_sel_STORE_4201704_ (.out1(proxy_sel_STORE_420170), .in1(sig_out_bus_mergerproxy_sel_STORE_4201704_));

endmodule

// FSM based controller description for instDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller_instDecode(done_port, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1, selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2, selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0, selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1, selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2, selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0, selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1, selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2, selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3, selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0, selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2, selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3, selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0, selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1, selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0, fuselector_PROXY_CTRLN_0_i0_LOAD, fuselector_PROXY_CTRLN_0_i0_STORE, fuselector_PROXY_CTRLN_0_i1_LOAD, fuselector_PROXY_CTRLN_0_i1_STORE, wrenable_reg_0, wrenable_reg_1, wrenable_reg_10, wrenable_reg_11, wrenable_reg_12, wrenable_reg_13, wrenable_reg_14, wrenable_reg_15, wrenable_reg_16, wrenable_reg_17, wrenable_reg_18, wrenable_reg_19, wrenable_reg_2, wrenable_reg_20, wrenable_reg_21, wrenable_reg_3, wrenable_reg_4, wrenable_reg_5, wrenable_reg_6, wrenable_reg_7, wrenable_reg_8, wrenable_reg_9, clock, reset, start_port);
  // IN
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0;
  output selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1;
  output selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2;
  output selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0;
  output selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1;
  output selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0;
  output selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1;
  output selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2;
  output selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0;
  output selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1;
  output selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0;
  output selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0;
  output selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1;
  output selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0;
  output fuselector_PROXY_CTRLN_0_i0_LOAD;
  output fuselector_PROXY_CTRLN_0_i0_STORE;
  output fuselector_PROXY_CTRLN_0_i1_LOAD;
  output fuselector_PROXY_CTRLN_0_i1_STORE;
  output wrenable_reg_0;
  output wrenable_reg_1;
  output wrenable_reg_10;
  output wrenable_reg_11;
  output wrenable_reg_12;
  output wrenable_reg_13;
  output wrenable_reg_14;
  output wrenable_reg_15;
  output wrenable_reg_16;
  output wrenable_reg_17;
  output wrenable_reg_18;
  output wrenable_reg_19;
  output wrenable_reg_2;
  output wrenable_reg_20;
  output wrenable_reg_21;
  output wrenable_reg_3;
  output wrenable_reg_4;
  output wrenable_reg_5;
  output wrenable_reg_6;
  output wrenable_reg_7;
  output wrenable_reg_8;
  output wrenable_reg_9;
  parameter [2:0] S_0 = 3'd0,
    S_1 = 3'd1,
    S_2 = 3'd2,
    S_3 = 3'd3,
    S_4 = 3'd4,
    S_5 = 3'd5,
    S_6 = 3'd6;
  reg [2:0] _present_state, _next_state;
  reg done_port;
  reg selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0;
  reg selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1;
  reg selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2;
  reg selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0;
  reg selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1;
  reg selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0;
  reg selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1;
  reg selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2;
  reg selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0;
  reg selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1;
  reg selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0;
  reg selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0;
  reg selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1;
  reg selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0;
  reg fuselector_PROXY_CTRLN_0_i0_LOAD;
  reg fuselector_PROXY_CTRLN_0_i0_STORE;
  reg fuselector_PROXY_CTRLN_0_i1_LOAD;
  reg fuselector_PROXY_CTRLN_0_i1_STORE;
  reg wrenable_reg_0;
  reg wrenable_reg_1;
  reg wrenable_reg_10;
  reg wrenable_reg_11;
  reg wrenable_reg_12;
  reg wrenable_reg_13;
  reg wrenable_reg_14;
  reg wrenable_reg_15;
  reg wrenable_reg_16;
  reg wrenable_reg_17;
  reg wrenable_reg_18;
  reg wrenable_reg_19;
  reg wrenable_reg_2;
  reg wrenable_reg_20;
  reg wrenable_reg_21;
  reg wrenable_reg_3;
  reg wrenable_reg_4;
  reg wrenable_reg_5;
  reg wrenable_reg_6;
  reg wrenable_reg_7;
  reg wrenable_reg_8;
  reg wrenable_reg_9;
  
  always @(posedge clock)
    if (reset == 1'b0) _present_state <= S_0;
    else _present_state <= _next_state;
  
  always @(*)
  begin
    done_port = 1'b0;
    selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0 = 1'b0;
    selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1 = 1'b0;
    selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2 = 1'b0;
    selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0 = 1'b0;
    selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1 = 1'b0;
    selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0 = 1'b0;
    selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1 = 1'b0;
    selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2 = 1'b0;
    selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0 = 1'b0;
    selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1 = 1'b0;
    selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0 = 1'b0;
    selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0 = 1'b0;
    selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'b0;
    selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'b0;
    fuselector_PROXY_CTRLN_0_i0_LOAD = 1'b0;
    fuselector_PROXY_CTRLN_0_i0_STORE = 1'b0;
    fuselector_PROXY_CTRLN_0_i1_LOAD = 1'b0;
    fuselector_PROXY_CTRLN_0_i1_STORE = 1'b0;
    wrenable_reg_0 = 1'b0;
    wrenable_reg_1 = 1'b0;
    wrenable_reg_10 = 1'b0;
    wrenable_reg_11 = 1'b0;
    wrenable_reg_12 = 1'b0;
    wrenable_reg_13 = 1'b0;
    wrenable_reg_14 = 1'b0;
    wrenable_reg_15 = 1'b0;
    wrenable_reg_16 = 1'b0;
    wrenable_reg_17 = 1'b0;
    wrenable_reg_18 = 1'b0;
    wrenable_reg_19 = 1'b0;
    wrenable_reg_2 = 1'b0;
    wrenable_reg_20 = 1'b0;
    wrenable_reg_21 = 1'b0;
    wrenable_reg_3 = 1'b0;
    wrenable_reg_4 = 1'b0;
    wrenable_reg_5 = 1'b0;
    wrenable_reg_6 = 1'b0;
    wrenable_reg_7 = 1'b0;
    wrenable_reg_8 = 1'b0;
    wrenable_reg_9 = 1'b0;
    case (_present_state)
      S_0 :
        if(start_port == 1'b1)
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0 = 1'b1;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_10 = 1'b1;
          wrenable_reg_11 = 1'b1;
          wrenable_reg_12 = 1'b1;
          wrenable_reg_13 = 1'b1;
          wrenable_reg_14 = 1'b1;
          wrenable_reg_15 = 1'b1;
          wrenable_reg_16 = 1'b1;
          wrenable_reg_17 = 1'b1;
          wrenable_reg_18 = 1'b1;
          wrenable_reg_19 = 1'b1;
          wrenable_reg_2 = 1'b1;
          wrenable_reg_20 = 1'b1;
          wrenable_reg_21 = 1'b1;
          wrenable_reg_3 = 1'b1;
          wrenable_reg_4 = 1'b1;
          wrenable_reg_5 = 1'b1;
          wrenable_reg_6 = 1'b1;
          wrenable_reg_7 = 1'b1;
          wrenable_reg_8 = 1'b1;
          wrenable_reg_9 = 1'b1;
          _next_state = S_1;
        end
        else
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1 = 1'bX;
          selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'bX;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_10 = 1'bX;
          wrenable_reg_11 = 1'bX;
          wrenable_reg_12 = 1'bX;
          wrenable_reg_13 = 1'bX;
          wrenable_reg_14 = 1'bX;
          wrenable_reg_15 = 1'bX;
          wrenable_reg_16 = 1'bX;
          wrenable_reg_17 = 1'bX;
          wrenable_reg_18 = 1'bX;
          wrenable_reg_19 = 1'bX;
          wrenable_reg_2 = 1'bX;
          wrenable_reg_20 = 1'bX;
          wrenable_reg_21 = 1'bX;
          wrenable_reg_3 = 1'bX;
          wrenable_reg_4 = 1'bX;
          wrenable_reg_5 = 1'bX;
          wrenable_reg_6 = 1'bX;
          wrenable_reg_7 = 1'bX;
          wrenable_reg_8 = 1'bX;
          wrenable_reg_9 = 1'bX;
          _next_state = S_0;
        end
      S_1 :
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          _next_state = S_2;
        end
      S_2 :
        begin
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          _next_state = S_3;
        end
      S_3 :
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2 = 1'b1;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0 = 1'b1;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          _next_state = S_4;
        end
      S_4 :
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0 = 1'b1;
          selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0 = 1'b1;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          _next_state = S_5;
        end
      S_5 :
        begin
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0 = 1'b1;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0 = 1'b1;
          selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'b1;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          fuselector_PROXY_CTRLN_0_i1_STORE = 1'b1;
          _next_state = S_6;
          done_port = 1'b1;
        end
      S_6 :
        begin
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1 = 1'b1;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0 = 1'b1;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'b1;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'b1;
          fuselector_PROXY_CTRLN_0_i0_STORE = 1'b1;
          _next_state = S_0;
        end
      default :
        begin
          _next_state = S_0;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0 = 1'bX;
          selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0 = 1'bX;
          selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1 = 1'bX;
          selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0 = 1'bX;
          selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0 = 1'bX;
          selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1 = 1'bX;
          selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_10 = 1'bX;
          wrenable_reg_11 = 1'bX;
          wrenable_reg_12 = 1'bX;
          wrenable_reg_13 = 1'bX;
          wrenable_reg_14 = 1'bX;
          wrenable_reg_15 = 1'bX;
          wrenable_reg_16 = 1'bX;
          wrenable_reg_17 = 1'bX;
          wrenable_reg_18 = 1'bX;
          wrenable_reg_19 = 1'bX;
          wrenable_reg_2 = 1'bX;
          wrenable_reg_20 = 1'bX;
          wrenable_reg_21 = 1'bX;
          wrenable_reg_3 = 1'bX;
          wrenable_reg_4 = 1'bX;
          wrenable_reg_5 = 1'bX;
          wrenable_reg_6 = 1'bX;
          wrenable_reg_7 = 1'bX;
          wrenable_reg_8 = 1'bX;
          wrenable_reg_9 = 1'bX;
        end
    endcase
  end
endmodule

// Top component for instDecode
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module instDecode(clock, reset, start_port, done_port, P0, P1, proxy_out1_420170, proxy_in1_420170, proxy_in2_420170, proxy_in3_420170, proxy_sel_LOAD_420170, proxy_sel_STORE_420170);
  // IN
  input clock;
  input reset;
  input start_port;
  input [31:0] P0;
  input [31:0] P1;
  input [63:0] proxy_out1_420170;
  // OUT
  output done_port;
  output [63:0] proxy_in1_420170;
  output [33:0] proxy_in2_420170;
  output [11:0] proxy_in3_420170;
  output [1:0] proxy_sel_LOAD_420170;
  output [1:0] proxy_sel_STORE_420170;
  // Component and signal declarations
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_PROXY_CTRLN_0_i0_LOAD;
  wire fuselector_PROXY_CTRLN_0_i0_STORE;
  wire fuselector_PROXY_CTRLN_0_i1_LOAD;
  wire fuselector_PROXY_CTRLN_0_i1_STORE;
  wire selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0;
  wire selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1;
  wire selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2;
  wire selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0;
  wire selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1;
  wire selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0;
  wire selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1;
  wire selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2;
  wire selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0;
  wire selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1;
  wire selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0;
  wire selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0;
  wire selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1;
  wire selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0;
  wire wrenable_reg_0;
  wire wrenable_reg_1;
  wire wrenable_reg_10;
  wire wrenable_reg_11;
  wire wrenable_reg_12;
  wire wrenable_reg_13;
  wire wrenable_reg_14;
  wire wrenable_reg_15;
  wire wrenable_reg_16;
  wire wrenable_reg_17;
  wire wrenable_reg_18;
  wire wrenable_reg_19;
  wire wrenable_reg_2;
  wire wrenable_reg_20;
  wire wrenable_reg_21;
  wire wrenable_reg_3;
  wire wrenable_reg_4;
  wire wrenable_reg_5;
  wire wrenable_reg_6;
  wire wrenable_reg_7;
  wire wrenable_reg_8;
  wire wrenable_reg_9;
  
  controller_instDecode Controller_i (.done_port(done_delayed_REG_signal_in), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2), .selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0), .selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2), .selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0), .selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1), .selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0(selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3), .selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3), .selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0), .selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1), .selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0(selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0), .fuselector_PROXY_CTRLN_0_i0_LOAD(fuselector_PROXY_CTRLN_0_i0_LOAD), .fuselector_PROXY_CTRLN_0_i0_STORE(fuselector_PROXY_CTRLN_0_i0_STORE), .fuselector_PROXY_CTRLN_0_i1_LOAD(fuselector_PROXY_CTRLN_0_i1_LOAD), .fuselector_PROXY_CTRLN_0_i1_STORE(fuselector_PROXY_CTRLN_0_i1_STORE), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_10(wrenable_reg_10), .wrenable_reg_11(wrenable_reg_11), .wrenable_reg_12(wrenable_reg_12), .wrenable_reg_13(wrenable_reg_13), .wrenable_reg_14(wrenable_reg_14), .wrenable_reg_15(wrenable_reg_15), .wrenable_reg_16(wrenable_reg_16), .wrenable_reg_17(wrenable_reg_17), .wrenable_reg_18(wrenable_reg_18), .wrenable_reg_19(wrenable_reg_19), .wrenable_reg_2(wrenable_reg_2), .wrenable_reg_20(wrenable_reg_20), .wrenable_reg_21(wrenable_reg_21), .wrenable_reg_3(wrenable_reg_3), .wrenable_reg_4(wrenable_reg_4), .wrenable_reg_5(wrenable_reg_5), .wrenable_reg_6(wrenable_reg_6), .wrenable_reg_7(wrenable_reg_7), .wrenable_reg_8(wrenable_reg_8), .wrenable_reg_9(wrenable_reg_9), .clock(clock), .reset(reset), .start_port(start_port));
  datapath_instDecode Datapath_i (.proxy_in1_420170(proxy_in1_420170), .proxy_in2_420170(proxy_in2_420170), .proxy_in3_420170(proxy_in3_420170), .proxy_sel_LOAD_420170(proxy_sel_LOAD_420170), .proxy_sel_STORE_420170(proxy_sel_STORE_420170), .clock(clock), .reset(reset), .in_port_P0(P0), .in_port_P1(P1), .proxy_out1_420170(proxy_out1_420170), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_0), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_1), .selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2(selector_MUX_10_PROXY_CTRLN_0_i1_0_0_2), .selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_0), .selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1(selector_MUX_10_PROXY_CTRLN_0_i1_0_1_1), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_0), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_1), .selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2(selector_MUX_11_PROXY_CTRLN_0_i1_1_0_2), .selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_0), .selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1(selector_MUX_11_PROXY_CTRLN_0_i1_1_1_1), .selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0(selector_MUX_12_PROXY_CTRLN_0_i1_2_0_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_1), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_2), .selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3(selector_MUX_6_PROXY_CTRLN_0_i0_0_0_3), .selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_0), .selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1(selector_MUX_6_PROXY_CTRLN_0_i0_0_1_1), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_0), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_1), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_2), .selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3(selector_MUX_7_PROXY_CTRLN_0_i0_1_0_3), .selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_0), .selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1(selector_MUX_7_PROXY_CTRLN_0_i0_1_1_1), .selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0(selector_MUX_8_PROXY_CTRLN_0_i0_2_0_0), .fuselector_PROXY_CTRLN_0_i0_LOAD(fuselector_PROXY_CTRLN_0_i0_LOAD), .fuselector_PROXY_CTRLN_0_i0_STORE(fuselector_PROXY_CTRLN_0_i0_STORE), .fuselector_PROXY_CTRLN_0_i1_LOAD(fuselector_PROXY_CTRLN_0_i1_LOAD), .fuselector_PROXY_CTRLN_0_i1_STORE(fuselector_PROXY_CTRLN_0_i1_STORE), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_10(wrenable_reg_10), .wrenable_reg_11(wrenable_reg_11), .wrenable_reg_12(wrenable_reg_12), .wrenable_reg_13(wrenable_reg_13), .wrenable_reg_14(wrenable_reg_14), .wrenable_reg_15(wrenable_reg_15), .wrenable_reg_16(wrenable_reg_16), .wrenable_reg_17(wrenable_reg_17), .wrenable_reg_18(wrenable_reg_18), .wrenable_reg_19(wrenable_reg_19), .wrenable_reg_2(wrenable_reg_2), .wrenable_reg_20(wrenable_reg_20), .wrenable_reg_21(wrenable_reg_21), .wrenable_reg_3(wrenable_reg_3), .wrenable_reg_4(wrenable_reg_4), .wrenable_reg_5(wrenable_reg_5), .wrenable_reg_6(wrenable_reg_6), .wrenable_reg_7(wrenable_reg_7), .wrenable_reg_8(wrenable_reg_8), .wrenable_reg_9(wrenable_reg_9));
  flipflop_AR #(.BITSIZE_in1(1), .BITSIZE_out1(1)) done_delayed_REG (.out1(done_delayed_REG_signal_out), .clock(clock), .reset(reset), .in1(done_delayed_REG_signal_in));
  // io-signal post fix
  assign done_port = done_delayed_REG_signal_out;

endmodule

// Datapath RTL description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module datapath_main(clock, reset, return_port, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, S_data_ram_size, Sin_Rdata_ram, Sin_DataRdy, Sout_Rdata_ram, Sout_DataRdy, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE, selector_IN_UNBOUNDED_main_419527_420296, selector_IN_UNBOUNDED_main_419527_420310, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1, selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0, selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0, selector_MUX_247_reg_0_0_0_0, selector_MUX_308_reg_64_0_0_0, selector_MUX_309_reg_65_0_0_0, selector_MUX_319_reg_74_0_0_0, selector_MUX_320_reg_75_0_0_0, selector_MUX_321_reg_76_0_0_0, selector_MUX_322_reg_77_0_0_0, selector_MUX_324_reg_79_0_0_0, selector_MUX_326_reg_80_0_0_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1, selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0, selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1, wrenable_reg_0, wrenable_reg_1, wrenable_reg_10, wrenable_reg_11, wrenable_reg_12, wrenable_reg_13, wrenable_reg_14, wrenable_reg_15, wrenable_reg_16, wrenable_reg_17, wrenable_reg_18, wrenable_reg_19, wrenable_reg_2, wrenable_reg_20, wrenable_reg_21, wrenable_reg_22, wrenable_reg_23, wrenable_reg_24, wrenable_reg_25, wrenable_reg_26, wrenable_reg_27, wrenable_reg_28, wrenable_reg_29, wrenable_reg_3, wrenable_reg_30, wrenable_reg_31, wrenable_reg_32, wrenable_reg_33, wrenable_reg_34, wrenable_reg_35, wrenable_reg_36, wrenable_reg_37, wrenable_reg_38, wrenable_reg_39, wrenable_reg_4, wrenable_reg_40, wrenable_reg_41, wrenable_reg_42, wrenable_reg_43, wrenable_reg_44, wrenable_reg_45, wrenable_reg_46, wrenable_reg_47, wrenable_reg_48, wrenable_reg_49, wrenable_reg_5, wrenable_reg_50, wrenable_reg_51, wrenable_reg_52, wrenable_reg_53, wrenable_reg_54, wrenable_reg_55, wrenable_reg_56, wrenable_reg_57, wrenable_reg_58, wrenable_reg_59, wrenable_reg_6, wrenable_reg_60, wrenable_reg_61, wrenable_reg_62, wrenable_reg_63, wrenable_reg_64, wrenable_reg_65, wrenable_reg_66, wrenable_reg_67, wrenable_reg_68, wrenable_reg_69, wrenable_reg_7, wrenable_reg_70, wrenable_reg_71, wrenable_reg_72, wrenable_reg_73, wrenable_reg_74, wrenable_reg_75, wrenable_reg_76, wrenable_reg_77, wrenable_reg_78, wrenable_reg_79, wrenable_reg_8, wrenable_reg_80, wrenable_reg_9, OUT_CONDITION_main_419527_420260, OUT_CONDITION_main_419527_420315, OUT_CONDITION_main_419527_421689, OUT_CONDITION_main_419527_421712, OUT_MULTIIF_main_419527_422136, OUT_MULTIIF_main_419527_423576, OUT_UNBOUNDED_main_419527_420296, OUT_UNBOUNDED_main_419527_420310);
  parameter MEM_var_419713_419512=32768, MEM_var_419737_419512=32768, MEM_var_420170_419527=32768, MEM_var_420270_419527=32768, MEM_var_420328_419527=32768, MEM_var_420468_419527=32768;
  // IN
  input clock;
  input reset;
  input [1:0] S_oe_ram;
  input [1:0] S_we_ram;
  input [33:0] S_addr_ram;
  input [63:0] S_Wdata_ram;
  input [11:0] S_data_ram_size;
  input [63:0] Sin_Rdata_ram;
  input [1:0] Sin_DataRdy;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE;
  input fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD;
  input fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  input fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  input selector_IN_UNBOUNDED_main_419527_420296;
  input selector_IN_UNBOUNDED_main_419527_420310;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0;
  input selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1;
  input selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0;
  input selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0;
  input selector_MUX_247_reg_0_0_0_0;
  input selector_MUX_308_reg_64_0_0_0;
  input selector_MUX_309_reg_65_0_0_0;
  input selector_MUX_319_reg_74_0_0_0;
  input selector_MUX_320_reg_75_0_0_0;
  input selector_MUX_321_reg_76_0_0_0;
  input selector_MUX_322_reg_77_0_0_0;
  input selector_MUX_324_reg_79_0_0_0;
  input selector_MUX_326_reg_80_0_0_0;
  input selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0;
  input selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1;
  input selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2;
  input selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0;
  input selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1;
  input selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0;
  input selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0;
  input selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0;
  input selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1;
  input selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2;
  input selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0;
  input selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1;
  input wrenable_reg_0;
  input wrenable_reg_1;
  input wrenable_reg_10;
  input wrenable_reg_11;
  input wrenable_reg_12;
  input wrenable_reg_13;
  input wrenable_reg_14;
  input wrenable_reg_15;
  input wrenable_reg_16;
  input wrenable_reg_17;
  input wrenable_reg_18;
  input wrenable_reg_19;
  input wrenable_reg_2;
  input wrenable_reg_20;
  input wrenable_reg_21;
  input wrenable_reg_22;
  input wrenable_reg_23;
  input wrenable_reg_24;
  input wrenable_reg_25;
  input wrenable_reg_26;
  input wrenable_reg_27;
  input wrenable_reg_28;
  input wrenable_reg_29;
  input wrenable_reg_3;
  input wrenable_reg_30;
  input wrenable_reg_31;
  input wrenable_reg_32;
  input wrenable_reg_33;
  input wrenable_reg_34;
  input wrenable_reg_35;
  input wrenable_reg_36;
  input wrenable_reg_37;
  input wrenable_reg_38;
  input wrenable_reg_39;
  input wrenable_reg_4;
  input wrenable_reg_40;
  input wrenable_reg_41;
  input wrenable_reg_42;
  input wrenable_reg_43;
  input wrenable_reg_44;
  input wrenable_reg_45;
  input wrenable_reg_46;
  input wrenable_reg_47;
  input wrenable_reg_48;
  input wrenable_reg_49;
  input wrenable_reg_5;
  input wrenable_reg_50;
  input wrenable_reg_51;
  input wrenable_reg_52;
  input wrenable_reg_53;
  input wrenable_reg_54;
  input wrenable_reg_55;
  input wrenable_reg_56;
  input wrenable_reg_57;
  input wrenable_reg_58;
  input wrenable_reg_59;
  input wrenable_reg_6;
  input wrenable_reg_60;
  input wrenable_reg_61;
  input wrenable_reg_62;
  input wrenable_reg_63;
  input wrenable_reg_64;
  input wrenable_reg_65;
  input wrenable_reg_66;
  input wrenable_reg_67;
  input wrenable_reg_68;
  input wrenable_reg_69;
  input wrenable_reg_7;
  input wrenable_reg_70;
  input wrenable_reg_71;
  input wrenable_reg_72;
  input wrenable_reg_73;
  input wrenable_reg_74;
  input wrenable_reg_75;
  input wrenable_reg_76;
  input wrenable_reg_77;
  input wrenable_reg_78;
  input wrenable_reg_79;
  input wrenable_reg_8;
  input wrenable_reg_80;
  input wrenable_reg_9;
  // OUT
  output [31:0] return_port;
  output [63:0] Sout_Rdata_ram;
  output [1:0] Sout_DataRdy;
  output OUT_CONDITION_main_419527_420260;
  output OUT_CONDITION_main_419527_420315;
  output OUT_CONDITION_main_419527_421689;
  output OUT_CONDITION_main_419527_421712;
  output [1:0] OUT_MULTIIF_main_419527_422136;
  output [1:0] OUT_MULTIIF_main_419527_423576;
  output OUT_UNBOUNDED_main_419527_420296;
  output OUT_UNBOUNDED_main_419527_420310;
  // Component and signal declarations
  wire null_out_signal_array_420270_0_Sout_DataRdy_0;
  wire null_out_signal_array_420270_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_420270_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_420270_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_420270_0_out1_1;
  wire [31:0] null_out_signal_array_420270_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_420270_0_proxy_out1_1;
  wire null_out_signal_array_420328_0_Sout_DataRdy_0;
  wire null_out_signal_array_420328_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_420328_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_420328_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_420328_0_out1_1;
  wire [31:0] null_out_signal_array_420328_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_420328_0_proxy_out1_1;
  wire null_out_signal_array_420468_0_Sout_DataRdy_0;
  wire null_out_signal_array_420468_0_Sout_DataRdy_1;
  wire [31:0] null_out_signal_array_420468_0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal_array_420468_0_Sout_Rdata_ram_1;
  wire [31:0] null_out_signal_array_420468_0_out1_1;
  wire [31:0] null_out_signal_array_420468_0_proxy_out1_0;
  wire [31:0] null_out_signal_array_420468_0_proxy_out1_1;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0;
  wire [31:0] out_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_array_420328_0;
  wire [31:0] out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0;
  wire [31:0] out_ASSIGN_UNSIGNED_FU_19_i0_fu_main_419527_423736;
  wire [31:0] out_IUdata_converter_FU_100_i0_fu_main_419527_422325;
  wire [31:0] out_IUdata_converter_FU_18_i0_fu_main_419527_422472;
  wire [31:0] out_IUdata_converter_FU_99_i0_fu_main_419527_422328;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0;
  wire [16:0] out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1;
  wire [16:0] out_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0;
  wire [6:0] out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0;
  wire [5:0] out_MUX_247_reg_0_0_0_0;
  wire [31:0] out_MUX_308_reg_64_0_0_0;
  wire [31:0] out_MUX_309_reg_65_0_0_0;
  wire [31:0] out_MUX_319_reg_74_0_0_0;
  wire out_MUX_320_reg_75_0_0_0;
  wire [31:0] out_MUX_321_reg_76_0_0_0;
  wire out_MUX_322_reg_77_0_0_0;
  wire [31:0] out_MUX_324_reg_79_0_0_0;
  wire [31:0] out_MUX_326_reg_80_0_0_0;
  wire [16:0] out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0;
  wire [16:0] out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1;
  wire [16:0] out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2;
  wire [16:0] out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0;
  wire [16:0] out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1;
  wire [6:0] out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0;
  wire [31:0] out_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0;
  wire [16:0] out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0;
  wire [16:0] out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1;
  wire [16:0] out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2;
  wire [16:0] out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0;
  wire [16:0] out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1;
  wire signed [31:0] out_UIdata_converter_FU_17_i0_fu_main_419527_422345;
  wire signed [31:0] out_UIdata_converter_FU_65_i0_fu_main_419527_422415;
  wire signed [31:0] out_UIdata_converter_FU_70_i0_fu_main_419527_422463;
  wire signed [31:0] out_UIdata_converter_FU_98_i0_fu_main_419527_422418;
  wire out_UUdata_converter_FU_101_i0_fu_main_419527_420419;
  wire out_UUdata_converter_FU_102_i0_fu_main_419527_420416;
  wire out_UUdata_converter_FU_103_i0_fu_main_419527_420412;
  wire out_UUdata_converter_FU_104_i0_fu_main_419527_420409;
  wire out_UUdata_converter_FU_110_i0_fu_main_419527_423720;
  wire out_UUdata_converter_FU_91_i0_fu_main_419527_421565;
  wire [16:0] out_addr_expr_FU_6_i0_fu_main_419527_420283;
  wire [16:0] out_addr_expr_FU_92_i0_fu_main_419527_420339;
  wire [16:0] out_addr_expr_FU_93_i0_fu_main_419527_421481;
  wire [16:0] out_addr_expr_FU_96_i0_fu_main_419527_422340;
  wire [31:0] out_aluDecode_154_i0_fu_main_419527_420310;
  wire out_const_0;
  wire [31:0] out_const_1;
  wire [5:0] out_const_10;
  wire [6:0] out_const_11;
  wire [7:0] out_const_12;
  wire [8:0] out_const_13;
  wire [9:0] out_const_14;
  wire [13:0] out_const_15;
  wire [14:0] out_const_16;
  wire [5:0] out_const_17;
  wire [2:0] out_const_18;
  wire [4:0] out_const_19;
  wire [4:0] out_const_2;
  wire [7:0] out_const_20;
  wire [1:0] out_const_21;
  wire [3:0] out_const_22;
  wire [4:0] out_const_23;
  wire [15:0] out_const_24;
  wire [4:0] out_const_25;
  wire [4:0] out_const_26;
  wire [4:0] out_const_27;
  wire [4:0] out_const_28;
  wire [15:0] out_const_29;
  wire [6:0] out_const_3;
  wire [31:0] out_const_30;
  wire [16:0] out_const_31;
  wire [15:0] out_const_32;
  wire [15:0] out_const_33;
  wire [15:0] out_const_34;
  wire [11:0] out_const_4;
  wire out_const_5;
  wire [1:0] out_const_6;
  wire [2:0] out_const_7;
  wire [3:0] out_const_8;
  wire [4:0] out_const_9;
  wire [14:0] out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_15;
  wire [1:0] out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2;
  wire [14:0] out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15;
  wire [1:0] out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2;
  wire [30:0] out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0_32_31;
  wire [5:0] out_conv_out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0_7_6;
  wire [5:0] out_conv_out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0_7_6;
  wire [4:0] out_conv_out_aluDecode_154_i0_fu_main_419527_420310_32_5;
  wire [31:0] out_conv_out_const_0_1_32;
  wire [6:0] out_conv_out_const_2_5_7;
  wire [31:0] out_conv_out_const_31_17_32;
  wire [31:0] out_conv_out_const_32_16_32;
  wire [31:0] out_conv_out_const_33_16_32;
  wire [31:0] out_conv_out_const_34_16_32;
  wire [5:0] out_conv_out_const_3_7_6;
  wire [16:0] out_conv_out_reg_10_reg_10_18_17;
  wire [16:0] out_conv_out_reg_11_reg_11_18_17;
  wire [16:0] out_conv_out_reg_12_reg_12_18_17;
  wire [16:0] out_conv_out_reg_13_reg_13_18_17;
  wire [16:0] out_conv_out_reg_14_reg_14_18_17;
  wire [16:0] out_conv_out_reg_15_reg_15_18_17;
  wire [16:0] out_conv_out_reg_16_reg_16_18_17;
  wire [16:0] out_conv_out_reg_17_reg_17_18_17;
  wire [16:0] out_conv_out_reg_23_reg_23_18_17;
  wire [31:0] out_conv_out_reg_4_reg_4_17_32;
  wire [16:0] out_conv_out_reg_66_reg_66_18_17;
  wire [16:0] out_conv_out_reg_67_reg_67_18_17;
  wire [16:0] out_conv_out_reg_6_reg_6_18_17;
  wire [16:0] out_conv_out_reg_72_reg_72_32_17;
  wire [16:0] out_conv_out_reg_78_reg_78_32_17;
  wire [16:0] out_conv_out_reg_7_reg_7_18_17;
  wire [16:0] out_conv_out_reg_8_reg_8_18_17;
  wire [16:0] out_conv_out_reg_9_reg_9_18_17;
  wire [5:0] out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0_1_6;
  wire [31:0] out_conv_out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1_1_32;
  wire [16:0] out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256_18_17;
  wire [16:0] out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292_32_17;
  wire [16:0] out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353_18_17;
  wire out_ge_expr_FU_32_32_32_113_i0_fu_main_419527_422420;
  wire out_lt_expr_FU_32_0_32_114_i0_fu_main_419527_422465;
  wire out_lt_expr_FU_32_32_32_115_i0_fu_main_419527_422411;
  wire out_lut_expr_FU_105_i0_fu_main_419527_424222;
  wire out_lut_expr_FU_106_i0_fu_main_419527_424226;
  wire out_lut_expr_FU_107_i0_fu_main_419527_424229;
  wire out_lut_expr_FU_108_i0_fu_main_419527_424232;
  wire out_lut_expr_FU_109_i0_fu_main_419527_424109;
  wire out_lut_expr_FU_10_i0_fu_main_419527_422336;
  wire out_lut_expr_FU_21_i0_fu_main_419527_422353;
  wire out_lut_expr_FU_27_i0_fu_main_419527_424155;
  wire out_lut_expr_FU_28_i0_fu_main_419527_422139;
  wire out_lut_expr_FU_29_i0_fu_main_419527_424159;
  wire out_lut_expr_FU_30_i0_fu_main_419527_422142;
  wire out_lut_expr_FU_31_i0_fu_main_419527_424163;
  wire out_lut_expr_FU_32_i0_fu_main_419527_422145;
  wire out_lut_expr_FU_33_i0_fu_main_419527_424167;
  wire out_lut_expr_FU_34_i0_fu_main_419527_422148;
  wire out_lut_expr_FU_35_i0_fu_main_419527_422151;
  wire out_lut_expr_FU_36_i0_fu_main_419527_422154;
  wire out_lut_expr_FU_37_i0_fu_main_419527_422157;
  wire out_lut_expr_FU_38_i0_fu_main_419527_422160;
  wire out_lut_expr_FU_39_i0_fu_main_419527_424175;
  wire out_lut_expr_FU_40_i0_fu_main_419527_422163;
  wire out_lut_expr_FU_41_i0_fu_main_419527_424180;
  wire out_lut_expr_FU_42_i0_fu_main_419527_422166;
  wire out_lut_expr_FU_43_i0_fu_main_419527_424184;
  wire out_lut_expr_FU_44_i0_fu_main_419527_422169;
  wire out_lut_expr_FU_45_i0_fu_main_419527_424188;
  wire out_lut_expr_FU_46_i0_fu_main_419527_422172;
  wire out_lut_expr_FU_47_i0_fu_main_419527_422175;
  wire out_lut_expr_FU_48_i0_fu_main_419527_422178;
  wire out_lut_expr_FU_49_i0_fu_main_419527_422181;
  wire out_lut_expr_FU_50_i0_fu_main_419527_422184;
  wire out_lut_expr_FU_51_i0_fu_main_419527_424196;
  wire out_lut_expr_FU_52_i0_fu_main_419527_422199;
  wire out_lut_expr_FU_53_i0_fu_main_419527_424200;
  wire out_lut_expr_FU_54_i0_fu_main_419527_422202;
  wire out_lut_expr_FU_55_i0_fu_main_419527_424204;
  wire out_lut_expr_FU_56_i0_fu_main_419527_422205;
  wire out_lut_expr_FU_57_i0_fu_main_419527_422208;
  wire out_lut_expr_FU_58_i0_fu_main_419527_422211;
  wire out_lut_expr_FU_59_i0_fu_main_419527_424210;
  wire out_lut_expr_FU_60_i0_fu_main_419527_422214;
  wire out_lut_expr_FU_61_i0_fu_main_419527_423572;
  wire out_lut_expr_FU_62_i0_fu_main_419527_423575;
  wire out_lut_expr_FU_64_i0_fu_main_419527_422436;
  wire out_lut_expr_FU_67_i0_fu_main_419527_422451;
  wire out_lut_expr_FU_69_i0_fu_main_419527_422454;
  wire out_lut_expr_FU_71_i0_fu_main_419527_422576;
  wire out_lut_expr_FU_74_i0_fu_main_419527_423582;
  wire out_lut_expr_FU_84_i0_fu_main_419527_422364;
  wire out_lut_expr_FU_85_i0_fu_main_419527_423593;
  wire out_lut_expr_FU_86_i0_fu_main_419527_423599;
  wire [1:0] out_multi_read_cond_FU_111_i0_fu_main_419527_422136;
  wire [1:0] out_multi_read_cond_FU_87_i0_fu_main_419527_423576;
  wire out_read_cond_FU_112_i0_fu_main_419527_421689;
  wire out_read_cond_FU_11_i0_fu_main_419527_420260;
  wire out_read_cond_FU_75_i0_fu_main_419527_420315;
  wire out_read_cond_FU_78_i0_fu_main_419527_421712;
  wire [5:0] out_reg_0_reg_0;
  wire [17:0] out_reg_10_reg_10;
  wire [17:0] out_reg_11_reg_11;
  wire [17:0] out_reg_12_reg_12;
  wire [17:0] out_reg_13_reg_13;
  wire [17:0] out_reg_14_reg_14;
  wire [17:0] out_reg_15_reg_15;
  wire [17:0] out_reg_16_reg_16;
  wire [17:0] out_reg_17_reg_17;
  wire [31:0] out_reg_18_reg_18;
  wire out_reg_19_reg_19;
  wire [16:0] out_reg_1_reg_1;
  wire [31:0] out_reg_20_reg_20;
  wire out_reg_21_reg_21;
  wire [31:0] out_reg_22_reg_22;
  wire [17:0] out_reg_23_reg_23;
  wire [31:0] out_reg_24_reg_24;
  wire [31:0] out_reg_25_reg_25;
  wire [14:0] out_reg_26_reg_26;
  wire [16:0] out_reg_27_reg_27;
  wire [31:0] out_reg_28_reg_28;
  wire [16:0] out_reg_29_reg_29;
  wire [16:0] out_reg_2_reg_2;
  wire [31:0] out_reg_30_reg_30;
  wire out_reg_31_reg_31;
  wire out_reg_32_reg_32;
  wire out_reg_33_reg_33;
  wire out_reg_34_reg_34;
  wire out_reg_35_reg_35;
  wire out_reg_36_reg_36;
  wire out_reg_37_reg_37;
  wire [31:0] out_reg_38_reg_38;
  wire [4:0] out_reg_39_reg_39;
  wire [16:0] out_reg_3_reg_3;
  wire out_reg_40_reg_40;
  wire out_reg_41_reg_41;
  wire out_reg_42_reg_42;
  wire out_reg_43_reg_43;
  wire out_reg_44_reg_44;
  wire out_reg_45_reg_45;
  wire out_reg_46_reg_46;
  wire out_reg_47_reg_47;
  wire out_reg_48_reg_48;
  wire out_reg_49_reg_49;
  wire [16:0] out_reg_4_reg_4;
  wire out_reg_50_reg_50;
  wire out_reg_51_reg_51;
  wire out_reg_52_reg_52;
  wire out_reg_53_reg_53;
  wire out_reg_54_reg_54;
  wire out_reg_55_reg_55;
  wire out_reg_56_reg_56;
  wire out_reg_57_reg_57;
  wire out_reg_58_reg_58;
  wire out_reg_59_reg_59;
  wire [16:0] out_reg_5_reg_5;
  wire out_reg_60_reg_60;
  wire out_reg_61_reg_61;
  wire out_reg_62_reg_62;
  wire out_reg_63_reg_63;
  wire [31:0] out_reg_64_reg_64;
  wire [31:0] out_reg_65_reg_65;
  wire [17:0] out_reg_66_reg_66;
  wire [17:0] out_reg_67_reg_67;
  wire [31:0] out_reg_68_reg_68;
  wire [31:0] out_reg_69_reg_69;
  wire [17:0] out_reg_6_reg_6;
  wire [31:0] out_reg_70_reg_70;
  wire [31:0] out_reg_71_reg_71;
  wire [31:0] out_reg_72_reg_72;
  wire [31:0] out_reg_73_reg_73;
  wire [31:0] out_reg_74_reg_74;
  wire out_reg_75_reg_75;
  wire [31:0] out_reg_76_reg_76;
  wire out_reg_77_reg_77;
  wire [31:0] out_reg_78_reg_78;
  wire [31:0] out_reg_79_reg_79;
  wire [17:0] out_reg_7_reg_7;
  wire [31:0] out_reg_80_reg_80;
  wire [17:0] out_reg_8_reg_8;
  wire [17:0] out_reg_9_reg_9;
  wire signed [30:0] out_rshift_expr_FU_32_0_32_116_i0_fu_main_419527_420295;
  wire signed [31:0] out_rshift_expr_FU_32_32_32_117_i0_fu_main_419527_420422;
  wire signed [31:0] out_rshift_expr_FU_32_32_32_117_i1_fu_main_419527_420426;
  wire [0:0] out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0;
  wire [0:0] out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1;
  wire [31:0] out_ui_bit_and_expr_FU_32_0_32_118_i0_fu_main_419527_420452;
  wire [31:0] out_ui_bit_and_expr_FU_32_32_32_119_i0_fu_main_419527_420403;
  wire [1:0] out_ui_bit_and_expr_FU_8_0_8_120_i0_fu_main_419527_422543;
  wire [31:0] out_ui_bit_ior_concat_expr_FU_121_i0_fu_main_419527_420442;
  wire [31:0] out_ui_bit_ior_expr_FU_32_32_32_122_i0_fu_main_419527_420405;
  wire [31:0] out_ui_bit_xor_expr_FU_32_32_32_123_i0_fu_main_419527_420407;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i0_fu_main_419527_423567;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i10_fu_main_419527_423657;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i11_fu_main_419527_423663;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i12_fu_main_419527_423669;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i13_fu_main_419527_423675;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i14_fu_main_419527_423681;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i15_fu_main_419527_423687;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i16_fu_main_419527_423693;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i17_fu_main_419527_423699;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i18_fu_main_419527_423705;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i19_fu_main_419527_423711;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i1_fu_main_419527_423603;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i20_fu_main_419527_423717;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i21_fu_main_419527_423723;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i22_fu_main_419527_423726;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i23_fu_main_419527_423729;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i24_fu_main_419527_423733;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i2_fu_main_419527_423609;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i3_fu_main_419527_423615;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i4_fu_main_419527_423621;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i5_fu_main_419527_423627;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i6_fu_main_419527_423633;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i7_fu_main_419527_423639;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i8_fu_main_419527_423645;
  wire [31:0] out_ui_cond_expr_FU_32_32_32_32_124_i9_fu_main_419527_423651;
  wire out_ui_eq_expr_FU_32_32_32_125_i0_fu_main_419527_422399;
  wire out_ui_extract_bit_expr_FU_20_i0_fu_main_419527_424113;
  wire out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749;
  wire out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752;
  wire out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758;
  wire out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761;
  wire out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764;
  wire out_ui_extract_bit_expr_FU_63_i0_fu_main_419527_424117;
  wire out_ui_extract_bit_expr_FU_66_i0_fu_main_419527_424121;
  wire out_ui_extract_bit_expr_FU_68_i0_fu_main_419527_424125;
  wire out_ui_extract_bit_expr_FU_72_i0_fu_main_419527_424132;
  wire out_ui_extract_bit_expr_FU_73_i0_fu_main_419527_424140;
  wire out_ui_extract_bit_expr_FU_83_i0_fu_main_419527_424064;
  wire out_ui_extract_bit_expr_FU_9_i0_fu_main_419527_423741;
  wire out_ui_ge_expr_FU_32_32_32_126_i0_fu_main_419527_422426;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_127_i0_fu_main_419527_420294;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_128_i0_fu_main_419527_422342;
  wire [16:0] out_ui_lshift_expr_FU_32_0_32_128_i1_fu_main_419527_422347;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_128_i2_fu_main_419527_422374;
  wire [16:0] out_ui_lshift_expr_FU_32_0_32_128_i3_fu_main_419527_422378;
  wire [16:0] out_ui_lshift_expr_FU_32_0_32_128_i4_fu_main_419527_422429;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_128_i5_fu_main_419527_422444;
  wire [31:0] out_ui_lshift_expr_FU_32_0_32_128_i6_fu_main_419527_422539;
  wire [31:0] out_ui_lshift_expr_FU_32_32_32_129_i0_fu_main_419527_420433;
  wire [31:0] out_ui_lshift_expr_FU_32_32_32_129_i1_fu_main_419527_420435;
  wire [7:0] out_ui_lshift_expr_FU_8_0_8_130_i0_fu_main_419527_422330;
  wire out_ui_lt_expr_FU_32_32_32_131_i0_fu_main_419527_422423;
  wire [31:0] out_ui_minus_expr_FU_32_32_32_132_i0_fu_main_419527_420401;
  wire [31:0] out_ui_mult_expr_FU_32_32_32_0_133_i0_fu_main_419527_420437;
  wire out_ui_ne_expr_FU_32_0_32_134_i0_fu_main_419527_422459;
  wire out_ui_ne_expr_FU_32_32_32_135_i0_fu_main_419527_422402;
  wire [29:0] out_ui_plus_expr_FU_32_0_32_136_i0_fu_main_419527_422536;
  wire [31:0] out_ui_plus_expr_FU_32_32_32_137_i0_fu_main_419527_420342;
  wire [31:0] out_ui_plus_expr_FU_32_32_32_137_i1_fu_main_419527_420439;
  wire [31:0] out_ui_plus_expr_FU_32_32_32_137_i2_fu_main_419527_420450;
  wire [31:0] out_ui_plus_expr_FU_32_32_32_137_i3_fu_main_419527_420457;
  wire [5:0] out_ui_plus_expr_FU_8_0_8_138_i0_fu_main_419527_420258;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_139_i0_fu_main_419527_421489;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_140_i0_fu_main_419527_421493;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_141_i0_fu_main_419527_421498;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_142_i0_fu_main_419527_421503;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_143_i0_fu_main_419527_421508;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_144_i0_fu_main_419527_421512;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_145_i0_fu_main_419527_421517;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_146_i0_fu_main_419527_421522;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_147_i0_fu_main_419527_421527;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_148_i0_fu_main_419527_421532;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_149_i0_fu_main_419527_421537;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_0_32_150_i0_fu_main_419527_421542;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256;
  wire [31:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i2_fu_main_419527_420311;
  wire [31:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i3_fu_main_419527_420329;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i4_fu_main_419527_420367;
  wire [31:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i5_fu_main_419527_420377;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353;
  wire [17:0] out_ui_pointer_plus_expr_FU_32_32_32_151_i7_fu_main_419527_420360;
  wire [29:0] out_ui_rshift_expr_FU_32_0_32_152_i0_fu_main_419527_422531;
  wire [31:0] out_ui_rshift_expr_FU_32_32_32_153_i0_fu_main_419527_420429;
  wire [31:0] out_ui_rshift_expr_FU_32_32_32_153_i1_fu_main_419527_420431;
  wire [16:0] out_ui_view_convert_expr_FU_94_i0_fu_main_419527_420279;
  wire [16:0] out_ui_view_convert_expr_FU_95_i0_fu_main_419527_420276;
  wire [31:0] out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_2;
  wire [31:0] out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_3;
  wire [31:0] out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_4;
  wire [63:0] proxy_out1_420170;
  wire s_done_fu_main_419527_420296;
  wire s_done_fu_main_419527_420310;
  wire [1:0] sig_in_bus_mergerSout_DataRdy0_0;
  wire [63:0] sig_in_bus_mergerSout_Rdata_ram1_0;
  wire [63:0] sig_in_bus_mergerproxy_in12_0;
  wire [33:0] sig_in_bus_mergerproxy_in23_0;
  wire [11:0] sig_in_bus_mergerproxy_in34_0;
  wire [1:0] sig_in_bus_mergerproxy_sel_LOAD5_0;
  wire [1:0] sig_in_bus_mergerproxy_sel_STORE6_0;
  wire [1:0] sig_in_vector_bus_mergerSout_DataRdy0_0;
  wire [63:0] sig_in_vector_bus_mergerSout_Rdata_ram1_0;
  wire [63:0] sig_in_vector_bus_mergerproxy_in12_0;
  wire [33:0] sig_in_vector_bus_mergerproxy_in23_0;
  wire [11:0] sig_in_vector_bus_mergerproxy_in34_0;
  wire [1:0] sig_in_vector_bus_mergerproxy_sel_LOAD5_0;
  wire [1:0] sig_in_vector_bus_mergerproxy_sel_STORE6_0;
  wire [1:0] sig_out_bus_mergerSout_DataRdy0_;
  wire [63:0] sig_out_bus_mergerSout_Rdata_ram1_;
  wire [63:0] sig_out_bus_mergerproxy_in12_;
  wire [33:0] sig_out_bus_mergerproxy_in23_;
  wire [11:0] sig_out_bus_mergerproxy_in34_;
  wire [1:0] sig_out_bus_mergerproxy_sel_LOAD5_;
  wire [1:0] sig_out_bus_mergerproxy_sel_STORE6_;
  wire [63:0] sig_out_vector_bus_mergerproxy_in12_;
  wire [33:0] sig_out_vector_bus_mergerproxy_in23_;
  wire [11:0] sig_out_vector_bus_mergerproxy_in34_;
  wire [1:0] sig_out_vector_bus_mergerproxy_sel_LOAD5_;
  wire [1:0] sig_out_vector_bus_mergerproxy_sel_STORE6_;
  
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) ASSIGN_UNSIGNED_FU_u_assign_0 (.out1(out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0), .in1(out_const_0));
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) ASSIGN_UNSIGNED_FU_u_assign_1 (.out1(out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1), .in1(out_const_0));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_2 (.out1(out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_2), .in1(out_const_1));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_3 (.out1(out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_3), .in1(out_reg_73_reg_73));
  ASSIGN_VECTOR_BOOL_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) ASSIGN_VECTOR_BOOL_FU_vb_assign_4 (.out1(out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_4), .in1(out_reg_80_reg_80));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0), .in1(out_conv_out_reg_8_reg_8_18_17), .in2(out_conv_out_reg_7_reg_7_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1), .in1(out_conv_out_reg_16_reg_16_18_17), .in2(out_conv_out_reg_14_reg_14_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2), .in1(out_conv_out_reg_12_reg_12_18_17), .in2(out_conv_out_reg_11_reg_11_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3), .in1(out_conv_out_reg_10_reg_10_18_17), .in2(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0), .in1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1), .in2(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 (.out1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1), .sel(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1), .in1(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3), .in2(out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0 (.out1(out_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0), .sel(selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0), .in1(out_conv_out_reg_78_reg_78_32_17), .in2(out_conv_out_reg_72_reg_72_32_17));
  MUX_GATE #(.BITSIZE_in1(7), .BITSIZE_in2(7), .BITSIZE_out1(7)) MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 (.out1(out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0), .sel(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0), .in1(out_conv_out_const_2_5_7), .in2(out_const_3));
  MUX_GATE #(.BITSIZE_in1(6), .BITSIZE_in2(6), .BITSIZE_out1(6)) MUX_247_reg_0_0_0_0 (.out1(out_MUX_247_reg_0_0_0_0), .sel(selector_MUX_247_reg_0_0_0_0), .in1(out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0_1_6), .in2(out_ui_plus_expr_FU_8_0_8_138_i0_fu_main_419527_420258));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_308_reg_64_0_0_0 (.out1(out_MUX_308_reg_64_0_0_0), .sel(selector_MUX_308_reg_64_0_0_0), .in1(out_reg_38_reg_38), .in2(out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_309_reg_65_0_0_0 (.out1(out_MUX_309_reg_65_0_0_0), .sel(selector_MUX_309_reg_65_0_0_0), .in1(out_reg_38_reg_38), .in2(out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_319_reg_74_0_0_0 (.out1(out_MUX_319_reg_74_0_0_0), .sel(selector_MUX_319_reg_74_0_0_0), .in1(out_ui_cond_expr_FU_32_32_32_32_124_i21_fu_main_419527_423723), .in2(out_ui_mult_expr_FU_32_32_32_0_133_i0_fu_main_419527_420437));
  MUX_GATE #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_out1(1)) MUX_320_reg_75_0_0_0 (.out1(out_MUX_320_reg_75_0_0_0), .sel(selector_MUX_320_reg_75_0_0_0), .in1(out_const_0), .in2(out_UUdata_converter_FU_110_i0_fu_main_419527_423720));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_321_reg_76_0_0_0 (.out1(out_MUX_321_reg_76_0_0_0), .sel(selector_MUX_321_reg_76_0_0_0), .in1(out_reg_74_reg_74), .in2(out_ui_plus_expr_FU_32_32_32_137_i0_fu_main_419527_420342));
  MUX_GATE #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_out1(1)) MUX_322_reg_77_0_0_0 (.out1(out_MUX_322_reg_77_0_0_0), .sel(selector_MUX_322_reg_77_0_0_0), .in1(out_const_0), .in2(out_UUdata_converter_FU_91_i0_fu_main_419527_421565));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_324_reg_79_0_0_0 (.out1(out_MUX_324_reg_79_0_0_0), .sel(selector_MUX_324_reg_79_0_0_0), .in1(out_conv_out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1_1_32), .in2(out_ui_cond_expr_FU_32_32_32_32_124_i24_fu_main_419527_423733));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_326_reg_80_0_0_0 (.out1(out_MUX_326_reg_80_0_0_0), .sel(selector_MUX_326_reg_80_0_0_0), .in1(out_reg_76_reg_76), .in2(out_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_array_420328_0));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0 (.out1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0), .sel(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0), .in1(out_conv_out_reg_9_reg_9_18_17), .in2(out_conv_out_reg_6_reg_6_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1 (.out1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1), .sel(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1), .in1(out_reg_4_reg_4), .in2(out_conv_out_reg_17_reg_17_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2 (.out1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2), .sel(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2), .in1(out_conv_out_reg_15_reg_15_18_17), .in2(out_conv_out_reg_13_reg_13_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 (.out1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0), .sel(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0), .in1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0), .in2(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 (.out1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1), .sel(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1), .in1(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2), .in2(out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0));
  MUX_GATE #(.BITSIZE_in1(7), .BITSIZE_in2(7), .BITSIZE_out1(7)) MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 (.out1(out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0), .sel(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0), .in1(out_conv_out_const_2_5_7), .in2(out_const_3));
  MUX_GATE #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 (.out1(out_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0), .sel(selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0), .in1(out_vb_assign_conn_obj_1_ASSIGN_VECTOR_BOOL_FU_vb_assign_2), .in2(out_vb_assign_conn_obj_4_ASSIGN_VECTOR_BOOL_FU_vb_assign_4));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0 (.out1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0), .sel(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0), .in1(out_conv_out_reg_67_reg_67_18_17), .in2(out_conv_out_reg_66_reg_66_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1 (.out1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1), .sel(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1), .in1(out_conv_out_reg_23_reg_23_18_17), .in2(out_reg_2_reg_2));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2 (.out1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2), .sel(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2), .in1(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256_18_17), .in2(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353_18_17));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 (.out1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0), .sel(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0), .in1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0), .in2(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1));
  MUX_GATE #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(17)) MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 (.out1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1), .sel(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1), .in1(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2), .in2(out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0));
  ARRAY_1D_STD_BRAM_NN #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file_a("array_ref_420170.mem"), .MEMORY_INIT_file_b("0_array_ref_420170.mem"), .n_elements(1), .data_size(288), .address_space_begin(MEM_var_420170_419527), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(0), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_420170_0 (.out1({out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0, out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0}), .Sout_Rdata_ram(sig_in_vector_bus_mergerSout_Rdata_ram1_0), .Sout_DataRdy(sig_in_vector_bus_mergerSout_DataRdy0_0), .proxy_out1(proxy_out1_420170), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .in2({out_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1, out_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1}), .in3({out_conv_out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0_7_6, out_conv_out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0_7_6}), .in4({out_const_5, out_const_5}), .sel_LOAD({fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD}), .sel_STORE({fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE}), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .Sin_Rdata_ram(Sin_Rdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_DataRdy(Sin_DataRdy), .proxy_in1(sig_out_vector_bus_mergerproxy_in12_), .proxy_in2(sig_out_vector_bus_mergerproxy_in23_), .proxy_in3(sig_out_vector_bus_mergerproxy_in34_), .proxy_sel_LOAD(sig_out_vector_bus_mergerproxy_sel_LOAD5_), .proxy_sel_STORE(sig_out_vector_bus_mergerproxy_sel_STORE6_));
  ARRAY_1D_STD_BRAM_NN_SDS #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file("array_ref_420270.mem"), .n_elements(32), .data_size(32), .address_space_begin(MEM_var_420270_419527), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(1), .READ_ONLY_MEMORY(0), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_420270_0 (.out1({null_out_signal_array_420270_0_out1_1, out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0}), .Sout_Rdata_ram({null_out_signal_array_420270_0_Sout_Rdata_ram_1, null_out_signal_array_420270_0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal_array_420270_0_Sout_DataRdy_1, null_out_signal_array_420270_0_Sout_DataRdy_0}), .proxy_out1({null_out_signal_array_420270_0_proxy_out1_1, null_out_signal_array_420270_0_proxy_out1_0}), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, out_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0}), .in2({17'b00000000000000000, out_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1}), .in3({6'b000000, out_conv_out_const_3_7_6}), .in4({1'b0, out_const_5}), .sel_LOAD({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD}), .sel_STORE({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE}), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_DataRdy({1'b0, 1'b0}), .proxy_in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .proxy_in2({17'b00000000000000000, 17'b00000000000000000}), .proxy_in3({6'b000000, 6'b000000}), .proxy_sel_LOAD({1'b0, 1'b0}), .proxy_sel_STORE({1'b0, 1'b0}));
  ARRAY_1D_STD_BRAM_NN_SDS #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file("array_ref_420328.mem"), .n_elements(8192), .data_size(32), .address_space_begin(MEM_var_420328_419527), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(1), .READ_ONLY_MEMORY(0), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_420328_0 (.out1({null_out_signal_array_420328_0_out1_1, out_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_array_420328_0}), .Sout_Rdata_ram({null_out_signal_array_420328_0_Sout_Rdata_ram_1, null_out_signal_array_420328_0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal_array_420328_0_Sout_DataRdy_1, null_out_signal_array_420328_0_Sout_DataRdy_0}), .proxy_out1({null_out_signal_array_420328_0_proxy_out1_1, null_out_signal_array_420328_0_proxy_out1_0}), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, out_vb_assign_conn_obj_3_ASSIGN_VECTOR_BOOL_FU_vb_assign_3}), .in2({17'b00000000000000000, out_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0}), .in3({6'b000000, out_conv_out_const_3_7_6}), .in4({1'b0, out_const_5}), .sel_LOAD({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD}), .sel_STORE({1'b0, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE}), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_DataRdy({1'b0, 1'b0}), .proxy_in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .proxy_in2({17'b00000000000000000, 17'b00000000000000000}), .proxy_in3({6'b000000, 6'b000000}), .proxy_sel_LOAD({1'b0, 1'b0}), .proxy_sel_STORE({1'b0, 1'b0}));
  ARRAY_1D_STD_DISTRAM_NN_SDS #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_in2(17), .PORTSIZE_in2(2), .BITSIZE_in3(6), .PORTSIZE_in3(2), .BITSIZE_in4(1), .PORTSIZE_in4(2), .BITSIZE_sel_LOAD(1), .PORTSIZE_sel_LOAD(2), .BITSIZE_sel_STORE(1), .PORTSIZE_sel_STORE(2), .BITSIZE_S_oe_ram(1), .PORTSIZE_S_oe_ram(2), .BITSIZE_S_we_ram(1), .PORTSIZE_S_we_ram(2), .BITSIZE_out1(32), .PORTSIZE_out1(2), .BITSIZE_S_addr_ram(17), .PORTSIZE_S_addr_ram(2), .BITSIZE_S_Wdata_ram(32), .PORTSIZE_S_Wdata_ram(2), .BITSIZE_Sin_Rdata_ram(32), .PORTSIZE_Sin_Rdata_ram(2), .BITSIZE_Sout_Rdata_ram(32), .PORTSIZE_Sout_Rdata_ram(2), .BITSIZE_S_data_ram_size(6), .PORTSIZE_S_data_ram_size(2), .BITSIZE_Sin_DataRdy(1), .PORTSIZE_Sin_DataRdy(2), .BITSIZE_Sout_DataRdy(1), .PORTSIZE_Sout_DataRdy(2), .MEMORY_INIT_file("array_ref_420468.mem"), .n_elements(1024), .data_size(32), .address_space_begin(MEM_var_420468_419527), .address_space_rangesize(32768), .BUS_PIPELINED(1), .BRAM_BITSIZE(32), .PRIVATE_MEMORY(1), .READ_ONLY_MEMORY(1), .USE_SPARSE_MEMORY(1), .BITSIZE_proxy_in1(32), .PORTSIZE_proxy_in1(2), .BITSIZE_proxy_in2(17), .PORTSIZE_proxy_in2(2), .BITSIZE_proxy_in3(6), .PORTSIZE_proxy_in3(2), .BITSIZE_proxy_sel_LOAD(1), .PORTSIZE_proxy_sel_LOAD(2), .BITSIZE_proxy_sel_STORE(1), .PORTSIZE_proxy_sel_STORE(2), .BITSIZE_proxy_out1(32), .PORTSIZE_proxy_out1(2)) array_420468_0 (.out1({null_out_signal_array_420468_0_out1_1, out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0}), .Sout_Rdata_ram({null_out_signal_array_420468_0_Sout_Rdata_ram_1, null_out_signal_array_420468_0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal_array_420468_0_Sout_DataRdy_1, null_out_signal_array_420468_0_Sout_DataRdy_0}), .proxy_out1({null_out_signal_array_420468_0_proxy_out1_1, null_out_signal_array_420468_0_proxy_out1_0}), .clock(clock), .reset(reset), .in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .in2({17'b00000000000000000, out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292_32_17}), .in3({6'b000000, out_conv_out_const_3_7_6}), .in4({1'b0, out_const_5}), .sel_LOAD({1'b0, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD}), .sel_STORE({1'b0, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE}), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_DataRdy({1'b0, 1'b0}), .proxy_in1({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .proxy_in2({17'b00000000000000000, 17'b00000000000000000}), .proxy_in3({6'b000000, 6'b000000}), .proxy_sel_LOAD({1'b0, 1'b0}), .proxy_sel_STORE({1'b0, 1'b0}));
  bus_merger #(.BITSIZE_in1(2), .PORTSIZE_in1(1), .BITSIZE_out1(2)) bus_mergerSout_DataRdy0_ (.out1(sig_out_bus_mergerSout_DataRdy0_), .in1({sig_in_bus_mergerSout_DataRdy0_0}));
  bus_merger #(.BITSIZE_in1(64), .PORTSIZE_in1(1), .BITSIZE_out1(64)) bus_mergerSout_Rdata_ram1_ (.out1(sig_out_bus_mergerSout_Rdata_ram1_), .in1({sig_in_bus_mergerSout_Rdata_ram1_0}));
  bus_merger #(.BITSIZE_in1(64), .PORTSIZE_in1(1), .BITSIZE_out1(64)) bus_mergerproxy_in12_ (.out1(sig_out_bus_mergerproxy_in12_), .in1({sig_in_bus_mergerproxy_in12_0}));
  bus_merger #(.BITSIZE_in1(34), .PORTSIZE_in1(1), .BITSIZE_out1(34)) bus_mergerproxy_in23_ (.out1(sig_out_bus_mergerproxy_in23_), .in1({sig_in_bus_mergerproxy_in23_0}));
  bus_merger #(.BITSIZE_in1(12), .PORTSIZE_in1(1), .BITSIZE_out1(12)) bus_mergerproxy_in34_ (.out1(sig_out_bus_mergerproxy_in34_), .in1({sig_in_bus_mergerproxy_in34_0}));
  bus_merger #(.BITSIZE_in1(2), .PORTSIZE_in1(1), .BITSIZE_out1(2)) bus_mergerproxy_sel_LOAD5_ (.out1(sig_out_bus_mergerproxy_sel_LOAD5_), .in1({sig_in_bus_mergerproxy_sel_LOAD5_0}));
  bus_merger #(.BITSIZE_in1(2), .PORTSIZE_in1(1), .BITSIZE_out1(2)) bus_mergerproxy_sel_STORE6_ (.out1(sig_out_bus_mergerproxy_sel_STORE6_), .in1({sig_in_bus_mergerproxy_sel_STORE6_0}));
  constant_value #(.BITSIZE_out1(1), .value(1'b0)) const_0 (.out1(out_const_0));
  constant_value #(.BITSIZE_out1(32), .value(32'b00000000000000000000000000000000)) const_1 (.out1(out_const_1));
  constant_value #(.BITSIZE_out1(6), .value(6'b100000)) const_10 (.out1(out_const_10));
  constant_value #(.BITSIZE_out1(7), .value(7'b1000000)) const_11 (.out1(out_const_11));
  constant_value #(.BITSIZE_out1(8), .value(8'b10000000)) const_12 (.out1(out_const_12));
  constant_value #(.BITSIZE_out1(9), .value(9'b100000000)) const_13 (.out1(out_const_13));
  constant_value #(.BITSIZE_out1(10), .value(10'b1000000000)) const_14 (.out1(out_const_14));
  constant_value #(.BITSIZE_out1(14), .value(14'b10000000000000)) const_15 (.out1(out_const_15));
  constant_value #(.BITSIZE_out1(15), .value(15'b100000000000000)) const_16 (.out1(out_const_16));
  constant_value #(.BITSIZE_out1(6), .value(6'b100001)) const_17 (.out1(out_const_17));
  constant_value #(.BITSIZE_out1(3), .value(3'b101)) const_18 (.out1(out_const_18));
  constant_value #(.BITSIZE_out1(5), .value(5'b10100)) const_19 (.out1(out_const_19));
  constant_value #(.BITSIZE_out1(5), .value(5'b01000)) const_2 (.out1(out_const_2));
  constant_value #(.BITSIZE_out1(8), .value(8'b10111000)) const_20 (.out1(out_const_20));
  constant_value #(.BITSIZE_out1(2), .value(2'b11)) const_21 (.out1(out_const_21));
  constant_value #(.BITSIZE_out1(4), .value(4'b1100)) const_22 (.out1(out_const_22));
  constant_value #(.BITSIZE_out1(5), .value(5'b11000)) const_23 (.out1(out_const_23));
  constant_value #(.BITSIZE_out1(16), .value(16'b1100110010100000)) const_24 (.out1(out_const_24));
  constant_value #(.BITSIZE_out1(5), .value(5'b11100)) const_25 (.out1(out_const_25));
  constant_value #(.BITSIZE_out1(5), .value(5'b11101)) const_26 (.out1(out_const_26));
  constant_value #(.BITSIZE_out1(5), .value(5'b11110)) const_27 (.out1(out_const_27));
  constant_value #(.BITSIZE_out1(5), .value(5'b11111)) const_28 (.out1(out_const_28));
  constant_value #(.BITSIZE_out1(16), .value(16'b1111101111111000)) const_29 (.out1(out_const_29));
  constant_value #(.BITSIZE_out1(7), .value(7'b0100000)) const_3 (.out1(out_const_3));
  constant_value #(.BITSIZE_out1(32), .value(32'b11111111111111111111111111111110)) const_30 (.out1(out_const_30));
  constant_value #(.BITSIZE_out1(17), .value(MEM_var_420170_419527)) const_31 (.out1(out_const_31));
  constant_value #(.BITSIZE_out1(16), .value(MEM_var_420270_419527)) const_32 (.out1(out_const_32));
  constant_value #(.BITSIZE_out1(16), .value(MEM_var_420328_419527)) const_33 (.out1(out_const_33));
  constant_value #(.BITSIZE_out1(16), .value(MEM_var_420468_419527)) const_34 (.out1(out_const_34));
  constant_value #(.BITSIZE_out1(12), .value(12'b010000000000)) const_4 (.out1(out_const_4));
  constant_value #(.BITSIZE_out1(1), .value(1'b1)) const_5 (.out1(out_const_5));
  constant_value #(.BITSIZE_out1(2), .value(2'b10)) const_6 (.out1(out_const_6));
  constant_value #(.BITSIZE_out1(3), .value(3'b100)) const_7 (.out1(out_const_7));
  constant_value #(.BITSIZE_out1(4), .value(4'b1000)) const_8 (.out1(out_const_8));
  constant_value #(.BITSIZE_out1(5), .value(5'b10000)) const_9 (.out1(out_const_9));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(15)) conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_15 (.out1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_15), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(2)) conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2 (.out1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(15)) conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15 (.out1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(2)) conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2 (.out1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(31)) conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0_32_31 (.out1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0_32_31), .in1(out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0_7_6 (.out1(out_conv_out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0_7_6), .in1(out_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0_7_6 (.out1(out_conv_out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0_7_6), .in1(out_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(5)) conv_out_aluDecode_154_i0_fu_main_419527_420310_32_5 (.out1(out_conv_out_aluDecode_154_i0_fu_main_419527_420310_32_5), .in1(out_aluDecode_154_i0_fu_main_419527_420310));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(32)) conv_out_const_0_1_32 (.out1(out_conv_out_const_0_1_32), .in1(out_const_0));
  UUdata_converter_FU #(.BITSIZE_in1(5), .BITSIZE_out1(7)) conv_out_const_2_5_7 (.out1(out_conv_out_const_2_5_7), .in1(out_const_2));
  UUdata_converter_FU #(.BITSIZE_in1(17), .BITSIZE_out1(32)) conv_out_const_31_17_32 (.out1(out_conv_out_const_31_17_32), .in1(out_const_31));
  UUdata_converter_FU #(.BITSIZE_in1(16), .BITSIZE_out1(32)) conv_out_const_32_16_32 (.out1(out_conv_out_const_32_16_32), .in1(out_const_32));
  UUdata_converter_FU #(.BITSIZE_in1(16), .BITSIZE_out1(32)) conv_out_const_33_16_32 (.out1(out_conv_out_const_33_16_32), .in1(out_const_33));
  UUdata_converter_FU #(.BITSIZE_in1(16), .BITSIZE_out1(32)) conv_out_const_34_16_32 (.out1(out_conv_out_const_34_16_32), .in1(out_const_34));
  UUdata_converter_FU #(.BITSIZE_in1(7), .BITSIZE_out1(6)) conv_out_const_3_7_6 (.out1(out_conv_out_const_3_7_6), .in1(out_const_3));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_10_reg_10_18_17 (.out1(out_conv_out_reg_10_reg_10_18_17), .in1(out_reg_10_reg_10));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_11_reg_11_18_17 (.out1(out_conv_out_reg_11_reg_11_18_17), .in1(out_reg_11_reg_11));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_12_reg_12_18_17 (.out1(out_conv_out_reg_12_reg_12_18_17), .in1(out_reg_12_reg_12));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_13_reg_13_18_17 (.out1(out_conv_out_reg_13_reg_13_18_17), .in1(out_reg_13_reg_13));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_14_reg_14_18_17 (.out1(out_conv_out_reg_14_reg_14_18_17), .in1(out_reg_14_reg_14));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_15_reg_15_18_17 (.out1(out_conv_out_reg_15_reg_15_18_17), .in1(out_reg_15_reg_15));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_16_reg_16_18_17 (.out1(out_conv_out_reg_16_reg_16_18_17), .in1(out_reg_16_reg_16));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_17_reg_17_18_17 (.out1(out_conv_out_reg_17_reg_17_18_17), .in1(out_reg_17_reg_17));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_23_reg_23_18_17 (.out1(out_conv_out_reg_23_reg_23_18_17), .in1(out_reg_23_reg_23));
  UUdata_converter_FU #(.BITSIZE_in1(17), .BITSIZE_out1(32)) conv_out_reg_4_reg_4_17_32 (.out1(out_conv_out_reg_4_reg_4_17_32), .in1(out_reg_4_reg_4));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_66_reg_66_18_17 (.out1(out_conv_out_reg_66_reg_66_18_17), .in1(out_reg_66_reg_66));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_67_reg_67_18_17 (.out1(out_conv_out_reg_67_reg_67_18_17), .in1(out_reg_67_reg_67));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_6_reg_6_18_17 (.out1(out_conv_out_reg_6_reg_6_18_17), .in1(out_reg_6_reg_6));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) conv_out_reg_72_reg_72_32_17 (.out1(out_conv_out_reg_72_reg_72_32_17), .in1(out_reg_72_reg_72));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) conv_out_reg_78_reg_78_32_17 (.out1(out_conv_out_reg_78_reg_78_32_17), .in1(out_reg_78_reg_78));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_7_reg_7_18_17 (.out1(out_conv_out_reg_7_reg_7_18_17), .in1(out_reg_7_reg_7));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_8_reg_8_18_17 (.out1(out_conv_out_reg_8_reg_8_18_17), .in1(out_reg_8_reg_8));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_reg_9_reg_9_18_17 (.out1(out_conv_out_reg_9_reg_9_18_17), .in1(out_reg_9_reg_9));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(6)) conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0_1_6 (.out1(out_conv_out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0_1_6), .in1(out_u_assign_conn_obj_0_ASSIGN_UNSIGNED_FU_u_assign_0));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(32)) conv_out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1_1_32 (.out1(out_conv_out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1_1_32), .in1(out_u_assign_conn_obj_2_ASSIGN_UNSIGNED_FU_u_assign_1));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256_18_17 (.out1(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256_18_17), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256));
  UUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292_32_17 (.out1(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292_32_17), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292));
  UUdata_converter_FU #(.BITSIZE_in1(18), .BITSIZE_out1(17)) conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353_18_17 (.out1(out_conv_out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353_18_17), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(8), .BITSIZE_out1(18), .LSB_PARAMETER(2)) fu_main_419527_420256 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i0_fu_main_419527_420256), .in1(out_reg_1_reg_1), .in2(out_ui_lshift_expr_FU_8_0_8_130_i0_fu_main_419527_422330));
  ui_plus_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(1), .BITSIZE_out1(6)) fu_main_419527_420258 (.out1(out_ui_plus_expr_FU_8_0_8_138_i0_fu_main_419527_420258), .in1(out_reg_0_reg_0), .in2(out_const_5));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_419527_420260 (.out1(out_read_cond_FU_11_i0_fu_main_419527_420260), .in1(out_lut_expr_FU_10_i0_fu_main_419527_422336));
  ui_view_convert_expr_FU #(.BITSIZE_in1(17), .BITSIZE_out1(17)) fu_main_419527_420276 (.out1(out_ui_view_convert_expr_FU_95_i0_fu_main_419527_420276), .in1(out_ui_view_convert_expr_FU_94_i0_fu_main_419527_420279));
  ui_view_convert_expr_FU #(.BITSIZE_in1(17), .BITSIZE_out1(17)) fu_main_419527_420279 (.out1(out_ui_view_convert_expr_FU_94_i0_fu_main_419527_420279), .in1(out_addr_expr_FU_6_i0_fu_main_419527_420283));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_main_419527_420283 (.out1(out_addr_expr_FU_6_i0_fu_main_419527_420283), .in1(out_conv_out_const_32_16_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(32), .BITSIZE_out1(32), .LSB_PARAMETER(2)) fu_main_419527_420292 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i1_fu_main_419527_420292), .in1(out_reg_5_reg_5), .in2(out_ui_lshift_expr_FU_32_0_32_128_i0_fu_main_419527_422342));
  ui_lshift_expr_FU #(.BITSIZE_in1(31), .BITSIZE_in2(1), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420294 (.out1(out_ui_lshift_expr_FU_32_0_32_127_i0_fu_main_419527_420294), .in1(out_conv_out_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_array_420468_0_32_31), .in2(out_const_5));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(31), .PRECISION(32)) fu_main_419527_420295 (.out1(out_rshift_expr_FU_32_0_32_116_i0_fu_main_419527_420295), .in1(out_UIdata_converter_FU_17_i0_fu_main_419527_422345), .in2(out_const_5));
  instDecode fu_main_419527_420296 (.done_port(s_done_fu_main_419527_420296), .proxy_in1_420170(sig_in_vector_bus_mergerproxy_in12_0), .proxy_in2_420170(sig_in_vector_bus_mergerproxy_in23_0), .proxy_in3_420170(sig_in_vector_bus_mergerproxy_in34_0), .proxy_sel_LOAD_420170(sig_in_vector_bus_mergerproxy_sel_LOAD5_0), .proxy_sel_STORE_420170(sig_in_vector_bus_mergerproxy_sel_STORE6_0), .clock(clock), .reset(reset), .start_port(selector_IN_UNBOUNDED_main_419527_420296), .P0(out_conv_out_reg_4_reg_4_17_32), .P1(out_reg_20_reg_20), .proxy_out1_420170(proxy_out1_420170));
  aluDecode #(.MEM_var_419713_419512(MEM_var_419713_419512), .MEM_var_419737_419512(MEM_var_419737_419512)) fu_main_419527_420310 (.done_port(s_done_fu_main_419527_420310), .return_port(out_aluDecode_154_i0_fu_main_419527_420310), .clock(clock), .reset(reset), .start_port(selector_IN_UNBOUNDED_main_419527_420310), .opcode(out_reg_25_reg_25), .funct3(out_reg_24_reg_24), .funct7(out_reg_22_reg_22));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(2)) fu_main_419527_420311 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i2_fu_main_419527_420311), .in1(out_reg_2_reg_2), .in2(out_ui_lshift_expr_FU_32_0_32_128_i1_fu_main_419527_422347));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_419527_420315 (.out1(out_read_cond_FU_75_i0_fu_main_419527_420315), .in1(out_reg_31_reg_31));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(32), .BITSIZE_out1(32), .LSB_PARAMETER(2)) fu_main_419527_420329 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i3_fu_main_419527_420329), .in1(out_reg_3_reg_3), .in2(out_reg_70_reg_70));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_main_419527_420339 (.out1(out_addr_expr_FU_92_i0_fu_main_419527_420339), .in1(out_conv_out_const_33_16_32));
  ui_plus_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420342 (.out1(out_ui_plus_expr_FU_32_32_32_137_i0_fu_main_419527_420342), .in1(out_reg_64_reg_64), .in2(out_reg_73_reg_73));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(2)) fu_main_419527_420353 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i6_fu_main_419527_420353), .in1(out_reg_2_reg_2), .in2(out_reg_29_reg_29));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(2)) fu_main_419527_420360 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i7_fu_main_419527_420360), .in1(out_reg_2_reg_2), .in2(out_reg_29_reg_29));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(17), .BITSIZE_out1(18), .LSB_PARAMETER(2)) fu_main_419527_420367 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i4_fu_main_419527_420367), .in1(out_reg_2_reg_2), .in2(out_reg_27_reg_27));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(32), .BITSIZE_out1(32), .LSB_PARAMETER(2)) fu_main_419527_420377 (.out1(out_ui_pointer_plus_expr_FU_32_32_32_151_i5_fu_main_419527_420377), .in1(out_reg_3_reg_3), .in2(out_ui_lshift_expr_FU_32_0_32_128_i5_fu_main_419527_422444));
  ui_minus_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420401 (.out1(out_ui_minus_expr_FU_32_32_32_132_i0_fu_main_419527_420401), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420403 (.out1(out_ui_bit_and_expr_FU_32_32_32_119_i0_fu_main_419527_420403), .in1(out_reg_64_reg_64), .in2(out_reg_73_reg_73));
  ui_bit_ior_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420405 (.out1(out_ui_bit_ior_expr_FU_32_32_32_122_i0_fu_main_419527_420405), .in1(out_reg_64_reg_64), .in2(out_reg_73_reg_73));
  ui_bit_xor_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420407 (.out1(out_ui_bit_xor_expr_FU_32_32_32_123_i0_fu_main_419527_420407), .in1(out_reg_64_reg_64), .in2(out_reg_73_reg_73));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_420409 (.out1(out_UUdata_converter_FU_104_i0_fu_main_419527_420409), .in1(out_UUdata_converter_FU_103_i0_fu_main_419527_420412));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_420412 (.out1(out_UUdata_converter_FU_103_i0_fu_main_419527_420412), .in1(out_lt_expr_FU_32_32_32_115_i0_fu_main_419527_422411));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_420416 (.out1(out_UUdata_converter_FU_102_i0_fu_main_419527_420416), .in1(out_UUdata_converter_FU_101_i0_fu_main_419527_420419));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_420419 (.out1(out_UUdata_converter_FU_101_i0_fu_main_419527_420419), .in1(out_ui_lt_expr_FU_32_32_32_131_i0_fu_main_419527_422423));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420422 (.out1(out_rshift_expr_FU_32_32_32_117_i0_fu_main_419527_420422), .in1(out_reg_28_reg_28), .in2(out_reg_64_reg_64));
  rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(15), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420426 (.out1(out_rshift_expr_FU_32_32_32_117_i1_fu_main_419527_420426), .in1(out_reg_28_reg_28), .in2(out_reg_26_reg_26));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420429 (.out1(out_ui_rshift_expr_FU_32_32_32_153_i0_fu_main_419527_420429), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(15), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420431 (.out1(out_ui_rshift_expr_FU_32_32_32_153_i1_fu_main_419527_420431), .in1(out_reg_73_reg_73), .in2(out_reg_26_reg_26));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420433 (.out1(out_ui_lshift_expr_FU_32_32_32_129_i0_fu_main_419527_420433), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(15), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_420435 (.out1(out_ui_lshift_expr_FU_32_32_32_129_i1_fu_main_419527_420435), .in1(out_reg_73_reg_73), .in2(out_reg_26_reg_26));
  ui_mult_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32), .PIPE_PARAMETER(0)) fu_main_419527_420437 (.out1(out_ui_mult_expr_FU_32_32_32_0_133_i0_fu_main_419527_420437), .clock(clock), .in1(out_reg_65_reg_65), .in2(out_reg_30_reg_30));
  ui_plus_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420439 (.out1(out_ui_plus_expr_FU_32_32_32_137_i1_fu_main_419527_420439), .in1(out_reg_64_reg_64), .in2(out_reg_79_reg_79));
  ui_bit_ior_concat_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_in3(2), .BITSIZE_out1(32), .OFFSET_PARAMETER(2)) fu_main_419527_420442 (.out1(out_ui_bit_ior_concat_expr_FU_121_i0_fu_main_419527_420442), .in1(out_ui_lshift_expr_FU_32_0_32_128_i6_fu_main_419527_422539), .in2(out_ui_bit_and_expr_FU_8_0_8_120_i0_fu_main_419527_422543), .in3(out_const_6));
  ui_plus_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420450 (.out1(out_ui_plus_expr_FU_32_32_32_137_i2_fu_main_419527_420450), .in1(out_reg_38_reg_38), .in2(out_reg_79_reg_79));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420452 (.out1(out_ui_bit_and_expr_FU_32_0_32_118_i0_fu_main_419527_420452), .in1(out_ui_plus_expr_FU_32_32_32_137_i3_fu_main_419527_420457), .in2(out_const_30));
  ui_plus_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(32)) fu_main_419527_420457 (.out1(out_ui_plus_expr_FU_32_32_32_137_i3_fu_main_419527_420457), .in1(out_reg_73_reg_73), .in2(out_reg_38_reg_38));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_main_419527_421481 (.out1(out_addr_expr_FU_93_i0_fu_main_419527_421481), .in1(out_conv_out_const_31_17_32));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(3), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421489 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_139_i0_fu_main_419527_421489), .in1(out_reg_4_reg_4), .in2(out_const_7));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(4), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421493 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_140_i0_fu_main_419527_421493), .in1(out_reg_4_reg_4), .in2(out_const_8));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(4), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421498 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_141_i0_fu_main_419527_421498), .in1(out_reg_4_reg_4), .in2(out_const_22));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421503 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_142_i0_fu_main_419527_421503), .in1(out_reg_4_reg_4), .in2(out_const_9));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421508 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_143_i0_fu_main_419527_421508), .in1(out_reg_4_reg_4), .in2(out_const_19));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421512 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_144_i0_fu_main_419527_421512), .in1(out_reg_4_reg_4), .in2(out_const_23));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421517 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_145_i0_fu_main_419527_421517), .in1(out_reg_4_reg_4), .in2(out_const_25));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421522 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_146_i0_fu_main_419527_421522), .in1(out_reg_4_reg_4), .in2(out_const_26));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421527 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_147_i0_fu_main_419527_421527), .in1(out_reg_4_reg_4), .in2(out_const_27));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(5), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421532 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_148_i0_fu_main_419527_421532), .in1(out_reg_4_reg_4), .in2(out_const_28));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(6), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421537 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_149_i0_fu_main_419527_421537), .in1(out_reg_4_reg_4), .in2(out_const_10));
  ui_pointer_plus_expr_FU #(.BITSIZE_in1(17), .BITSIZE_in2(6), .BITSIZE_out1(18), .LSB_PARAMETER(0)) fu_main_419527_421542 (.out1(out_ui_pointer_plus_expr_FU_32_0_32_150_i0_fu_main_419527_421542), .in1(out_reg_4_reg_4), .in2(out_const_17));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_421565 (.out1(out_UUdata_converter_FU_91_i0_fu_main_419527_421565), .in1(out_reg_75_reg_75));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_419527_421689 (.out1(out_read_cond_FU_112_i0_fu_main_419527_421689), .in1(out_reg_33_reg_33));
  read_cond_FU #(.BITSIZE_in1(1)) fu_main_419527_421712 (.out1(out_read_cond_FU_78_i0_fu_main_419527_421712), .in1(out_reg_21_reg_21));
  multi_read_cond_FU #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) fu_main_419527_422136 (.out1(out_multi_read_cond_FU_111_i0_fu_main_419527_422136), .in1({out_reg_62_reg_62, out_reg_53_reg_53}));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422139 (.out1(out_lut_expr_FU_28_i0_fu_main_419527_422139), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_27_i0_fu_main_419527_424155), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422142 (.out1(out_lut_expr_FU_30_i0_fu_main_419527_422142), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_29_i0_fu_main_419527_424159), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422145 (.out1(out_lut_expr_FU_32_i0_fu_main_419527_422145), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_31_i0_fu_main_419527_424163), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422148 (.out1(out_lut_expr_FU_34_i0_fu_main_419527_422148), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_33_i0_fu_main_419527_424167), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422151 (.out1(out_lut_expr_FU_35_i0_fu_main_419527_422151), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_27_i0_fu_main_419527_424155), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422154 (.out1(out_lut_expr_FU_36_i0_fu_main_419527_422154), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_29_i0_fu_main_419527_424159), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422157 (.out1(out_lut_expr_FU_37_i0_fu_main_419527_422157), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_31_i0_fu_main_419527_424163), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422160 (.out1(out_lut_expr_FU_38_i0_fu_main_419527_422160), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_33_i0_fu_main_419527_424167), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422163 (.out1(out_lut_expr_FU_40_i0_fu_main_419527_422163), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_39_i0_fu_main_419527_424175), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422166 (.out1(out_lut_expr_FU_42_i0_fu_main_419527_422166), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_41_i0_fu_main_419527_424180), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422169 (.out1(out_lut_expr_FU_44_i0_fu_main_419527_422169), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_43_i0_fu_main_419527_424184), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_422172 (.out1(out_lut_expr_FU_46_i0_fu_main_419527_422172), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_45_i0_fu_main_419527_424188), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422175 (.out1(out_lut_expr_FU_47_i0_fu_main_419527_422175), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_39_i0_fu_main_419527_424175), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422178 (.out1(out_lut_expr_FU_48_i0_fu_main_419527_422178), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_41_i0_fu_main_419527_424180), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422181 (.out1(out_lut_expr_FU_49_i0_fu_main_419527_422181), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_43_i0_fu_main_419527_424184), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422184 (.out1(out_lut_expr_FU_50_i0_fu_main_419527_422184), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_lut_expr_FU_45_i0_fu_main_419527_424188), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(6), .BITSIZE_out1(1)) fu_main_419527_422199 (.out1(out_lut_expr_FU_52_i0_fu_main_419527_422199), .in1(out_const_10), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_lut_expr_FU_51_i0_fu_main_419527_424196), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(6), .BITSIZE_out1(1)) fu_main_419527_422202 (.out1(out_lut_expr_FU_54_i0_fu_main_419527_422202), .in1(out_const_10), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_lut_expr_FU_53_i0_fu_main_419527_424200), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(14), .BITSIZE_out1(1)) fu_main_419527_422205 (.out1(out_lut_expr_FU_56_i0_fu_main_419527_422205), .in1(out_const_15), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in5(out_lut_expr_FU_55_i0_fu_main_419527_424204), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(7), .BITSIZE_out1(1)) fu_main_419527_422208 (.out1(out_lut_expr_FU_57_i0_fu_main_419527_422208), .in1(out_const_11), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_lut_expr_FU_51_i0_fu_main_419527_424196), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(7), .BITSIZE_out1(1)) fu_main_419527_422211 (.out1(out_lut_expr_FU_58_i0_fu_main_419527_422211), .in1(out_const_11), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_lut_expr_FU_53_i0_fu_main_419527_424200), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(15), .BITSIZE_out1(1)) fu_main_419527_422214 (.out1(out_lut_expr_FU_60_i0_fu_main_419527_422214), .in1(out_const_16), .in2(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in3(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in4(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in5(out_lut_expr_FU_59_i0_fu_main_419527_424210), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  IUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422325 (.out1(out_IUdata_converter_FU_100_i0_fu_main_419527_422325), .in1(out_rshift_expr_FU_32_32_32_117_i0_fu_main_419527_420422));
  IUdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422328 (.out1(out_IUdata_converter_FU_99_i0_fu_main_419527_422328), .in1(out_rshift_expr_FU_32_32_32_117_i1_fu_main_419527_420426));
  ui_lshift_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(2), .BITSIZE_out1(8), .PRECISION(32)) fu_main_419527_422330 (.out1(out_ui_lshift_expr_FU_8_0_8_130_i0_fu_main_419527_422330), .in1(out_reg_0_reg_0), .in2(out_const_6));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422336 (.out1(out_lut_expr_FU_10_i0_fu_main_419527_422336), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_9_i0_fu_main_419527_423741), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  addr_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(17)) fu_main_419527_422340 (.out1(out_addr_expr_FU_96_i0_fu_main_419527_422340), .in1(out_conv_out_const_34_16_32));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_422342 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i0_fu_main_419527_422342), .in1(out_reg_79_reg_79), .in2(out_const_6));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422345 (.out1(out_UIdata_converter_FU_17_i0_fu_main_419527_422345), .in1(out_ui_lshift_expr_FU_32_0_32_127_i0_fu_main_419527_420294));
  ui_lshift_expr_FU #(.BITSIZE_in1(15), .BITSIZE_in2(2), .BITSIZE_out1(17), .PRECISION(32)) fu_main_419527_422347 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i1_fu_main_419527_422347), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15), .in2(out_const_6));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422353 (.out1(out_lut_expr_FU_21_i0_fu_main_419527_422353), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_20_i0_fu_main_419527_424113), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422364 (.out1(out_lut_expr_FU_84_i0_fu_main_419527_422364), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_83_i0_fu_main_419527_424064), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_422374 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i2_fu_main_419527_422374), .in1(out_ui_plus_expr_FU_32_32_32_137_i0_fu_main_419527_420342), .in2(out_const_6));
  ui_lshift_expr_FU #(.BITSIZE_in1(15), .BITSIZE_in2(2), .BITSIZE_out1(17), .PRECISION(32)) fu_main_419527_422378 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i3_fu_main_419527_422378), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_15), .in2(out_const_6));
  ui_eq_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422399 (.out1(out_ui_eq_expr_FU_32_32_32_125_i0_fu_main_419527_422399), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_ne_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422402 (.out1(out_ui_ne_expr_FU_32_32_32_135_i0_fu_main_419527_422402), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  lt_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422411 (.out1(out_lt_expr_FU_32_32_32_115_i0_fu_main_419527_422411), .in1(out_reg_28_reg_28), .in2(out_UIdata_converter_FU_98_i0_fu_main_419527_422418));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422415 (.out1(out_UIdata_converter_FU_65_i0_fu_main_419527_422415), .in1(out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422418 (.out1(out_UIdata_converter_FU_98_i0_fu_main_419527_422418), .in1(out_reg_64_reg_64));
  ge_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422420 (.out1(out_ge_expr_FU_32_32_32_113_i0_fu_main_419527_422420), .in1(out_reg_28_reg_28), .in2(out_UIdata_converter_FU_98_i0_fu_main_419527_422418));
  ui_lt_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422423 (.out1(out_ui_lt_expr_FU_32_32_32_131_i0_fu_main_419527_422423), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_ge_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(32), .BITSIZE_out1(1)) fu_main_419527_422426 (.out1(out_ui_ge_expr_FU_32_32_32_126_i0_fu_main_419527_422426), .in1(out_reg_73_reg_73), .in2(out_reg_64_reg_64));
  ui_lshift_expr_FU #(.BITSIZE_in1(15), .BITSIZE_in2(2), .BITSIZE_out1(17), .PRECISION(32)) fu_main_419527_422429 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i4_fu_main_419527_422429), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15), .in2(out_const_6));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422436 (.out1(out_lut_expr_FU_64_i0_fu_main_419527_422436), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_63_i0_fu_main_419527_424117), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_lshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_422444 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i5_fu_main_419527_422444), .in1(out_reg_76_reg_76), .in2(out_const_6));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422451 (.out1(out_lut_expr_FU_67_i0_fu_main_419527_422451), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_66_i0_fu_main_419527_424121), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_422454 (.out1(out_lut_expr_FU_69_i0_fu_main_419527_422454), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_68_i0_fu_main_419527_424125), .in3(1'b0), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_ne_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(1), .BITSIZE_out1(1)) fu_main_419527_422459 (.out1(out_ui_ne_expr_FU_32_0_32_134_i0_fu_main_419527_422459), .in1(out_reg_20_reg_20), .in2(out_const_0));
  UIdata_converter_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_422463 (.out1(out_UIdata_converter_FU_70_i0_fu_main_419527_422463), .in1(out_reg_79_reg_79));
  lt_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(12), .BITSIZE_out1(1)) fu_main_419527_422465 (.out1(out_lt_expr_FU_32_0_32_114_i0_fu_main_419527_422465), .in1(out_UIdata_converter_FU_70_i0_fu_main_419527_422463), .in2(out_const_4));
  IUdata_converter_FU #(.BITSIZE_in1(31), .BITSIZE_out1(32)) fu_main_419527_422472 (.out1(out_IUdata_converter_FU_18_i0_fu_main_419527_422472), .in1(out_rshift_expr_FU_32_0_32_116_i0_fu_main_419527_420295));
  ui_rshift_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(30), .PRECISION(32)) fu_main_419527_422531 (.out1(out_ui_rshift_expr_FU_32_0_32_152_i0_fu_main_419527_422531), .in1(out_reg_79_reg_79), .in2(out_const_6));
  ui_plus_expr_FU #(.BITSIZE_in1(30), .BITSIZE_in2(1), .BITSIZE_out1(30)) fu_main_419527_422536 (.out1(out_ui_plus_expr_FU_32_0_32_136_i0_fu_main_419527_422536), .in1(out_ui_rshift_expr_FU_32_0_32_152_i0_fu_main_419527_422531), .in2(out_const_5));
  ui_lshift_expr_FU #(.BITSIZE_in1(30), .BITSIZE_in2(2), .BITSIZE_out1(32), .PRECISION(32)) fu_main_419527_422539 (.out1(out_ui_lshift_expr_FU_32_0_32_128_i6_fu_main_419527_422539), .in1(out_ui_plus_expr_FU_32_0_32_136_i0_fu_main_419527_422536), .in2(out_const_6));
  ui_bit_and_expr_FU #(.BITSIZE_in1(32), .BITSIZE_in2(2), .BITSIZE_out1(2)) fu_main_419527_422543 (.out1(out_ui_bit_and_expr_FU_8_0_8_120_i0_fu_main_419527_422543), .in1(out_reg_79_reg_79), .in2(out_const_21));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_422576 (.out1(out_lut_expr_FU_71_i0_fu_main_419527_422576), .in1(out_const_8), .in2(out_reg_19_reg_19), .in3(out_ui_ne_expr_FU_32_0_32_134_i0_fu_main_419527_422459), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(1), .BITSIZE_out1(32)) fu_main_419527_423567 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i0_fu_main_419527_423567), .in1(out_reg_54_reg_54), .in2(out_reg_64_reg_64), .in3(out_const_0));
  lut_expr_FU #(.BITSIZE_in1(9), .BITSIZE_out1(1)) fu_main_419527_423572 (.out1(out_lut_expr_FU_61_i0_fu_main_419527_423572), .in1(out_const_13), .in2(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in3(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(10), .BITSIZE_out1(1)) fu_main_419527_423575 (.out1(out_lut_expr_FU_62_i0_fu_main_419527_423575), .in1(out_const_14), .in2(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in3(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  multi_read_cond_FU #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) fu_main_419527_423576 (.out1(out_multi_read_cond_FU_87_i0_fu_main_419527_423576), .in1({out_reg_36_reg_36, out_reg_37_reg_37}));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_423582 (.out1(out_lut_expr_FU_74_i0_fu_main_419527_423582), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_72_i0_fu_main_419527_424132), .in3(out_ui_extract_bit_expr_FU_73_i0_fu_main_419527_424140), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_423593 (.out1(out_lut_expr_FU_85_i0_fu_main_419527_423593), .in1(out_const_12), .in2(out_ui_extract_bit_expr_FU_83_i0_fu_main_419527_424064), .in3(out_reg_32_reg_32), .in4(out_reg_34_reg_34), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_423599 (.out1(out_lut_expr_FU_86_i0_fu_main_419527_423599), .in1(out_const_12), .in2(out_ui_extract_bit_expr_FU_83_i0_fu_main_419527_424064), .in3(out_reg_32_reg_32), .in4(out_reg_35_reg_35), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423603 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i1_fu_main_419527_423603), .in1(out_reg_40_reg_40), .in2(out_ui_plus_expr_FU_32_32_32_137_i0_fu_main_419527_420342), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i0_fu_main_419527_423567));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423609 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i2_fu_main_419527_423609), .in1(out_reg_41_reg_41), .in2(out_ui_minus_expr_FU_32_32_32_132_i0_fu_main_419527_420401), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i1_fu_main_419527_423603));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423615 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i3_fu_main_419527_423615), .in1(out_reg_42_reg_42), .in2(out_ui_bit_and_expr_FU_32_32_32_119_i0_fu_main_419527_420403), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i2_fu_main_419527_423609));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423621 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i4_fu_main_419527_423621), .in1(out_reg_43_reg_43), .in2(out_ui_bit_ior_expr_FU_32_32_32_122_i0_fu_main_419527_420405), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i3_fu_main_419527_423615));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423627 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i5_fu_main_419527_423627), .in1(out_reg_44_reg_44), .in2(out_ui_bit_xor_expr_FU_32_32_32_123_i0_fu_main_419527_420407), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i4_fu_main_419527_423621));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423633 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i6_fu_main_419527_423633), .in1(out_reg_45_reg_45), .in2(out_UUdata_converter_FU_104_i0_fu_main_419527_420409), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i5_fu_main_419527_423627));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423639 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i7_fu_main_419527_423639), .in1(out_reg_46_reg_46), .in2(out_UUdata_converter_FU_102_i0_fu_main_419527_420416), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i6_fu_main_419527_423633));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423645 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i8_fu_main_419527_423645), .in1(out_reg_47_reg_47), .in2(out_IUdata_converter_FU_100_i0_fu_main_419527_422325), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i7_fu_main_419527_423639));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423651 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i9_fu_main_419527_423651), .in1(out_reg_48_reg_48), .in2(out_IUdata_converter_FU_99_i0_fu_main_419527_422328), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i8_fu_main_419527_423645));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423657 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i10_fu_main_419527_423657), .in1(out_reg_49_reg_49), .in2(out_ui_rshift_expr_FU_32_32_32_153_i0_fu_main_419527_420429), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i9_fu_main_419527_423651));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423663 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i11_fu_main_419527_423663), .in1(out_reg_50_reg_50), .in2(out_ui_rshift_expr_FU_32_32_32_153_i1_fu_main_419527_420431), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i10_fu_main_419527_423657));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423669 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i12_fu_main_419527_423669), .in1(out_reg_51_reg_51), .in2(out_ui_lshift_expr_FU_32_32_32_129_i0_fu_main_419527_420433), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i11_fu_main_419527_423663));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423675 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i13_fu_main_419527_423675), .in1(out_reg_52_reg_52), .in2(out_ui_lshift_expr_FU_32_32_32_129_i1_fu_main_419527_420435), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i12_fu_main_419527_423669));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423681 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i14_fu_main_419527_423681), .in1(out_reg_55_reg_55), .in2(out_ui_plus_expr_FU_32_32_32_137_i1_fu_main_419527_420439), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i13_fu_main_419527_423675));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423687 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i15_fu_main_419527_423687), .in1(out_reg_56_reg_56), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i14_fu_main_419527_423681));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423693 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i16_fu_main_419527_423693), .in1(out_reg_57_reg_57), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i15_fu_main_419527_423687));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423699 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i17_fu_main_419527_423699), .in1(out_reg_58_reg_58), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i16_fu_main_419527_423693));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423705 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i18_fu_main_419527_423705), .in1(out_reg_59_reg_59), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i17_fu_main_419527_423699));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423711 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i19_fu_main_419527_423711), .in1(out_reg_60_reg_60), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i18_fu_main_419527_423705));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423717 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i20_fu_main_419527_423717), .in1(out_reg_61_reg_61), .in2(out_const_0), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i19_fu_main_419527_423711));
  UUdata_converter_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_423720 (.out1(out_UUdata_converter_FU_110_i0_fu_main_419527_423720), .in1(out_lut_expr_FU_109_i0_fu_main_419527_424109));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423723 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i21_fu_main_419527_423723), .in1(out_reg_63_reg_63), .in2(out_reg_18_reg_18), .in3(out_reg_71_reg_71));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423726 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i22_fu_main_419527_423726), .in1(out_lut_expr_FU_84_i0_fu_main_419527_422364), .in2(out_reg_18_reg_18), .in3(out_reg_68_reg_68));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423729 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i23_fu_main_419527_423729), .in1(out_lut_expr_FU_85_i0_fu_main_419527_423593), .in2(out_reg_18_reg_18), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i22_fu_main_419527_423726));
  ui_cond_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(32), .BITSIZE_in3(32), .BITSIZE_out1(32)) fu_main_419527_423733 (.out1(out_ui_cond_expr_FU_32_32_32_32_124_i24_fu_main_419527_423733), .in1(out_lut_expr_FU_86_i0_fu_main_419527_423599), .in2(out_reg_69_reg_69), .in3(out_ui_cond_expr_FU_32_32_32_32_124_i23_fu_main_419527_423729));
  ASSIGN_UNSIGNED_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) fu_main_419527_423736 (.out1(out_ASSIGN_UNSIGNED_FU_19_i0_fu_main_419527_423736), .in1(out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(6), .BITSIZE_in2(3)) fu_main_419527_423741 (.out1(out_ui_extract_bit_expr_FU_9_i0_fu_main_419527_423741), .in1(out_ui_plus_expr_FU_8_0_8_138_i0_fu_main_419527_420258), .in2(out_const_18));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(1)) fu_main_419527_423749 (.out1(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in1(out_reg_39_reg_39), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(1)) fu_main_419527_423752 (.out1(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in1(out_reg_39_reg_39), .in2(out_const_5));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(2)) fu_main_419527_423758 (.out1(out_ui_extract_bit_expr_FU_24_i0_fu_main_419527_423758), .in1(out_reg_39_reg_39), .in2(out_const_6));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(2)) fu_main_419527_423761 (.out1(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in1(out_reg_39_reg_39), .in2(out_const_21));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(5), .BITSIZE_in2(3)) fu_main_419527_423764 (.out1(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in1(out_reg_39_reg_39), .in2(out_const_7));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(1), .BITSIZE_in2(1)) fu_main_419527_424064 (.out1(out_ui_extract_bit_expr_FU_83_i0_fu_main_419527_424064), .in1(out_reg_77_reg_77), .in2(out_const_0));
  lut_expr_FU #(.BITSIZE_in1(16), .BITSIZE_out1(1)) fu_main_419527_424109 (.out1(out_lut_expr_FU_109_i0_fu_main_419527_424109), .in1(out_const_29), .in2(out_ui_ge_expr_FU_32_32_32_126_i0_fu_main_419527_422426), .in3(out_reg_61_reg_61), .in4(out_reg_63_reg_63), .in5(out_lut_expr_FU_108_i0_fu_main_419527_424232), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424113 (.out1(out_ui_extract_bit_expr_FU_20_i0_fu_main_419527_424113), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424117 (.out1(out_ui_extract_bit_expr_FU_63_i0_fu_main_419527_424117), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424121 (.out1(out_ui_extract_bit_expr_FU_66_i0_fu_main_419527_424121), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424125 (.out1(out_ui_extract_bit_expr_FU_68_i0_fu_main_419527_424125), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424132 (.out1(out_ui_extract_bit_expr_FU_72_i0_fu_main_419527_424132), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0_32_2), .in2(out_const_0));
  ui_extract_bit_expr_FU #(.BITSIZE_in1(2), .BITSIZE_in2(1)) fu_main_419527_424140 (.out1(out_ui_extract_bit_expr_FU_73_i0_fu_main_419527_424140), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_2), .in2(out_const_0));
  lut_expr_FU #(.BITSIZE_in1(1), .BITSIZE_out1(1)) fu_main_419527_424155 (.out1(out_lut_expr_FU_27_i0_fu_main_419527_424155), .in1(out_const_5), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(2), .BITSIZE_out1(1)) fu_main_419527_424159 (.out1(out_lut_expr_FU_29_i0_fu_main_419527_424159), .in1(out_const_6), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_424163 (.out1(out_lut_expr_FU_31_i0_fu_main_419527_424163), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_424167 (.out1(out_lut_expr_FU_33_i0_fu_main_419527_424167), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(5), .BITSIZE_out1(1)) fu_main_419527_424175 (.out1(out_lut_expr_FU_39_i0_fu_main_419527_424175), .in1(out_const_9), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(6), .BITSIZE_out1(1)) fu_main_419527_424180 (.out1(out_lut_expr_FU_41_i0_fu_main_419527_424180), .in1(out_const_10), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(7), .BITSIZE_out1(1)) fu_main_419527_424184 (.out1(out_lut_expr_FU_43_i0_fu_main_419527_424184), .in1(out_const_11), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_424188 (.out1(out_lut_expr_FU_45_i0_fu_main_419527_424188), .in1(out_const_12), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_25_i0_fu_main_419527_423761), .in5(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(5), .BITSIZE_out1(1)) fu_main_419527_424196 (.out1(out_lut_expr_FU_51_i0_fu_main_419527_424196), .in1(out_const_9), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(6), .BITSIZE_out1(1)) fu_main_419527_424200 (.out1(out_lut_expr_FU_53_i0_fu_main_419527_424200), .in1(out_const_10), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(out_ui_extract_bit_expr_FU_26_i0_fu_main_419527_423764), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(4), .BITSIZE_out1(1)) fu_main_419527_424204 (.out1(out_lut_expr_FU_55_i0_fu_main_419527_424204), .in1(out_const_8), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(3), .BITSIZE_out1(1)) fu_main_419527_424210 (.out1(out_lut_expr_FU_59_i0_fu_main_419527_424210), .in1(out_const_7), .in2(out_ui_extract_bit_expr_FU_22_i0_fu_main_419527_423749), .in3(out_ui_extract_bit_expr_FU_23_i0_fu_main_419527_423752), .in4(1'b0), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(16), .BITSIZE_out1(1)) fu_main_419527_424222 (.out1(out_lut_expr_FU_105_i0_fu_main_419527_424222), .in1(out_const_24), .in2(out_ui_eq_expr_FU_32_32_32_125_i0_fu_main_419527_422399), .in3(out_ui_ne_expr_FU_32_32_32_135_i0_fu_main_419527_422402), .in4(out_reg_56_reg_56), .in5(out_reg_57_reg_57), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_424226 (.out1(out_lut_expr_FU_106_i0_fu_main_419527_424226), .in1(out_const_20), .in2(out_lt_expr_FU_32_32_32_115_i0_fu_main_419527_422411), .in3(out_reg_58_reg_58), .in4(out_lut_expr_FU_105_i0_fu_main_419527_424222), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_424229 (.out1(out_lut_expr_FU_107_i0_fu_main_419527_424229), .in1(out_const_20), .in2(out_ge_expr_FU_32_32_32_113_i0_fu_main_419527_422420), .in3(out_reg_59_reg_59), .in4(out_lut_expr_FU_106_i0_fu_main_419527_424226), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  lut_expr_FU #(.BITSIZE_in1(8), .BITSIZE_out1(1)) fu_main_419527_424232 (.out1(out_lut_expr_FU_108_i0_fu_main_419527_424232), .in1(out_const_20), .in2(out_ui_lt_expr_FU_32_32_32_131_i0_fu_main_419527_422423), .in3(out_reg_60_reg_60), .in4(out_lut_expr_FU_107_i0_fu_main_419527_424229), .in5(1'b0), .in6(1'b0), .in7(1'b0), .in8(1'b0), .in9(1'b0));
  join_signal #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) join_signalbus_mergerSout_DataRdy0_0 (.out1(sig_in_bus_mergerSout_DataRdy0_0), .in1(sig_in_vector_bus_mergerSout_DataRdy0_0));
  join_signal #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_out1(64)) join_signalbus_mergerSout_Rdata_ram1_0 (.out1(sig_in_bus_mergerSout_Rdata_ram1_0), .in1(sig_in_vector_bus_mergerSout_Rdata_ram1_0));
  join_signal #(.BITSIZE_in1(32), .PORTSIZE_in1(2), .BITSIZE_out1(64)) join_signalbus_mergerproxy_in12_0 (.out1(sig_in_bus_mergerproxy_in12_0), .in1(sig_in_vector_bus_mergerproxy_in12_0));
  join_signal #(.BITSIZE_in1(17), .PORTSIZE_in1(2), .BITSIZE_out1(34)) join_signalbus_mergerproxy_in23_0 (.out1(sig_in_bus_mergerproxy_in23_0), .in1(sig_in_vector_bus_mergerproxy_in23_0));
  join_signal #(.BITSIZE_in1(6), .PORTSIZE_in1(2), .BITSIZE_out1(12)) join_signalbus_mergerproxy_in34_0 (.out1(sig_in_bus_mergerproxy_in34_0), .in1(sig_in_vector_bus_mergerproxy_in34_0));
  join_signal #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) join_signalbus_mergerproxy_sel_LOAD5_0 (.out1(sig_in_bus_mergerproxy_sel_LOAD5_0), .in1(sig_in_vector_bus_mergerproxy_sel_LOAD5_0));
  join_signal #(.BITSIZE_in1(1), .PORTSIZE_in1(2), .BITSIZE_out1(2)) join_signalbus_mergerproxy_sel_STORE6_0 (.out1(sig_in_bus_mergerproxy_sel_STORE6_0), .in1(sig_in_vector_bus_mergerproxy_sel_STORE6_0));
  register_SE #(.BITSIZE_in1(6), .BITSIZE_out1(6)) reg_0 (.out1(out_reg_0_reg_0), .clock(clock), .reset(reset), .in1(out_MUX_247_reg_0_0_0_0), .wenable(wrenable_reg_0));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_1 (.out1(out_reg_1_reg_1), .clock(clock), .reset(reset), .in1(out_ui_view_convert_expr_FU_95_i0_fu_main_419527_420276), .wenable(wrenable_reg_1));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_10 (.out1(out_reg_10_reg_10), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_143_i0_fu_main_419527_421508), .wenable(wrenable_reg_10));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_11 (.out1(out_reg_11_reg_11), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_144_i0_fu_main_419527_421512), .wenable(wrenable_reg_11));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_12 (.out1(out_reg_12_reg_12), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_145_i0_fu_main_419527_421517), .wenable(wrenable_reg_12));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_13 (.out1(out_reg_13_reg_13), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_146_i0_fu_main_419527_421522), .wenable(wrenable_reg_13));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_14 (.out1(out_reg_14_reg_14), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_147_i0_fu_main_419527_421527), .wenable(wrenable_reg_14));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_15 (.out1(out_reg_15_reg_15), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_148_i0_fu_main_419527_421532), .wenable(wrenable_reg_15));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_16 (.out1(out_reg_16_reg_16), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_149_i0_fu_main_419527_421537), .wenable(wrenable_reg_16));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_17 (.out1(out_reg_17_reg_17), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_150_i0_fu_main_419527_421542), .wenable(wrenable_reg_17));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_18 (.out1(out_reg_18_reg_18), .clock(clock), .reset(reset), .in1(out_ui_bit_ior_concat_expr_FU_121_i0_fu_main_419527_420442), .wenable(wrenable_reg_18));
  register_STD #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_19 (.out1(out_reg_19_reg_19), .clock(clock), .reset(reset), .in1(out_lt_expr_FU_32_0_32_114_i0_fu_main_419527_422465), .wenable(wrenable_reg_19));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_2 (.out1(out_reg_2_reg_2), .clock(clock), .reset(reset), .in1(out_addr_expr_FU_6_i0_fu_main_419527_420283), .wenable(wrenable_reg_2));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_20 (.out1(out_reg_20_reg_20), .clock(clock), .reset(reset), .in1(out_IUdata_converter_FU_18_i0_fu_main_419527_422472), .wenable(wrenable_reg_20));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_21 (.out1(out_reg_21_reg_21), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_71_i0_fu_main_419527_422576), .wenable(wrenable_reg_21));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_22 (.out1(out_reg_22_reg_22), .clock(clock), .reset(reset), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0), .wenable(wrenable_reg_22));
  register_STD #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_23 (.out1(out_reg_23_reg_23), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i2_fu_main_419527_420311), .wenable(wrenable_reg_23));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_24 (.out1(out_reg_24_reg_24), .clock(clock), .reset(reset), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0), .wenable(wrenable_reg_24));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_25 (.out1(out_reg_25_reg_25), .clock(clock), .reset(reset), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0), .wenable(wrenable_reg_25));
  register_SE #(.BITSIZE_in1(15), .BITSIZE_out1(15)) reg_26 (.out1(out_reg_26_reg_26), .clock(clock), .reset(reset), .in1(out_conv_out_ARRAY_1D_STD_BRAM_NN_0_i1_array_420170_0_32_15), .wenable(wrenable_reg_26));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_27 (.out1(out_reg_27_reg_27), .clock(clock), .reset(reset), .in1(out_ui_lshift_expr_FU_32_0_32_128_i3_fu_main_419527_422378), .wenable(wrenable_reg_27));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_28 (.out1(out_reg_28_reg_28), .clock(clock), .reset(reset), .in1(out_UIdata_converter_FU_65_i0_fu_main_419527_422415), .wenable(wrenable_reg_28));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_29 (.out1(out_reg_29_reg_29), .clock(clock), .reset(reset), .in1(out_ui_lshift_expr_FU_32_0_32_128_i4_fu_main_419527_422429), .wenable(wrenable_reg_29));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_3 (.out1(out_reg_3_reg_3), .clock(clock), .reset(reset), .in1(out_addr_expr_FU_92_i0_fu_main_419527_420339), .wenable(wrenable_reg_3));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_30 (.out1(out_reg_30_reg_30), .clock(clock), .reset(reset), .in1(out_ASSIGN_UNSIGNED_FU_19_i0_fu_main_419527_423736), .wenable(wrenable_reg_30));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_31 (.out1(out_reg_31_reg_31), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_21_i0_fu_main_419527_422353), .wenable(wrenable_reg_31));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_32 (.out1(out_reg_32_reg_32), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_67_i0_fu_main_419527_422451), .wenable(wrenable_reg_32));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_33 (.out1(out_reg_33_reg_33), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_64_i0_fu_main_419527_422436), .wenable(wrenable_reg_33));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_34 (.out1(out_reg_34_reg_34), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_69_i0_fu_main_419527_422454), .wenable(wrenable_reg_34));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_35 (.out1(out_reg_35_reg_35), .clock(clock), .reset(reset), .in1(out_ui_extract_bit_expr_FU_68_i0_fu_main_419527_424125), .wenable(wrenable_reg_35));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_36 (.out1(out_reg_36_reg_36), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_74_i0_fu_main_419527_423582), .wenable(wrenable_reg_36));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_37 (.out1(out_reg_37_reg_37), .clock(clock), .reset(reset), .in1(out_ui_extract_bit_expr_FU_72_i0_fu_main_419527_424132), .wenable(wrenable_reg_37));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_38 (.out1(out_reg_38_reg_38), .clock(clock), .reset(reset), .in1(out_ARRAY_1D_STD_BRAM_NN_0_i0_array_420170_0), .wenable(wrenable_reg_38));
  register_SE #(.BITSIZE_in1(5), .BITSIZE_out1(5)) reg_39 (.out1(out_reg_39_reg_39), .clock(clock), .reset(reset), .in1(out_conv_out_aluDecode_154_i0_fu_main_419527_420310_32_5), .wenable(wrenable_reg_39));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_4 (.out1(out_reg_4_reg_4), .clock(clock), .reset(reset), .in1(out_addr_expr_FU_93_i0_fu_main_419527_421481), .wenable(wrenable_reg_4));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_40 (.out1(out_reg_40_reg_40), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_28_i0_fu_main_419527_422139), .wenable(wrenable_reg_40));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_41 (.out1(out_reg_41_reg_41), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_30_i0_fu_main_419527_422142), .wenable(wrenable_reg_41));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_42 (.out1(out_reg_42_reg_42), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_32_i0_fu_main_419527_422145), .wenable(wrenable_reg_42));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_43 (.out1(out_reg_43_reg_43), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_34_i0_fu_main_419527_422148), .wenable(wrenable_reg_43));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_44 (.out1(out_reg_44_reg_44), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_35_i0_fu_main_419527_422151), .wenable(wrenable_reg_44));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_45 (.out1(out_reg_45_reg_45), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_36_i0_fu_main_419527_422154), .wenable(wrenable_reg_45));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_46 (.out1(out_reg_46_reg_46), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_37_i0_fu_main_419527_422157), .wenable(wrenable_reg_46));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_47 (.out1(out_reg_47_reg_47), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_38_i0_fu_main_419527_422160), .wenable(wrenable_reg_47));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_48 (.out1(out_reg_48_reg_48), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_40_i0_fu_main_419527_422163), .wenable(wrenable_reg_48));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_49 (.out1(out_reg_49_reg_49), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_42_i0_fu_main_419527_422166), .wenable(wrenable_reg_49));
  register_SE #(.BITSIZE_in1(17), .BITSIZE_out1(17)) reg_5 (.out1(out_reg_5_reg_5), .clock(clock), .reset(reset), .in1(out_addr_expr_FU_96_i0_fu_main_419527_422340), .wenable(wrenable_reg_5));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_50 (.out1(out_reg_50_reg_50), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_44_i0_fu_main_419527_422169), .wenable(wrenable_reg_50));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_51 (.out1(out_reg_51_reg_51), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_46_i0_fu_main_419527_422172), .wenable(wrenable_reg_51));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_52 (.out1(out_reg_52_reg_52), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_47_i0_fu_main_419527_422175), .wenable(wrenable_reg_52));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_53 (.out1(out_reg_53_reg_53), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_48_i0_fu_main_419527_422178), .wenable(wrenable_reg_53));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_54 (.out1(out_reg_54_reg_54), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_49_i0_fu_main_419527_422181), .wenable(wrenable_reg_54));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_55 (.out1(out_reg_55_reg_55), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_50_i0_fu_main_419527_422184), .wenable(wrenable_reg_55));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_56 (.out1(out_reg_56_reg_56), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_52_i0_fu_main_419527_422199), .wenable(wrenable_reg_56));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_57 (.out1(out_reg_57_reg_57), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_54_i0_fu_main_419527_422202), .wenable(wrenable_reg_57));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_58 (.out1(out_reg_58_reg_58), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_56_i0_fu_main_419527_422205), .wenable(wrenable_reg_58));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_59 (.out1(out_reg_59_reg_59), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_57_i0_fu_main_419527_422208), .wenable(wrenable_reg_59));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_6 (.out1(out_reg_6_reg_6), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_139_i0_fu_main_419527_421489), .wenable(wrenable_reg_6));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_60 (.out1(out_reg_60_reg_60), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_58_i0_fu_main_419527_422211), .wenable(wrenable_reg_60));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_61 (.out1(out_reg_61_reg_61), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_60_i0_fu_main_419527_422214), .wenable(wrenable_reg_61));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_62 (.out1(out_reg_62_reg_62), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_61_i0_fu_main_419527_423572), .wenable(wrenable_reg_62));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_63 (.out1(out_reg_63_reg_63), .clock(clock), .reset(reset), .in1(out_lut_expr_FU_62_i0_fu_main_419527_423575), .wenable(wrenable_reg_63));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_64 (.out1(out_reg_64_reg_64), .clock(clock), .reset(reset), .in1(out_MUX_308_reg_64_0_0_0), .wenable(wrenable_reg_64));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_65 (.out1(out_reg_65_reg_65), .clock(clock), .reset(reset), .in1(out_MUX_309_reg_65_0_0_0), .wenable(wrenable_reg_65));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_66 (.out1(out_reg_66_reg_66), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i7_fu_main_419527_420360), .wenable(wrenable_reg_66));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_67 (.out1(out_reg_67_reg_67), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i4_fu_main_419527_420367), .wenable(wrenable_reg_67));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_68 (.out1(out_reg_68_reg_68), .clock(clock), .reset(reset), .in1(out_ui_plus_expr_FU_32_32_32_137_i2_fu_main_419527_420450), .wenable(wrenable_reg_68));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_69 (.out1(out_reg_69_reg_69), .clock(clock), .reset(reset), .in1(out_ui_bit_and_expr_FU_32_0_32_118_i0_fu_main_419527_420452), .wenable(wrenable_reg_69));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_7 (.out1(out_reg_7_reg_7), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_140_i0_fu_main_419527_421493), .wenable(wrenable_reg_7));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_70 (.out1(out_reg_70_reg_70), .clock(clock), .reset(reset), .in1(out_ui_lshift_expr_FU_32_0_32_128_i2_fu_main_419527_422374), .wenable(wrenable_reg_70));
  register_STD #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_71 (.out1(out_reg_71_reg_71), .clock(clock), .reset(reset), .in1(out_ui_cond_expr_FU_32_32_32_32_124_i20_fu_main_419527_423717), .wenable(wrenable_reg_71));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_72 (.out1(out_reg_72_reg_72), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i3_fu_main_419527_420329), .wenable(wrenable_reg_72));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_73 (.out1(out_reg_73_reg_73), .clock(clock), .reset(reset), .in1(out_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_array_420270_0), .wenable(wrenable_reg_73));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_74 (.out1(out_reg_74_reg_74), .clock(clock), .reset(reset), .in1(out_MUX_319_reg_74_0_0_0), .wenable(wrenable_reg_74));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_75 (.out1(out_reg_75_reg_75), .clock(clock), .reset(reset), .in1(out_MUX_320_reg_75_0_0_0), .wenable(wrenable_reg_75));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_76 (.out1(out_reg_76_reg_76), .clock(clock), .reset(reset), .in1(out_MUX_321_reg_76_0_0_0), .wenable(wrenable_reg_76));
  register_SE #(.BITSIZE_in1(1), .BITSIZE_out1(1)) reg_77 (.out1(out_reg_77_reg_77), .clock(clock), .reset(reset), .in1(out_MUX_322_reg_77_0_0_0), .wenable(wrenable_reg_77));
  register_STD #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_78 (.out1(out_reg_78_reg_78), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_32_32_151_i5_fu_main_419527_420377), .wenable(wrenable_reg_78));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_79 (.out1(out_reg_79_reg_79), .clock(clock), .reset(reset), .in1(out_MUX_324_reg_79_0_0_0), .wenable(wrenable_reg_79));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_8 (.out1(out_reg_8_reg_8), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_141_i0_fu_main_419527_421498), .wenable(wrenable_reg_8));
  register_SE #(.BITSIZE_in1(32), .BITSIZE_out1(32)) reg_80 (.out1(out_reg_80_reg_80), .clock(clock), .reset(reset), .in1(out_MUX_326_reg_80_0_0_0), .wenable(wrenable_reg_80));
  register_SE #(.BITSIZE_in1(18), .BITSIZE_out1(18)) reg_9 (.out1(out_reg_9_reg_9), .clock(clock), .reset(reset), .in1(out_ui_pointer_plus_expr_FU_32_0_32_142_i0_fu_main_419527_421503), .wenable(wrenable_reg_9));
  split_signal #(.BITSIZE_in1(2), .BITSIZE_out1(1), .PORTSIZE_out1(2)) split_signalbus_mergerSout_DataRdy0_ (.out1(Sout_DataRdy), .in1(sig_out_bus_mergerSout_DataRdy0_));
  split_signal #(.BITSIZE_in1(64), .BITSIZE_out1(32), .PORTSIZE_out1(2)) split_signalbus_mergerSout_Rdata_ram1_ (.out1(Sout_Rdata_ram), .in1(sig_out_bus_mergerSout_Rdata_ram1_));
  split_signal #(.BITSIZE_in1(64), .BITSIZE_out1(32), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in12_ (.out1(sig_out_vector_bus_mergerproxy_in12_), .in1(sig_out_bus_mergerproxy_in12_));
  split_signal #(.BITSIZE_in1(34), .BITSIZE_out1(17), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in23_ (.out1(sig_out_vector_bus_mergerproxy_in23_), .in1(sig_out_bus_mergerproxy_in23_));
  split_signal #(.BITSIZE_in1(12), .BITSIZE_out1(6), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_in34_ (.out1(sig_out_vector_bus_mergerproxy_in34_), .in1(sig_out_bus_mergerproxy_in34_));
  split_signal #(.BITSIZE_in1(2), .BITSIZE_out1(1), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_sel_LOAD5_ (.out1(sig_out_vector_bus_mergerproxy_sel_LOAD5_), .in1(sig_out_bus_mergerproxy_sel_LOAD5_));
  split_signal #(.BITSIZE_in1(2), .BITSIZE_out1(1), .PORTSIZE_out1(2)) split_signalbus_mergerproxy_sel_STORE6_ (.out1(sig_out_vector_bus_mergerproxy_sel_STORE6_), .in1(sig_out_bus_mergerproxy_sel_STORE6_));
  // io-signal post fix
  assign return_port = out_conv_out_const_0_1_32;
  assign OUT_CONDITION_main_419527_420260 = out_read_cond_FU_11_i0_fu_main_419527_420260;
  assign OUT_CONDITION_main_419527_420315 = out_read_cond_FU_75_i0_fu_main_419527_420315;
  assign OUT_CONDITION_main_419527_421689 = out_read_cond_FU_112_i0_fu_main_419527_421689;
  assign OUT_CONDITION_main_419527_421712 = out_read_cond_FU_78_i0_fu_main_419527_421712;
  assign OUT_MULTIIF_main_419527_422136 = out_multi_read_cond_FU_111_i0_fu_main_419527_422136;
  assign OUT_MULTIIF_main_419527_423576 = out_multi_read_cond_FU_87_i0_fu_main_419527_423576;
  assign OUT_UNBOUNDED_main_419527_420296 = s_done_fu_main_419527_420296;
  assign OUT_UNBOUNDED_main_419527_420310 = s_done_fu_main_419527_420310;

endmodule

// FSM based controller description for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module controller_main(done_port, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD, fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD, fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE, selector_IN_UNBOUNDED_main_419527_420296, selector_IN_UNBOUNDED_main_419527_420310, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0, selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1, selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0, selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0, selector_MUX_247_reg_0_0_0_0, selector_MUX_308_reg_64_0_0_0, selector_MUX_309_reg_65_0_0_0, selector_MUX_319_reg_74_0_0_0, selector_MUX_320_reg_75_0_0_0, selector_MUX_321_reg_76_0_0_0, selector_MUX_322_reg_77_0_0_0, selector_MUX_324_reg_79_0_0_0, selector_MUX_326_reg_80_0_0_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0, selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1, selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0, selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0, selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1, wrenable_reg_0, wrenable_reg_1, wrenable_reg_10, wrenable_reg_11, wrenable_reg_12, wrenable_reg_13, wrenable_reg_14, wrenable_reg_15, wrenable_reg_16, wrenable_reg_17, wrenable_reg_18, wrenable_reg_19, wrenable_reg_2, wrenable_reg_20, wrenable_reg_21, wrenable_reg_22, wrenable_reg_23, wrenable_reg_24, wrenable_reg_25, wrenable_reg_26, wrenable_reg_27, wrenable_reg_28, wrenable_reg_29, wrenable_reg_3, wrenable_reg_30, wrenable_reg_31, wrenable_reg_32, wrenable_reg_33, wrenable_reg_34, wrenable_reg_35, wrenable_reg_36, wrenable_reg_37, wrenable_reg_38, wrenable_reg_39, wrenable_reg_4, wrenable_reg_40, wrenable_reg_41, wrenable_reg_42, wrenable_reg_43, wrenable_reg_44, wrenable_reg_45, wrenable_reg_46, wrenable_reg_47, wrenable_reg_48, wrenable_reg_49, wrenable_reg_5, wrenable_reg_50, wrenable_reg_51, wrenable_reg_52, wrenable_reg_53, wrenable_reg_54, wrenable_reg_55, wrenable_reg_56, wrenable_reg_57, wrenable_reg_58, wrenable_reg_59, wrenable_reg_6, wrenable_reg_60, wrenable_reg_61, wrenable_reg_62, wrenable_reg_63, wrenable_reg_64, wrenable_reg_65, wrenable_reg_66, wrenable_reg_67, wrenable_reg_68, wrenable_reg_69, wrenable_reg_7, wrenable_reg_70, wrenable_reg_71, wrenable_reg_72, wrenable_reg_73, wrenable_reg_74, wrenable_reg_75, wrenable_reg_76, wrenable_reg_77, wrenable_reg_78, wrenable_reg_79, wrenable_reg_8, wrenable_reg_80, wrenable_reg_9, OUT_CONDITION_main_419527_420260, OUT_CONDITION_main_419527_420315, OUT_CONDITION_main_419527_421689, OUT_CONDITION_main_419527_421712, OUT_MULTIIF_main_419527_422136, OUT_MULTIIF_main_419527_423576, OUT_UNBOUNDED_main_419527_420296, OUT_UNBOUNDED_main_419527_420310, clock, reset, start_port);
  // IN
  input OUT_CONDITION_main_419527_420260;
  input OUT_CONDITION_main_419527_420315;
  input OUT_CONDITION_main_419527_421689;
  input OUT_CONDITION_main_419527_421712;
  input [1:0] OUT_MULTIIF_main_419527_422136;
  input [1:0] OUT_MULTIIF_main_419527_423576;
  input OUT_UNBOUNDED_main_419527_420296;
  input OUT_UNBOUNDED_main_419527_420310;
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE;
  output fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD;
  output fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  output fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  output selector_IN_UNBOUNDED_main_419527_420296;
  output selector_IN_UNBOUNDED_main_419527_420310;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0;
  output selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1;
  output selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0;
  output selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0;
  output selector_MUX_247_reg_0_0_0_0;
  output selector_MUX_308_reg_64_0_0_0;
  output selector_MUX_309_reg_65_0_0_0;
  output selector_MUX_319_reg_74_0_0_0;
  output selector_MUX_320_reg_75_0_0_0;
  output selector_MUX_321_reg_76_0_0_0;
  output selector_MUX_322_reg_77_0_0_0;
  output selector_MUX_324_reg_79_0_0_0;
  output selector_MUX_326_reg_80_0_0_0;
  output selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0;
  output selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1;
  output selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2;
  output selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0;
  output selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1;
  output selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0;
  output selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0;
  output selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0;
  output selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1;
  output selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2;
  output selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0;
  output selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1;
  output wrenable_reg_0;
  output wrenable_reg_1;
  output wrenable_reg_10;
  output wrenable_reg_11;
  output wrenable_reg_12;
  output wrenable_reg_13;
  output wrenable_reg_14;
  output wrenable_reg_15;
  output wrenable_reg_16;
  output wrenable_reg_17;
  output wrenable_reg_18;
  output wrenable_reg_19;
  output wrenable_reg_2;
  output wrenable_reg_20;
  output wrenable_reg_21;
  output wrenable_reg_22;
  output wrenable_reg_23;
  output wrenable_reg_24;
  output wrenable_reg_25;
  output wrenable_reg_26;
  output wrenable_reg_27;
  output wrenable_reg_28;
  output wrenable_reg_29;
  output wrenable_reg_3;
  output wrenable_reg_30;
  output wrenable_reg_31;
  output wrenable_reg_32;
  output wrenable_reg_33;
  output wrenable_reg_34;
  output wrenable_reg_35;
  output wrenable_reg_36;
  output wrenable_reg_37;
  output wrenable_reg_38;
  output wrenable_reg_39;
  output wrenable_reg_4;
  output wrenable_reg_40;
  output wrenable_reg_41;
  output wrenable_reg_42;
  output wrenable_reg_43;
  output wrenable_reg_44;
  output wrenable_reg_45;
  output wrenable_reg_46;
  output wrenable_reg_47;
  output wrenable_reg_48;
  output wrenable_reg_49;
  output wrenable_reg_5;
  output wrenable_reg_50;
  output wrenable_reg_51;
  output wrenable_reg_52;
  output wrenable_reg_53;
  output wrenable_reg_54;
  output wrenable_reg_55;
  output wrenable_reg_56;
  output wrenable_reg_57;
  output wrenable_reg_58;
  output wrenable_reg_59;
  output wrenable_reg_6;
  output wrenable_reg_60;
  output wrenable_reg_61;
  output wrenable_reg_62;
  output wrenable_reg_63;
  output wrenable_reg_64;
  output wrenable_reg_65;
  output wrenable_reg_66;
  output wrenable_reg_67;
  output wrenable_reg_68;
  output wrenable_reg_69;
  output wrenable_reg_7;
  output wrenable_reg_70;
  output wrenable_reg_71;
  output wrenable_reg_72;
  output wrenable_reg_73;
  output wrenable_reg_74;
  output wrenable_reg_75;
  output wrenable_reg_76;
  output wrenable_reg_77;
  output wrenable_reg_78;
  output wrenable_reg_79;
  output wrenable_reg_8;
  output wrenable_reg_80;
  output wrenable_reg_9;
  parameter [4:0] S_22 = 5'd22,
    S_0 = 5'd0,
    S_23 = 5'd23,
    S_1 = 5'd1,
    S_2 = 5'd2,
    S_3 = 5'd3,
    S_4 = 5'd4,
    S_5 = 5'd5,
    S_6 = 5'd6,
    S_7 = 5'd7,
    S_8 = 5'd8,
    S_9 = 5'd9,
    S_10 = 5'd10,
    S_11 = 5'd11,
    S_12 = 5'd12,
    S_13 = 5'd13,
    S_24 = 5'd24,
    S_25 = 5'd25,
    S_26 = 5'd26,
    S_27 = 5'd27,
    S_28 = 5'd28,
    S_17 = 5'd17,
    S_18 = 5'd18,
    S_19 = 5'd19,
    S_21 = 5'd21,
    S_16 = 5'd16,
    S_15 = 5'd15,
    S_29 = 5'd29,
    S_30 = 5'd30,
    S_20 = 5'd20,
    S_14 = 5'd14,
    S_31 = 5'd31;
  reg [4:0] _present_state, _next_state;
  reg done_port;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  reg fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  reg selector_IN_UNBOUNDED_main_419527_420296;
  reg selector_IN_UNBOUNDED_main_419527_420310;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0;
  reg selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1;
  reg selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0;
  reg selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0;
  reg selector_MUX_247_reg_0_0_0_0;
  reg selector_MUX_308_reg_64_0_0_0;
  reg selector_MUX_309_reg_65_0_0_0;
  reg selector_MUX_319_reg_74_0_0_0;
  reg selector_MUX_320_reg_75_0_0_0;
  reg selector_MUX_321_reg_76_0_0_0;
  reg selector_MUX_322_reg_77_0_0_0;
  reg selector_MUX_324_reg_79_0_0_0;
  reg selector_MUX_326_reg_80_0_0_0;
  reg selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0;
  reg selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1;
  reg selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2;
  reg selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0;
  reg selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1;
  reg selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0;
  reg selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0;
  reg selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0;
  reg selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1;
  reg selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2;
  reg selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0;
  reg selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1;
  reg wrenable_reg_0;
  reg wrenable_reg_1;
  reg wrenable_reg_10;
  reg wrenable_reg_11;
  reg wrenable_reg_12;
  reg wrenable_reg_13;
  reg wrenable_reg_14;
  reg wrenable_reg_15;
  reg wrenable_reg_16;
  reg wrenable_reg_17;
  reg wrenable_reg_18;
  reg wrenable_reg_19;
  reg wrenable_reg_2;
  reg wrenable_reg_20;
  reg wrenable_reg_21;
  reg wrenable_reg_22;
  reg wrenable_reg_23;
  reg wrenable_reg_24;
  reg wrenable_reg_25;
  reg wrenable_reg_26;
  reg wrenable_reg_27;
  reg wrenable_reg_28;
  reg wrenable_reg_29;
  reg wrenable_reg_3;
  reg wrenable_reg_30;
  reg wrenable_reg_31;
  reg wrenable_reg_32;
  reg wrenable_reg_33;
  reg wrenable_reg_34;
  reg wrenable_reg_35;
  reg wrenable_reg_36;
  reg wrenable_reg_37;
  reg wrenable_reg_38;
  reg wrenable_reg_39;
  reg wrenable_reg_4;
  reg wrenable_reg_40;
  reg wrenable_reg_41;
  reg wrenable_reg_42;
  reg wrenable_reg_43;
  reg wrenable_reg_44;
  reg wrenable_reg_45;
  reg wrenable_reg_46;
  reg wrenable_reg_47;
  reg wrenable_reg_48;
  reg wrenable_reg_49;
  reg wrenable_reg_5;
  reg wrenable_reg_50;
  reg wrenable_reg_51;
  reg wrenable_reg_52;
  reg wrenable_reg_53;
  reg wrenable_reg_54;
  reg wrenable_reg_55;
  reg wrenable_reg_56;
  reg wrenable_reg_57;
  reg wrenable_reg_58;
  reg wrenable_reg_59;
  reg wrenable_reg_6;
  reg wrenable_reg_60;
  reg wrenable_reg_61;
  reg wrenable_reg_62;
  reg wrenable_reg_63;
  reg wrenable_reg_64;
  reg wrenable_reg_65;
  reg wrenable_reg_66;
  reg wrenable_reg_67;
  reg wrenable_reg_68;
  reg wrenable_reg_69;
  reg wrenable_reg_7;
  reg wrenable_reg_70;
  reg wrenable_reg_71;
  reg wrenable_reg_72;
  reg wrenable_reg_73;
  reg wrenable_reg_74;
  reg wrenable_reg_75;
  reg wrenable_reg_76;
  reg wrenable_reg_77;
  reg wrenable_reg_78;
  reg wrenable_reg_79;
  reg wrenable_reg_8;
  reg wrenable_reg_80;
  reg wrenable_reg_9;
  
  always @(posedge clock)
    if (reset == 1'b0) _present_state <= S_22;
    else _present_state <= _next_state;
  
  always @(*)
  begin
    done_port = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD = 1'b0;
    fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE = 1'b0;
    selector_IN_UNBOUNDED_main_419527_420296 = 1'b0;
    selector_IN_UNBOUNDED_main_419527_420310 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 = 1'b0;
    selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'b0;
    selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0 = 1'b0;
    selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'b0;
    selector_MUX_247_reg_0_0_0_0 = 1'b0;
    selector_MUX_308_reg_64_0_0_0 = 1'b0;
    selector_MUX_309_reg_65_0_0_0 = 1'b0;
    selector_MUX_319_reg_74_0_0_0 = 1'b0;
    selector_MUX_320_reg_75_0_0_0 = 1'b0;
    selector_MUX_321_reg_76_0_0_0 = 1'b0;
    selector_MUX_322_reg_77_0_0_0 = 1'b0;
    selector_MUX_324_reg_79_0_0_0 = 1'b0;
    selector_MUX_326_reg_80_0_0_0 = 1'b0;
    selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0 = 1'b0;
    selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1 = 1'b0;
    selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2 = 1'b0;
    selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 = 1'b0;
    selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 = 1'b0;
    selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'b0;
    selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 = 1'b0;
    selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0 = 1'b0;
    selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1 = 1'b0;
    selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2 = 1'b0;
    selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 = 1'b0;
    selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 = 1'b0;
    wrenable_reg_0 = 1'b0;
    wrenable_reg_1 = 1'b0;
    wrenable_reg_10 = 1'b0;
    wrenable_reg_11 = 1'b0;
    wrenable_reg_12 = 1'b0;
    wrenable_reg_13 = 1'b0;
    wrenable_reg_14 = 1'b0;
    wrenable_reg_15 = 1'b0;
    wrenable_reg_16 = 1'b0;
    wrenable_reg_17 = 1'b0;
    wrenable_reg_18 = 1'b0;
    wrenable_reg_19 = 1'b0;
    wrenable_reg_2 = 1'b0;
    wrenable_reg_20 = 1'b0;
    wrenable_reg_21 = 1'b0;
    wrenable_reg_22 = 1'b0;
    wrenable_reg_23 = 1'b0;
    wrenable_reg_24 = 1'b0;
    wrenable_reg_25 = 1'b0;
    wrenable_reg_26 = 1'b0;
    wrenable_reg_27 = 1'b0;
    wrenable_reg_28 = 1'b0;
    wrenable_reg_29 = 1'b0;
    wrenable_reg_3 = 1'b0;
    wrenable_reg_30 = 1'b0;
    wrenable_reg_31 = 1'b0;
    wrenable_reg_32 = 1'b0;
    wrenable_reg_33 = 1'b0;
    wrenable_reg_34 = 1'b0;
    wrenable_reg_35 = 1'b0;
    wrenable_reg_36 = 1'b0;
    wrenable_reg_37 = 1'b0;
    wrenable_reg_38 = 1'b0;
    wrenable_reg_39 = 1'b0;
    wrenable_reg_4 = 1'b0;
    wrenable_reg_40 = 1'b0;
    wrenable_reg_41 = 1'b0;
    wrenable_reg_42 = 1'b0;
    wrenable_reg_43 = 1'b0;
    wrenable_reg_44 = 1'b0;
    wrenable_reg_45 = 1'b0;
    wrenable_reg_46 = 1'b0;
    wrenable_reg_47 = 1'b0;
    wrenable_reg_48 = 1'b0;
    wrenable_reg_49 = 1'b0;
    wrenable_reg_5 = 1'b0;
    wrenable_reg_50 = 1'b0;
    wrenable_reg_51 = 1'b0;
    wrenable_reg_52 = 1'b0;
    wrenable_reg_53 = 1'b0;
    wrenable_reg_54 = 1'b0;
    wrenable_reg_55 = 1'b0;
    wrenable_reg_56 = 1'b0;
    wrenable_reg_57 = 1'b0;
    wrenable_reg_58 = 1'b0;
    wrenable_reg_59 = 1'b0;
    wrenable_reg_6 = 1'b0;
    wrenable_reg_60 = 1'b0;
    wrenable_reg_61 = 1'b0;
    wrenable_reg_62 = 1'b0;
    wrenable_reg_63 = 1'b0;
    wrenable_reg_64 = 1'b0;
    wrenable_reg_65 = 1'b0;
    wrenable_reg_66 = 1'b0;
    wrenable_reg_67 = 1'b0;
    wrenable_reg_68 = 1'b0;
    wrenable_reg_69 = 1'b0;
    wrenable_reg_7 = 1'b0;
    wrenable_reg_70 = 1'b0;
    wrenable_reg_71 = 1'b0;
    wrenable_reg_72 = 1'b0;
    wrenable_reg_73 = 1'b0;
    wrenable_reg_74 = 1'b0;
    wrenable_reg_75 = 1'b0;
    wrenable_reg_76 = 1'b0;
    wrenable_reg_77 = 1'b0;
    wrenable_reg_78 = 1'b0;
    wrenable_reg_79 = 1'b0;
    wrenable_reg_8 = 1'b0;
    wrenable_reg_80 = 1'b0;
    wrenable_reg_9 = 1'b0;
    case (_present_state)
      S_22 :
        if(start_port == 1'b1)
        begin
          selector_MUX_247_reg_0_0_0_0 = 1'b1;
          wrenable_reg_0 = 1'b1;
          wrenable_reg_1 = 1'b1;
          wrenable_reg_2 = 1'b1;
          wrenable_reg_3 = 1'b1;
          wrenable_reg_4 = 1'b1;
          wrenable_reg_5 = 1'b1;
          _next_state = S_0;
        end
        else
        begin
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'bX;
          selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0 = 1'bX;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'bX;
          selector_MUX_247_reg_0_0_0_0 = 1'bX;
          selector_MUX_308_reg_64_0_0_0 = 1'bX;
          selector_MUX_309_reg_65_0_0_0 = 1'bX;
          selector_MUX_319_reg_74_0_0_0 = 1'bX;
          selector_MUX_320_reg_75_0_0_0 = 1'bX;
          selector_MUX_321_reg_76_0_0_0 = 1'bX;
          selector_MUX_322_reg_77_0_0_0 = 1'bX;
          selector_MUX_324_reg_79_0_0_0 = 1'bX;
          selector_MUX_326_reg_80_0_0_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 = 1'bX;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'bX;
          selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_10 = 1'bX;
          wrenable_reg_11 = 1'bX;
          wrenable_reg_12 = 1'bX;
          wrenable_reg_13 = 1'bX;
          wrenable_reg_14 = 1'bX;
          wrenable_reg_15 = 1'bX;
          wrenable_reg_16 = 1'bX;
          wrenable_reg_17 = 1'bX;
          wrenable_reg_18 = 1'bX;
          wrenable_reg_19 = 1'bX;
          wrenable_reg_2 = 1'bX;
          wrenable_reg_20 = 1'bX;
          wrenable_reg_21 = 1'bX;
          wrenable_reg_22 = 1'bX;
          wrenable_reg_23 = 1'bX;
          wrenable_reg_24 = 1'bX;
          wrenable_reg_25 = 1'bX;
          wrenable_reg_26 = 1'bX;
          wrenable_reg_27 = 1'bX;
          wrenable_reg_28 = 1'bX;
          wrenable_reg_29 = 1'bX;
          wrenable_reg_3 = 1'bX;
          wrenable_reg_30 = 1'bX;
          wrenable_reg_31 = 1'bX;
          wrenable_reg_32 = 1'bX;
          wrenable_reg_33 = 1'bX;
          wrenable_reg_34 = 1'bX;
          wrenable_reg_35 = 1'bX;
          wrenable_reg_36 = 1'bX;
          wrenable_reg_37 = 1'bX;
          wrenable_reg_38 = 1'bX;
          wrenable_reg_39 = 1'bX;
          wrenable_reg_4 = 1'bX;
          wrenable_reg_40 = 1'bX;
          wrenable_reg_41 = 1'bX;
          wrenable_reg_42 = 1'bX;
          wrenable_reg_43 = 1'bX;
          wrenable_reg_44 = 1'bX;
          wrenable_reg_45 = 1'bX;
          wrenable_reg_46 = 1'bX;
          wrenable_reg_47 = 1'bX;
          wrenable_reg_48 = 1'bX;
          wrenable_reg_49 = 1'bX;
          wrenable_reg_5 = 1'bX;
          wrenable_reg_50 = 1'bX;
          wrenable_reg_51 = 1'bX;
          wrenable_reg_52 = 1'bX;
          wrenable_reg_53 = 1'bX;
          wrenable_reg_54 = 1'bX;
          wrenable_reg_55 = 1'bX;
          wrenable_reg_56 = 1'bX;
          wrenable_reg_57 = 1'bX;
          wrenable_reg_58 = 1'bX;
          wrenable_reg_59 = 1'bX;
          wrenable_reg_6 = 1'bX;
          wrenable_reg_60 = 1'bX;
          wrenable_reg_61 = 1'bX;
          wrenable_reg_62 = 1'bX;
          wrenable_reg_63 = 1'bX;
          wrenable_reg_64 = 1'bX;
          wrenable_reg_65 = 1'bX;
          wrenable_reg_66 = 1'bX;
          wrenable_reg_67 = 1'bX;
          wrenable_reg_68 = 1'bX;
          wrenable_reg_69 = 1'bX;
          wrenable_reg_7 = 1'bX;
          wrenable_reg_70 = 1'bX;
          wrenable_reg_71 = 1'bX;
          wrenable_reg_72 = 1'bX;
          wrenable_reg_73 = 1'bX;
          wrenable_reg_74 = 1'bX;
          wrenable_reg_75 = 1'bX;
          wrenable_reg_76 = 1'bX;
          wrenable_reg_77 = 1'bX;
          wrenable_reg_78 = 1'bX;
          wrenable_reg_79 = 1'bX;
          wrenable_reg_8 = 1'bX;
          wrenable_reg_80 = 1'bX;
          wrenable_reg_9 = 1'bX;
          _next_state = S_22;
        end
      S_0 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE = 1'b1;
          selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2 = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 = 1'b1;
          wrenable_reg_0 = 1'b1;
          if (OUT_CONDITION_main_419527_420260 == 1'b1)
            begin
              _next_state = S_0;
            end
          else
            begin
              _next_state = S_23;
              wrenable_reg_0 = 1'b0;
            end
        end
      S_23 :
        begin
          selector_MUX_324_reg_79_0_0_0 = 1'b1;
          wrenable_reg_10 = 1'b1;
          wrenable_reg_11 = 1'b1;
          wrenable_reg_12 = 1'b1;
          wrenable_reg_13 = 1'b1;
          wrenable_reg_14 = 1'b1;
          wrenable_reg_15 = 1'b1;
          wrenable_reg_16 = 1'b1;
          wrenable_reg_17 = 1'b1;
          wrenable_reg_6 = 1'b1;
          wrenable_reg_7 = 1'b1;
          wrenable_reg_79 = 1'b1;
          wrenable_reg_8 = 1'b1;
          wrenable_reg_9 = 1'b1;
          _next_state = S_1;
        end
      S_1 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE = 1'b1;
          fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD = 1'b1;
          selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 = 1'b1;
          wrenable_reg_18 = 1'b1;
          wrenable_reg_19 = 1'b1;
          wrenable_reg_20 = 1'b1;
          _next_state = S_2;
        end
      S_2 :
        begin
          selector_IN_UNBOUNDED_main_419527_420296 = 1'b1;
          wrenable_reg_21 = 1'b1;
          if (OUT_UNBOUNDED_main_419527_420296 == 1'b0)
            begin
              _next_state = S_3;
            end
          else
            begin
              _next_state = S_4;
            end
        end
      S_3 :
        begin
          if (OUT_UNBOUNDED_main_419527_420296 == 1'b0)
            begin
              _next_state = S_3;
            end
          else
            begin
              _next_state = S_4;
            end
        end
      S_4 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1 = 1'b1;
          _next_state = S_5;
        end
      S_5 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3 = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 = 1'b1;
          wrenable_reg_22 = 1'b1;
          wrenable_reg_23 = 1'b1;
          _next_state = S_6;
        end
      S_6 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1 = 1'b1;
          wrenable_reg_24 = 1'b1;
          wrenable_reg_25 = 1'b1;
          _next_state = S_7;
        end
      S_7 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2 = 1'b1;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 = 1'b1;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'b1;
          wrenable_reg_26 = 1'b1;
          wrenable_reg_27 = 1'b1;
          wrenable_reg_28 = 1'b1;
          wrenable_reg_29 = 1'b1;
          wrenable_reg_30 = 1'b1;
          wrenable_reg_73 = 1'b1;
          _next_state = S_8;
        end
      S_8 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 = 1'b1;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2 = 1'b1;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 = 1'b1;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'b1;
          wrenable_reg_31 = 1'b1;
          wrenable_reg_32 = 1'b1;
          _next_state = S_9;
        end
      S_9 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'b1;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 = 1'b1;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'b1;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'b1;
          wrenable_reg_33 = 1'b1;
          wrenable_reg_34 = 1'b1;
          wrenable_reg_35 = 1'b1;
          _next_state = S_10;
        end
      S_10 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD = 1'b1;
          wrenable_reg_36 = 1'b1;
          wrenable_reg_37 = 1'b1;
          _next_state = S_11;
        end
      S_11 :
        begin
          selector_IN_UNBOUNDED_main_419527_420310 = 1'b1;
          wrenable_reg_38 = 1'b1;
          wrenable_reg_39 = 1'b1;
          if (OUT_UNBOUNDED_main_419527_420310 == 1'b0)
            begin
              _next_state = S_12;
            end
          else
            begin
              _next_state = S_13;
            end
        end
      S_12 :
        begin
          wrenable_reg_39 = 1'b1;
          if (OUT_UNBOUNDED_main_419527_420310 == 1'b0)
            begin
              _next_state = S_12;
            end
          else
            begin
              _next_state = S_13;
            end
        end
      S_13 :
        begin
          selector_MUX_308_reg_64_0_0_0 = 1'b1;
          selector_MUX_309_reg_65_0_0_0 = 1'b1;
          wrenable_reg_40 = 1'b1;
          wrenable_reg_41 = 1'b1;
          wrenable_reg_42 = 1'b1;
          wrenable_reg_43 = 1'b1;
          wrenable_reg_44 = 1'b1;
          wrenable_reg_45 = 1'b1;
          wrenable_reg_46 = 1'b1;
          wrenable_reg_47 = 1'b1;
          wrenable_reg_48 = 1'b1;
          wrenable_reg_49 = 1'b1;
          wrenable_reg_50 = 1'b1;
          wrenable_reg_51 = 1'b1;
          wrenable_reg_52 = 1'b1;
          wrenable_reg_53 = 1'b1;
          wrenable_reg_54 = 1'b1;
          wrenable_reg_55 = 1'b1;
          wrenable_reg_56 = 1'b1;
          wrenable_reg_57 = 1'b1;
          wrenable_reg_58 = 1'b1;
          wrenable_reg_59 = 1'b1;
          wrenable_reg_60 = 1'b1;
          wrenable_reg_61 = 1'b1;
          wrenable_reg_62 = 1'b1;
          wrenable_reg_63 = 1'b1;
          wrenable_reg_64 = 1'b1;
          wrenable_reg_65 = 1'b1;
          if (OUT_CONDITION_main_419527_420315 == 1'b1)
            begin
              _next_state = S_24;
              selector_MUX_308_reg_64_0_0_0 = 1'b0;
              selector_MUX_309_reg_65_0_0_0 = 1'b0;
              wrenable_reg_64 = 1'b0;
              wrenable_reg_65 = 1'b0;
            end
          else
            begin
              _next_state = S_26;
            end
        end
      S_24 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 = 1'b1;
          _next_state = S_25;
        end
      S_25 :
        begin
          wrenable_reg_64 = 1'b1;
          wrenable_reg_65 = 1'b1;
          _next_state = S_26;
        end
      S_26 :
        begin
          wrenable_reg_66 = 1'b1;
          wrenable_reg_67 = 1'b1;
          wrenable_reg_68 = 1'b1;
          wrenable_reg_69 = 1'b1;
          wrenable_reg_70 = 1'b1;
          wrenable_reg_71 = 1'b1;
          wrenable_reg_75 = 1'b1;
          wrenable_reg_76 = 1'b1;
          _next_state = S_27;
        end
      S_27 :
        begin
          selector_MUX_319_reg_74_0_0_0 = 1'b1;
          wrenable_reg_74 = 1'b1;
          if (OUT_MULTIIF_main_419527_422136[0] == 1'b1)
            begin
              _next_state = S_21;
              selector_MUX_319_reg_74_0_0_0 = 1'b0;
              wrenable_reg_74 = 1'b0;
            end
          else if (OUT_MULTIIF_main_419527_422136[1] == 1'b1)
            begin
              _next_state = S_28;
              selector_MUX_319_reg_74_0_0_0 = 1'b0;
              wrenable_reg_74 = 1'b0;
            end
          else
            begin
              _next_state = S_16;
            end
        end
      S_28 :
        begin
          selector_MUX_322_reg_77_0_0_0 = 1'b1;
          wrenable_reg_72 = 1'b1;
          wrenable_reg_77 = 1'b1;
          if (OUT_CONDITION_main_419527_421689 == 1'b1)
            begin
              _next_state = S_15;
              wrenable_reg_72 = 1'b0;
            end
          else
            begin
              _next_state = S_17;
              selector_MUX_322_reg_77_0_0_0 = 1'b0;
              wrenable_reg_77 = 1'b0;
            end
        end
      S_17 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 = 1'b1;
          _next_state = S_18;
        end
      S_18 :
        begin
          wrenable_reg_73 = 1'b1;
          _next_state = S_19;
        end
      S_19 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE = 1'b1;
          selector_MUX_322_reg_77_0_0_0 = 1'b1;
          wrenable_reg_77 = 1'b1;
          _next_state = S_15;
        end
      S_21 :
        begin
          selector_MUX_320_reg_75_0_0_0 = 1'b1;
          wrenable_reg_74 = 1'b1;
          wrenable_reg_75 = 1'b1;
          _next_state = S_16;
        end
      S_16 :
        begin
          selector_MUX_321_reg_76_0_0_0 = 1'b1;
          wrenable_reg_76 = 1'b1;
          wrenable_reg_77 = 1'b1;
          _next_state = S_15;
        end
      S_15 :
        begin
          selector_MUX_326_reg_80_0_0_0 = 1'b1;
          wrenable_reg_78 = 1'b1;
          wrenable_reg_79 = 1'b1;
          wrenable_reg_80 = 1'b1;
          if (OUT_MULTIIF_main_419527_423576[0] == 1'b1)
            begin
              _next_state = S_29;
              selector_MUX_326_reg_80_0_0_0 = 1'b0;
              wrenable_reg_80 = 1'b0;
            end
          else if (OUT_MULTIIF_main_419527_423576[1] == 1'b1)
            begin
              _next_state = S_14;
              selector_MUX_326_reg_80_0_0_0 = 1'b0;
              wrenable_reg_78 = 1'b0;
              wrenable_reg_80 = 1'b0;
            end
          else
            begin
              _next_state = S_20;
              wrenable_reg_78 = 1'b0;
            end
        end
      S_29 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD = 1'b1;
          selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0 = 1'b1;
          _next_state = S_30;
        end
      S_30 :
        begin
          wrenable_reg_80 = 1'b1;
          _next_state = S_20;
        end
      S_20 :
        begin
          fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0 = 1'b1;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 = 1'b1;
          _next_state = S_14;
        end
      S_14 :
        begin
          if (OUT_CONDITION_main_419527_421712 == 1'b1)
            begin
              _next_state = S_1;
            end
          else
            begin
              _next_state = S_31;
              done_port = 1'b1;
            end
        end
      S_31 :
        begin
          _next_state = S_22;
        end
      default :
        begin
          _next_state = S_22;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0 = 1'bX;
          selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1 = 1'bX;
          selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0 = 1'bX;
          selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0 = 1'bX;
          selector_MUX_247_reg_0_0_0_0 = 1'bX;
          selector_MUX_308_reg_64_0_0_0 = 1'bX;
          selector_MUX_309_reg_65_0_0_0 = 1'bX;
          selector_MUX_319_reg_74_0_0_0 = 1'bX;
          selector_MUX_320_reg_75_0_0_0 = 1'bX;
          selector_MUX_321_reg_76_0_0_0 = 1'bX;
          selector_MUX_322_reg_77_0_0_0 = 1'bX;
          selector_MUX_324_reg_79_0_0_0 = 1'bX;
          selector_MUX_326_reg_80_0_0_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0 = 1'bX;
          selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1 = 1'bX;
          selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0 = 1'bX;
          selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0 = 1'bX;
          selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1 = 1'bX;
          wrenable_reg_0 = 1'bX;
          wrenable_reg_1 = 1'bX;
          wrenable_reg_10 = 1'bX;
          wrenable_reg_11 = 1'bX;
          wrenable_reg_12 = 1'bX;
          wrenable_reg_13 = 1'bX;
          wrenable_reg_14 = 1'bX;
          wrenable_reg_15 = 1'bX;
          wrenable_reg_16 = 1'bX;
          wrenable_reg_17 = 1'bX;
          wrenable_reg_18 = 1'bX;
          wrenable_reg_19 = 1'bX;
          wrenable_reg_2 = 1'bX;
          wrenable_reg_20 = 1'bX;
          wrenable_reg_21 = 1'bX;
          wrenable_reg_22 = 1'bX;
          wrenable_reg_23 = 1'bX;
          wrenable_reg_24 = 1'bX;
          wrenable_reg_25 = 1'bX;
          wrenable_reg_26 = 1'bX;
          wrenable_reg_27 = 1'bX;
          wrenable_reg_28 = 1'bX;
          wrenable_reg_29 = 1'bX;
          wrenable_reg_3 = 1'bX;
          wrenable_reg_30 = 1'bX;
          wrenable_reg_31 = 1'bX;
          wrenable_reg_32 = 1'bX;
          wrenable_reg_33 = 1'bX;
          wrenable_reg_34 = 1'bX;
          wrenable_reg_35 = 1'bX;
          wrenable_reg_36 = 1'bX;
          wrenable_reg_37 = 1'bX;
          wrenable_reg_38 = 1'bX;
          wrenable_reg_39 = 1'bX;
          wrenable_reg_4 = 1'bX;
          wrenable_reg_40 = 1'bX;
          wrenable_reg_41 = 1'bX;
          wrenable_reg_42 = 1'bX;
          wrenable_reg_43 = 1'bX;
          wrenable_reg_44 = 1'bX;
          wrenable_reg_45 = 1'bX;
          wrenable_reg_46 = 1'bX;
          wrenable_reg_47 = 1'bX;
          wrenable_reg_48 = 1'bX;
          wrenable_reg_49 = 1'bX;
          wrenable_reg_5 = 1'bX;
          wrenable_reg_50 = 1'bX;
          wrenable_reg_51 = 1'bX;
          wrenable_reg_52 = 1'bX;
          wrenable_reg_53 = 1'bX;
          wrenable_reg_54 = 1'bX;
          wrenable_reg_55 = 1'bX;
          wrenable_reg_56 = 1'bX;
          wrenable_reg_57 = 1'bX;
          wrenable_reg_58 = 1'bX;
          wrenable_reg_59 = 1'bX;
          wrenable_reg_6 = 1'bX;
          wrenable_reg_60 = 1'bX;
          wrenable_reg_61 = 1'bX;
          wrenable_reg_62 = 1'bX;
          wrenable_reg_63 = 1'bX;
          wrenable_reg_64 = 1'bX;
          wrenable_reg_65 = 1'bX;
          wrenable_reg_66 = 1'bX;
          wrenable_reg_67 = 1'bX;
          wrenable_reg_68 = 1'bX;
          wrenable_reg_69 = 1'bX;
          wrenable_reg_7 = 1'bX;
          wrenable_reg_70 = 1'bX;
          wrenable_reg_71 = 1'bX;
          wrenable_reg_72 = 1'bX;
          wrenable_reg_73 = 1'bX;
          wrenable_reg_74 = 1'bX;
          wrenable_reg_75 = 1'bX;
          wrenable_reg_76 = 1'bX;
          wrenable_reg_77 = 1'bX;
          wrenable_reg_78 = 1'bX;
          wrenable_reg_79 = 1'bX;
          wrenable_reg_8 = 1'bX;
          wrenable_reg_80 = 1'bX;
          wrenable_reg_9 = 1'bX;
        end
    endcase
  end
endmodule

// Top component for main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module _main(clock, reset, start_port, done_port, return_port, S_oe_ram, S_we_ram, S_addr_ram, S_Wdata_ram, S_data_ram_size, Sin_Rdata_ram, Sin_DataRdy, Sout_Rdata_ram, Sout_DataRdy);
  parameter MEM_var_419713_419512=32768, MEM_var_419737_419512=32768, MEM_var_420170_419527=32768, MEM_var_420270_419527=32768, MEM_var_420328_419527=32768, MEM_var_420468_419527=32768;
  // IN
  input clock;
  input reset;
  input start_port;
  input [1:0] S_oe_ram;
  input [1:0] S_we_ram;
  input [33:0] S_addr_ram;
  input [63:0] S_Wdata_ram;
  input [11:0] S_data_ram_size;
  input [63:0] Sin_Rdata_ram;
  input [1:0] Sin_DataRdy;
  // OUT
  output done_port;
  output [31:0] return_port;
  output [63:0] Sout_Rdata_ram;
  output [1:0] Sout_DataRdy;
  // Component and signal declarations
  wire OUT_CONDITION_main_419527_420260;
  wire OUT_CONDITION_main_419527_420315;
  wire OUT_CONDITION_main_419527_421689;
  wire OUT_CONDITION_main_419527_421712;
  wire [1:0] OUT_MULTIIF_main_419527_422136;
  wire [1:0] OUT_MULTIIF_main_419527_423576;
  wire OUT_UNBOUNDED_main_419527_420296;
  wire OUT_UNBOUNDED_main_419527_420310;
  wire done_delayed_REG_signal_in;
  wire done_delayed_REG_signal_out;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD;
  wire fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE;
  wire selector_IN_UNBOUNDED_main_419527_420296;
  wire selector_IN_UNBOUNDED_main_419527_420310;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0;
  wire selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1;
  wire selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0;
  wire selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0;
  wire selector_MUX_247_reg_0_0_0_0;
  wire selector_MUX_308_reg_64_0_0_0;
  wire selector_MUX_309_reg_65_0_0_0;
  wire selector_MUX_319_reg_74_0_0_0;
  wire selector_MUX_320_reg_75_0_0_0;
  wire selector_MUX_321_reg_76_0_0_0;
  wire selector_MUX_322_reg_77_0_0_0;
  wire selector_MUX_324_reg_79_0_0_0;
  wire selector_MUX_326_reg_80_0_0_0;
  wire selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0;
  wire selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1;
  wire selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2;
  wire selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0;
  wire selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1;
  wire selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0;
  wire selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0;
  wire selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0;
  wire selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1;
  wire selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2;
  wire selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0;
  wire selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1;
  wire wrenable_reg_0;
  wire wrenable_reg_1;
  wire wrenable_reg_10;
  wire wrenable_reg_11;
  wire wrenable_reg_12;
  wire wrenable_reg_13;
  wire wrenable_reg_14;
  wire wrenable_reg_15;
  wire wrenable_reg_16;
  wire wrenable_reg_17;
  wire wrenable_reg_18;
  wire wrenable_reg_19;
  wire wrenable_reg_2;
  wire wrenable_reg_20;
  wire wrenable_reg_21;
  wire wrenable_reg_22;
  wire wrenable_reg_23;
  wire wrenable_reg_24;
  wire wrenable_reg_25;
  wire wrenable_reg_26;
  wire wrenable_reg_27;
  wire wrenable_reg_28;
  wire wrenable_reg_29;
  wire wrenable_reg_3;
  wire wrenable_reg_30;
  wire wrenable_reg_31;
  wire wrenable_reg_32;
  wire wrenable_reg_33;
  wire wrenable_reg_34;
  wire wrenable_reg_35;
  wire wrenable_reg_36;
  wire wrenable_reg_37;
  wire wrenable_reg_38;
  wire wrenable_reg_39;
  wire wrenable_reg_4;
  wire wrenable_reg_40;
  wire wrenable_reg_41;
  wire wrenable_reg_42;
  wire wrenable_reg_43;
  wire wrenable_reg_44;
  wire wrenable_reg_45;
  wire wrenable_reg_46;
  wire wrenable_reg_47;
  wire wrenable_reg_48;
  wire wrenable_reg_49;
  wire wrenable_reg_5;
  wire wrenable_reg_50;
  wire wrenable_reg_51;
  wire wrenable_reg_52;
  wire wrenable_reg_53;
  wire wrenable_reg_54;
  wire wrenable_reg_55;
  wire wrenable_reg_56;
  wire wrenable_reg_57;
  wire wrenable_reg_58;
  wire wrenable_reg_59;
  wire wrenable_reg_6;
  wire wrenable_reg_60;
  wire wrenable_reg_61;
  wire wrenable_reg_62;
  wire wrenable_reg_63;
  wire wrenable_reg_64;
  wire wrenable_reg_65;
  wire wrenable_reg_66;
  wire wrenable_reg_67;
  wire wrenable_reg_68;
  wire wrenable_reg_69;
  wire wrenable_reg_7;
  wire wrenable_reg_70;
  wire wrenable_reg_71;
  wire wrenable_reg_72;
  wire wrenable_reg_73;
  wire wrenable_reg_74;
  wire wrenable_reg_75;
  wire wrenable_reg_76;
  wire wrenable_reg_77;
  wire wrenable_reg_78;
  wire wrenable_reg_79;
  wire wrenable_reg_8;
  wire wrenable_reg_80;
  wire wrenable_reg_9;
  
  controller_main Controller_i (.done_port(done_delayed_REG_signal_in), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE), .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD), .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE), .selector_IN_UNBOUNDED_main_419527_420296(selector_IN_UNBOUNDED_main_419527_420296), .selector_IN_UNBOUNDED_main_419527_420310(selector_IN_UNBOUNDED_main_419527_420310), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1), .selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0(selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0), .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0), .selector_MUX_247_reg_0_0_0_0(selector_MUX_247_reg_0_0_0_0), .selector_MUX_308_reg_64_0_0_0(selector_MUX_308_reg_64_0_0_0), .selector_MUX_309_reg_65_0_0_0(selector_MUX_309_reg_65_0_0_0), .selector_MUX_319_reg_74_0_0_0(selector_MUX_319_reg_74_0_0_0), .selector_MUX_320_reg_75_0_0_0(selector_MUX_320_reg_75_0_0_0), .selector_MUX_321_reg_76_0_0_0(selector_MUX_321_reg_76_0_0_0), .selector_MUX_322_reg_77_0_0_0(selector_MUX_322_reg_77_0_0_0), .selector_MUX_324_reg_79_0_0_0(selector_MUX_324_reg_79_0_0_0), .selector_MUX_326_reg_80_0_0_0(selector_MUX_326_reg_80_0_0_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1), .selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0), .selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0(selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_10(wrenable_reg_10), .wrenable_reg_11(wrenable_reg_11), .wrenable_reg_12(wrenable_reg_12), .wrenable_reg_13(wrenable_reg_13), .wrenable_reg_14(wrenable_reg_14), .wrenable_reg_15(wrenable_reg_15), .wrenable_reg_16(wrenable_reg_16), .wrenable_reg_17(wrenable_reg_17), .wrenable_reg_18(wrenable_reg_18), .wrenable_reg_19(wrenable_reg_19), .wrenable_reg_2(wrenable_reg_2), .wrenable_reg_20(wrenable_reg_20), .wrenable_reg_21(wrenable_reg_21), .wrenable_reg_22(wrenable_reg_22), .wrenable_reg_23(wrenable_reg_23), .wrenable_reg_24(wrenable_reg_24), .wrenable_reg_25(wrenable_reg_25), .wrenable_reg_26(wrenable_reg_26), .wrenable_reg_27(wrenable_reg_27), .wrenable_reg_28(wrenable_reg_28), .wrenable_reg_29(wrenable_reg_29), .wrenable_reg_3(wrenable_reg_3), .wrenable_reg_30(wrenable_reg_30), .wrenable_reg_31(wrenable_reg_31), .wrenable_reg_32(wrenable_reg_32), .wrenable_reg_33(wrenable_reg_33), .wrenable_reg_34(wrenable_reg_34), .wrenable_reg_35(wrenable_reg_35), .wrenable_reg_36(wrenable_reg_36), .wrenable_reg_37(wrenable_reg_37), .wrenable_reg_38(wrenable_reg_38), .wrenable_reg_39(wrenable_reg_39), .wrenable_reg_4(wrenable_reg_4), .wrenable_reg_40(wrenable_reg_40), .wrenable_reg_41(wrenable_reg_41), .wrenable_reg_42(wrenable_reg_42), .wrenable_reg_43(wrenable_reg_43), .wrenable_reg_44(wrenable_reg_44), .wrenable_reg_45(wrenable_reg_45), .wrenable_reg_46(wrenable_reg_46), .wrenable_reg_47(wrenable_reg_47), .wrenable_reg_48(wrenable_reg_48), .wrenable_reg_49(wrenable_reg_49), .wrenable_reg_5(wrenable_reg_5), .wrenable_reg_50(wrenable_reg_50), .wrenable_reg_51(wrenable_reg_51), .wrenable_reg_52(wrenable_reg_52), .wrenable_reg_53(wrenable_reg_53), .wrenable_reg_54(wrenable_reg_54), .wrenable_reg_55(wrenable_reg_55), .wrenable_reg_56(wrenable_reg_56), .wrenable_reg_57(wrenable_reg_57), .wrenable_reg_58(wrenable_reg_58), .wrenable_reg_59(wrenable_reg_59), .wrenable_reg_6(wrenable_reg_6), .wrenable_reg_60(wrenable_reg_60), .wrenable_reg_61(wrenable_reg_61), .wrenable_reg_62(wrenable_reg_62), .wrenable_reg_63(wrenable_reg_63), .wrenable_reg_64(wrenable_reg_64), .wrenable_reg_65(wrenable_reg_65), .wrenable_reg_66(wrenable_reg_66), .wrenable_reg_67(wrenable_reg_67), .wrenable_reg_68(wrenable_reg_68), .wrenable_reg_69(wrenable_reg_69), .wrenable_reg_7(wrenable_reg_7), .wrenable_reg_70(wrenable_reg_70), .wrenable_reg_71(wrenable_reg_71), .wrenable_reg_72(wrenable_reg_72), .wrenable_reg_73(wrenable_reg_73), .wrenable_reg_74(wrenable_reg_74), .wrenable_reg_75(wrenable_reg_75), .wrenable_reg_76(wrenable_reg_76), .wrenable_reg_77(wrenable_reg_77), .wrenable_reg_78(wrenable_reg_78), .wrenable_reg_79(wrenable_reg_79), .wrenable_reg_8(wrenable_reg_8), .wrenable_reg_80(wrenable_reg_80), .wrenable_reg_9(wrenable_reg_9), .OUT_CONDITION_main_419527_420260(OUT_CONDITION_main_419527_420260), .OUT_CONDITION_main_419527_420315(OUT_CONDITION_main_419527_420315), .OUT_CONDITION_main_419527_421689(OUT_CONDITION_main_419527_421689), .OUT_CONDITION_main_419527_421712(OUT_CONDITION_main_419527_421712), .OUT_MULTIIF_main_419527_422136(OUT_MULTIIF_main_419527_422136), .OUT_MULTIIF_main_419527_423576(OUT_MULTIIF_main_419527_423576), .OUT_UNBOUNDED_main_419527_420296(OUT_UNBOUNDED_main_419527_420296), .OUT_UNBOUNDED_main_419527_420310(OUT_UNBOUNDED_main_419527_420310), .clock(clock), .reset(reset), .start_port(start_port));
  datapath_main #(.MEM_var_419713_419512(MEM_var_419713_419512), .MEM_var_419737_419512(MEM_var_419737_419512), .MEM_var_420170_419527(MEM_var_420170_419527), .MEM_var_420270_419527(MEM_var_420270_419527), .MEM_var_420328_419527(MEM_var_420328_419527), .MEM_var_420468_419527(MEM_var_420468_419527)) Datapath_i (.return_port(return_port), .Sout_Rdata_ram(Sout_Rdata_ram), .Sout_DataRdy(Sout_DataRdy), .OUT_CONDITION_main_419527_420260(OUT_CONDITION_main_419527_420260), .OUT_CONDITION_main_419527_420315(OUT_CONDITION_main_419527_420315), .OUT_CONDITION_main_419527_421689(OUT_CONDITION_main_419527_421689), .OUT_CONDITION_main_419527_421712(OUT_CONDITION_main_419527_421712), .OUT_MULTIIF_main_419527_422136(OUT_MULTIIF_main_419527_422136), .OUT_MULTIIF_main_419527_423576(OUT_MULTIIF_main_419527_423576), .OUT_UNBOUNDED_main_419527_420296(OUT_UNBOUNDED_main_419527_420296), .OUT_UNBOUNDED_main_419527_420310(OUT_UNBOUNDED_main_419527_420310), .clock(clock), .reset(reset), .S_oe_ram(S_oe_ram), .S_we_ram(S_we_ram), .S_addr_ram(S_addr_ram), .S_Wdata_ram(S_Wdata_ram), .S_data_ram_size(S_data_ram_size), .Sin_Rdata_ram(Sin_Rdata_ram), .Sin_DataRdy(Sin_DataRdy), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_0_i1_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_STORE), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_LOAD), .fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE(fuselector_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_STORE), .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_LOAD), .fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE(fuselector_ARRAY_1D_STD_DISTRAM_NN_SDS_3_i0_STORE), .selector_IN_UNBOUNDED_main_419527_420296(selector_IN_UNBOUNDED_main_419527_420296), .selector_IN_UNBOUNDED_main_419527_420310(selector_IN_UNBOUNDED_main_419527_420310), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_0), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_1), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_2), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_0_3), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_0), .selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1(selector_MUX_0_ARRAY_1D_STD_BRAM_NN_0_i0_1_1_1), .selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0(selector_MUX_11_ARRAY_1D_STD_BRAM_NN_SDS_2_i0_1_0_0), .selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0(selector_MUX_1_ARRAY_1D_STD_BRAM_NN_0_i0_2_0_0), .selector_MUX_247_reg_0_0_0_0(selector_MUX_247_reg_0_0_0_0), .selector_MUX_308_reg_64_0_0_0(selector_MUX_308_reg_64_0_0_0), .selector_MUX_309_reg_65_0_0_0(selector_MUX_309_reg_65_0_0_0), .selector_MUX_319_reg_74_0_0_0(selector_MUX_319_reg_74_0_0_0), .selector_MUX_320_reg_75_0_0_0(selector_MUX_320_reg_75_0_0_0), .selector_MUX_321_reg_76_0_0_0(selector_MUX_321_reg_76_0_0_0), .selector_MUX_322_reg_77_0_0_0(selector_MUX_322_reg_77_0_0_0), .selector_MUX_324_reg_79_0_0_0(selector_MUX_324_reg_79_0_0_0), .selector_MUX_326_reg_80_0_0_0(selector_MUX_326_reg_80_0_0_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_1), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_0_2), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_0), .selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1(selector_MUX_3_ARRAY_1D_STD_BRAM_NN_0_i1_1_1_1), .selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0(selector_MUX_4_ARRAY_1D_STD_BRAM_NN_0_i1_2_0_0), .selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0(selector_MUX_6_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_0_0_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_1), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_0_2), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_0), .selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1(selector_MUX_7_ARRAY_1D_STD_BRAM_NN_SDS_1_i0_1_1_1), .wrenable_reg_0(wrenable_reg_0), .wrenable_reg_1(wrenable_reg_1), .wrenable_reg_10(wrenable_reg_10), .wrenable_reg_11(wrenable_reg_11), .wrenable_reg_12(wrenable_reg_12), .wrenable_reg_13(wrenable_reg_13), .wrenable_reg_14(wrenable_reg_14), .wrenable_reg_15(wrenable_reg_15), .wrenable_reg_16(wrenable_reg_16), .wrenable_reg_17(wrenable_reg_17), .wrenable_reg_18(wrenable_reg_18), .wrenable_reg_19(wrenable_reg_19), .wrenable_reg_2(wrenable_reg_2), .wrenable_reg_20(wrenable_reg_20), .wrenable_reg_21(wrenable_reg_21), .wrenable_reg_22(wrenable_reg_22), .wrenable_reg_23(wrenable_reg_23), .wrenable_reg_24(wrenable_reg_24), .wrenable_reg_25(wrenable_reg_25), .wrenable_reg_26(wrenable_reg_26), .wrenable_reg_27(wrenable_reg_27), .wrenable_reg_28(wrenable_reg_28), .wrenable_reg_29(wrenable_reg_29), .wrenable_reg_3(wrenable_reg_3), .wrenable_reg_30(wrenable_reg_30), .wrenable_reg_31(wrenable_reg_31), .wrenable_reg_32(wrenable_reg_32), .wrenable_reg_33(wrenable_reg_33), .wrenable_reg_34(wrenable_reg_34), .wrenable_reg_35(wrenable_reg_35), .wrenable_reg_36(wrenable_reg_36), .wrenable_reg_37(wrenable_reg_37), .wrenable_reg_38(wrenable_reg_38), .wrenable_reg_39(wrenable_reg_39), .wrenable_reg_4(wrenable_reg_4), .wrenable_reg_40(wrenable_reg_40), .wrenable_reg_41(wrenable_reg_41), .wrenable_reg_42(wrenable_reg_42), .wrenable_reg_43(wrenable_reg_43), .wrenable_reg_44(wrenable_reg_44), .wrenable_reg_45(wrenable_reg_45), .wrenable_reg_46(wrenable_reg_46), .wrenable_reg_47(wrenable_reg_47), .wrenable_reg_48(wrenable_reg_48), .wrenable_reg_49(wrenable_reg_49), .wrenable_reg_5(wrenable_reg_5), .wrenable_reg_50(wrenable_reg_50), .wrenable_reg_51(wrenable_reg_51), .wrenable_reg_52(wrenable_reg_52), .wrenable_reg_53(wrenable_reg_53), .wrenable_reg_54(wrenable_reg_54), .wrenable_reg_55(wrenable_reg_55), .wrenable_reg_56(wrenable_reg_56), .wrenable_reg_57(wrenable_reg_57), .wrenable_reg_58(wrenable_reg_58), .wrenable_reg_59(wrenable_reg_59), .wrenable_reg_6(wrenable_reg_6), .wrenable_reg_60(wrenable_reg_60), .wrenable_reg_61(wrenable_reg_61), .wrenable_reg_62(wrenable_reg_62), .wrenable_reg_63(wrenable_reg_63), .wrenable_reg_64(wrenable_reg_64), .wrenable_reg_65(wrenable_reg_65), .wrenable_reg_66(wrenable_reg_66), .wrenable_reg_67(wrenable_reg_67), .wrenable_reg_68(wrenable_reg_68), .wrenable_reg_69(wrenable_reg_69), .wrenable_reg_7(wrenable_reg_7), .wrenable_reg_70(wrenable_reg_70), .wrenable_reg_71(wrenable_reg_71), .wrenable_reg_72(wrenable_reg_72), .wrenable_reg_73(wrenable_reg_73), .wrenable_reg_74(wrenable_reg_74), .wrenable_reg_75(wrenable_reg_75), .wrenable_reg_76(wrenable_reg_76), .wrenable_reg_77(wrenable_reg_77), .wrenable_reg_78(wrenable_reg_78), .wrenable_reg_79(wrenable_reg_79), .wrenable_reg_8(wrenable_reg_8), .wrenable_reg_80(wrenable_reg_80), .wrenable_reg_9(wrenable_reg_9));
  flipflop_AR #(.BITSIZE_in1(1), .BITSIZE_out1(1)) done_delayed_REG (.out1(done_delayed_REG_signal_out), .clock(clock), .reset(reset), .in1(done_delayed_REG_signal_in));
  // io-signal post fix
  assign done_port = done_delayed_REG_signal_out;

endmodule

// Minimal interface for function: main
// This component has been derived from the input source code and so it does not fall under the copyright of PandA framework, but it follows the input source code copyright, and may be aggregated with components of the BAMBU/PANDA IP LIBRARY.
// Author(s): Component automatically generated by bambu
// License: THIS COMPONENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
`timescale 1ns / 1ps
module main(clock, reset, start_port, done_port, return_port);
  parameter MEM_var_419713_419512=32768, MEM_var_419737_419512=32768, MEM_var_420170_419527=32768, MEM_var_420270_419527=32768, MEM_var_420328_419527=32768, MEM_var_420468_419527=32768;
  // IN
  input clock;
  input reset;
  input start_port;
  // OUT
  output done_port;
  output [31:0] return_port;
  // Component and signal declarations
  wire null_out_signal__main_i0_Sout_DataRdy_0;
  wire null_out_signal__main_i0_Sout_DataRdy_1;
  wire [31:0] null_out_signal__main_i0_Sout_Rdata_ram_0;
  wire [31:0] null_out_signal__main_i0_Sout_Rdata_ram_1;
  wire [31:0] out_return_port_ui_view_convert_expr_FU;
  
  _main #(.MEM_var_419713_419512(MEM_var_419713_419512), .MEM_var_419737_419512(MEM_var_419737_419512), .MEM_var_420170_419527(MEM_var_420170_419527), .MEM_var_420270_419527(MEM_var_420270_419527), .MEM_var_420328_419527(MEM_var_420328_419527), .MEM_var_420468_419527(MEM_var_420468_419527)) _main_i0 (.done_port(done_port), .return_port(out_return_port_ui_view_convert_expr_FU), .Sout_Rdata_ram({null_out_signal__main_i0_Sout_Rdata_ram_1, null_out_signal__main_i0_Sout_Rdata_ram_0}), .Sout_DataRdy({null_out_signal__main_i0_Sout_DataRdy_1, null_out_signal__main_i0_Sout_DataRdy_0}), .clock(clock), .reset(reset), .start_port(start_port), .S_oe_ram({1'b0, 1'b0}), .S_we_ram({1'b0, 1'b0}), .S_addr_ram({17'b00000000000000000, 17'b00000000000000000}), .S_Wdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .S_data_ram_size({6'b000000, 6'b000000}), .Sin_Rdata_ram({32'b00000000000000000000000000000000, 32'b00000000000000000000000000000000}), .Sin_DataRdy({1'b0, 1'b0}));
  ui_view_convert_expr_FU #(.BITSIZE_in1(32), .BITSIZE_out1(32)) return_port_ui_view_convert_expr_FU (.out1(return_port), .in1(out_return_port_ui_view_convert_expr_FU));

endmodule


