import AXI4_Stream::*;
import AXI4_Lite_Slave::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Types::*;
import Connectable::*;
//import FIFO::*;
import FIFOF::*;
import Clocks::*;
import GetPut::*;
import Vector::*;
import BlueUtils::*;
import StmtFSM::*;
import Probe::*;

typedef 64 AxisDataWidth;
Integer axis_data_width=valueOf(AxisDataWidth);


interface AXISGateN#(numeric type nports);
    (* prefix="s_axis" *)
    interface Vector#(nports, AXI4_Stream_Rd_Fab#(AxisDataWidth, 0)) s_axis_fab;

    (* prefix="m_axis" *)
    interface Vector#(nports, AXI4_Stream_Wr_Fab#(AxisDataWidth, 0)) m_axis_fab;

    (* always_enabled, always_ready *)
    method Action pps(Bit#(1) x);

    (* always_enabled, always_ready *)
    method Action arm(Bool x);

    (* always_enabled, always_ready *)
    method Bool transmitting;

    (* always_enabled, always_ready *)
    method Bool all_incoming;

    (* always_enabled, always_ready *)
    method Bool any_outgoing;
endinterface

module mkAXISGateN(AXISGateN#(n));
    Reg#(Bit#(1)) _old_pps<-mkReg(0);
    Reg#(Bit#(1)) _pps<-mkReg(0);
    Wire#(Bool) _armed<-mkBypassWire;
    Reg#(Bool) _transmit<-mkReg(False);

    Vector#(n, FIFOF#(AXI4_Stream_Pkg#(AxisDataWidth, 0))) fifos <- replicateM(mkFIFOF);
    Vector#(n, AXI4_Stream_Wr#(AxisDataWidth, 0)) masters<-replicateM(mkAXI4_Stream_Wr(4));
    Vector#(n, AXI4_Stream_Rd#(AxisDataWidth, 0)) slaves<-replicateM(mkAXI4_Stream_Rd(4));
    Wire#(Bool) incoming <- mkDWire(False);
    Wire#(Bool) outgoing <- mkDWire(False);

    function AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) extract_axis_rd_fab(AXI4_Stream_Rd#(AxisDataWidth,0) x);
        return x.fab;
    endfunction

    function AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) extract_axis_wr_fab(AXI4_Stream_Wr#(AxisDataWidth,0) x);
        return x.fab;
    endfunction

    rule arm1;
        if (_old_pps==0 && _pps==1)begin
            _transmit<=_armed;
        end
        _old_pps<=_pps;
    endrule

    
    rule recv;
        for(Integer i=0;i!=valueOf(n);i=i+1)begin
            let data<-slaves[i].pkg.get();
            //if(_transmit) masters[i].pkg.put(data);
            //data_regs[i]<=data;
            if(fifos[i].notFull()) fifos[i].enq(data);
        end
        incoming<=True;
    endrule

    rule send;
        for(Integer i=0;i!=valueOf(n);i=i+1)begin
            let d=fifos[i].first;
            fifos[i].deq;
            if(_transmit) masters[i].pkg.put(d);
        end
        outgoing<=True;
    endrule
    



    method Action pps(Bit#(1) x);
        _pps<=x;
    endmethod

    method Action arm(Bool x);
        _armed<=x;
    endmethod

    method Bool transmitting=_transmit;
    method Bool any_outgoing=outgoing;
    method Bool all_incoming=incoming;

    interface m_axis_fab=map(extract_axis_wr_fab, masters);
    interface s_axis_fab=map(extract_axis_rd_fab, slaves);
endmodule


(*synthesize*)
module mkAXISGate8(AXISGateN#(8));
    AXISGateN#(8) gate<-mkAXISGateN;
    return gate;
endmodule