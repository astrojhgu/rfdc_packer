import AXI4_Stream::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Types::*;
import FIFO::*;
import GetPut::*;
import Vector::*;
import BlueUtils::*;
import StmtFSM::*;
import RfdcPacker::*;
import Connectable::*;

typedef 512 AxisDataWidth;
typedef 8192 PayloadLength;

function Bit#(16) gen_data(Integer x1);
    Bit#(16) x = fromInteger(x1);
    return x;
endfunction

function Bit#(16) next_datum(Bit#(16) x);
    return x+32;
endfunction

function Vector#(32, Bit#(16)) next_data(Vector#(32, Bit#(16)) x);
    return map(next_datum, x);
endfunction




interface FakeSrc;
    (* prefix="m_axis" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_fab;
endinterface


(*synthesize*)
module mkFakeSrc(FakeSrc);
    AXI4_Stream_Wr#(512, 0) m_axis<-mkAXI4_Stream_Wr(4);
    Reg#(Int#(32)) i<-mkReg(0);
    Reg#(Vector#(32, Bit#(16))) data<-mkReg(genWith(gen_data));

    rule each;
        m_axis.pkg.put(AXI4_Stream_Pkg{
            data:pack(data), 
            user: 0,
            keep:64'hffff_ffff_ffff_ffff,
            dest: 0,
            id: 0, 
            last: False
        });
        data<=next_data(data);
    endrule
    interface AXI4_Stream_Wr_Fab m_axis_fab=m_axis.fab;
endmodule


(*synthesize*)
module mkFakeSrcWithHdr(FakeSrc);
    FakeSrc fs<-mkFakeSrc;
    RfdcPacker packer<-mkRfdcPacker(0);

    AXI4_Stream_Wr#(AxisDataWidth, 0) m_axis<-mkAXI4_Stream_Wr(2);

    mkConnection(fs.m_axis_fab, packer.s_axis_fab);

    rule cfg;
        packer.cfg.pkt_hdr(PktHdr{
            dst_mac:48'hff_ff_ff_ff_ff_ff,
            src_mac:48'h00_0a_35_00_01_02,
            dst_ip:32'h0a_64_0b_10,
            src_ip:32'h0a_64_0b_01,
            dst_port:16'h1234,
            src_port:16'h5678,
            ipv4_checksum:0
        });
    endrule

    rule each;
        let a<-packer.get.get();
        m_axis.pkg.put(a);
    endrule


    interface AXI4_Stream_Wr_Fab m_axis_fab=m_axis.fab;
endmodule


(*synthesize*)
module mkFakeSrcWithHdr2(FakeSrc);
    Vector#(2, FakeSrc) fs <- replicateM(mkFakeSrc);

    RfdcPackerN#(2) packers<-mkRfdcPackerN;

    rule cfg;
        for(Integer i=0;i<2;i=i+1)begin
            packers.cfg[i].pkt_hdr(PktHdr{
                dst_mac:48'hff_ff_ff_ff_ff_ff,
                src_mac:48'h00_0a_35_00_01_02,
                dst_ip:32'h0a_64_0b_10 + fromInteger(i),
                src_ip:32'h0a_64_0b_01,
                dst_port:16'h1234 + fromInteger(i),
                src_port:16'h5678,
                ipv4_checksum:0
            });
        end
    endrule


    for(Integer i=0;i<2;i=i+1)begin
        mkConnection(fs[i].m_axis_fab, packers.s_axis_fab[i]);
    end
    
    Reg#(UInt#(32)) step_cnt<-mkReg(0);
    
    interface AXI4_Stream_Wr_Fab m_axis_fab=packers.m_axis_fab;
endmodule
