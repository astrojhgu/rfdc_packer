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
    AXI4_Stream_Wr#(512, 0) m_axis<-mkAXI4_Stream_Wr(1);
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
    RfdcPacker packer<-mkRfdcPacker;

    AXI4_Stream_Wr#(AxisDataWidth, 0) m_axis<-mkAXI4_Stream_Wr(1);

    Reg#(Bool) configured<-mkReg(False);

    mkConnection(fs.m_axis_fab, packer.s_axis_fab);

    rule cfged;
        packer.configured(pack(configured));
    endrule

    rule cfg(!configured);
        packer.set_dst_mac('h10_70_fd_b3_68_de);
        packer.set_src_mac('h10_70_fd_b3_70_df);
        packer.set_dst_ip('h0a_64_0b_01);
        packer.set_src_ip('h0a_64_0b_10);
        packer.set_dst_port('h1122);
        packer.set_src_port('h1122);
        configured<=True;
    endrule

    rule each(configured);
        let a<-packer.get.get();
        m_axis.pkg.put(a);
    endrule


    interface AXI4_Stream_Wr_Fab m_axis_fab=m_axis.fab;
endmodule


(*synthesize*)
module mkFakeSrcWithHdr2(FakeSrc);
    Vector#(2, FakeSrc) fs <- replicateM(mkFakeSrc);

    AXI4_Lite_Master_Wr#(11, 32) axi4_lite_wr<-mkAXI4_Lite_Master_Wr(2);
    AXI4_Lite_Master_Rd#(11, 32) axi4_lite_rd<-mkAXI4_Lite_Master_Rd(2);

    RfdcPackerN#(2) packers<-mkRfdcPackerN;

    AXI4_Stream_Rd#(512,0) s_axis<-mkAXI4_Stream_Rd(2);

    Reg#(Bool) configured<-mkReg(False);

    //mkConnection(fs.m_axis_fab, s_axis.fab);
    mkConnection(axi4_lite_wr.fab, packers.s_axi_wr_fab);
    mkConnection(axi4_lite_rd.fab, packers.s_axi_rd_fab);
    mkConnection(packers.m_axis_fab,s_axis.fab);

    function Stmt config_packer(Bit#(3) sel, Bit#(8) addr, Bit#(32) value);
        return seq
        axi4_lite_wr.request.put(AXI4_Lite_Write_Rq_Pkg{
            addr: {sel, addr},
            data: value, 
            strb: 4'hf,
            prot: PRIV_INSECURE_INSTRUCTION
        });
        action
            let r<-axi4_lite_wr.response.get();
            $display("resp: ",r);
        endaction
        endseq;
    endfunction

    for(Integer i=0;i<2;i=i+1)begin
        mkConnection(fs[i].m_axis_fab, packers.s_axis_fab[i]);
    end
    
    Reg#(UInt#(32)) step_cnt<-mkReg(0);
    rule cfg;
        packers.configured(pack(configured));
    endrule

    rule each(configured);
        //$display("cfg:", configured);
        //let d<-s_axis.pkg.get();
        //RfDCFrame y=unpack(d.data);
        //$display(fshow(y));
        //if(d.last)
        //$display("========");
        //step_cnt<=step_cnt+1;
        //if(step_cnt==260)$finish;
    endrule

    
    mkAutoFSM(
        seq        
            config_packer(0, 8'h00, 32'h10_70_fd_b3);
            config_packer(0, 8'h04, 32'h00_00_68_de);
            config_packer(0, 8'h08, 32'h10_70_fd_b3);
            config_packer(0, 8'h0c, 32'h00_00_68_11);

            config_packer(0, 8'h10, 32'h0a_64_0b_01);
            config_packer(0, 8'h14, 32'h0a_64_0b_10);
            config_packer(0, 8'h18, 32'h00_00_11_22);
            config_packer(0, 8'h1c, 32'h00_00_12_34);
            
            config_packer(1, 8'h00, 32'h10_70_fd_b3);
            config_packer(1, 8'h04, 32'h00_00_68_de);
            config_packer(1, 8'h08, 32'h10_70_fd_b3);
            config_packer(1, 8'h0c, 32'h00_00_68_ee);

            config_packer(1, 8'h10, 32'h0a_64_0b_01);
            config_packer(1, 8'h14, 32'h0a_64_0b_20);
            config_packer(1, 8'h18, 32'h00_00_11_22);
            config_packer(1, 8'h1c, 32'h00_00_12_34);

            configured<=True;
            while(True) seq
                noAction;
            endseq
        endseq
    );   
    
    interface AXI4_Stream_Wr_Fab m_axis_fab=packers.m_axis_fab;
endmodule
