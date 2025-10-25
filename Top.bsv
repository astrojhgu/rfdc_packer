import RfdcPacker::*;
import AXI4_Stream::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Types::*;
import Vector::*;
import GetPut::*;
import Connectable::*;
import StmtFSM::*;
import FakeSrc::*;




module mkTop(Empty);
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
        let d<-s_axis.pkg.get();

        RfDCFrame y=unpack(d.data);
        $display(fshow(y));
        if(d.last)
        $display("========");
        step_cnt<=step_cnt+1;
        if(step_cnt==260)$finish;
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
    
endmodule
