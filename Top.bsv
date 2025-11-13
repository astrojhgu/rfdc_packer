import RfdcPacker::*;
import NetResponder::*;
import AXI4_Stream::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Types::*;
import Vector::*;
import GetPut::*;
import Connectable::*;
import StmtFSM::*;
import FakeSrc::*;
import AXISink::*;
import AXISGate::*;

/*
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
*/

/*
module mkTop(Empty);
    ArpResponderCfg cfg<-mkArpResponderCfg;
    ArpResponder#(1) arsp<-mkArpResponder;

    AXI4_Stream_Wr#(512,0) axis_wr<-mkAXI4_Stream_Wr(2);
    AXI4_Stream_Rd#(512,0) axis_rd<-mkAXI4_Stream_Rd(2);

    mkConnection(cfg.m_axi_rd_fab, arsp.s_axi_rd_fab);
    mkConnection(cfg.m_axi_wr_fab, arsp.s_axi_wr_fab);

    mkConnection(axis_wr.fab, arsp.s_axis_eth_fab);
    mkConnection(arsp.m_axis_eth_fab, axis_rd.fab);

    mkAutoFSM(
        seq            
            action
            ArpPacket ap=ArpPacket{
                dst_mac: 48'hff_ff_ff_ff_ff_ff,
                src_mac: 48'h01_02_03_04_05_06,
                ether_type: 16'h0800,
                arp_data: ArpData{
                    htype: 16'h1,
                    ptype: 16'h0800,
                    hlen: 6,
                    plen: 4,
                    oper: 1,
                    sha: 48'h01_02_03_04_05_06,
                    spa: 32'h01_02_03_04,
                    tha: 0,
                    tpa: 32'h0a_64_0b_10
                }
            };
            $display("aaa");
            axis_wr.pkg.put(AXI4_Stream_Pkg{
                data: {176'hffff_ffffffff_ffffffff_ffffffff_ffffffff_ffffffff,pack(ap)},
                        user: 0, 
                        keep: {22'h3fffff, 42'h3ffffffffff},
                        dest: 0, 
                        id: 0, 
                        last: True
            });
            endaction
            action 
                let d<-axis_rd.pkg.get();
                ArpPacket x=unpack(d.data[335:0]);
                $display(fshow(x));
            endaction

            repeat(32) noAction;
        endseq
    );

endmodule
*/

module mkTop(Empty);
    FakeSrc src<-mkFakeSrc;
    //AutoRfdcPackerN#(1) packer<-mkAutoRfdcPackerN;
    RfdcPackerN#(1) packer<-mkRfdcPackerN;
    RfdcPackerNCfg#(1) cfg<-mkRfdcPackerNCfg;

    AXI4_Lite_Master_Wr#(12, 32) axi4_lite_wr<-mkAXI4_Lite_Master_Wr(2);
    AXI4_Lite_Master_Rd#(12, 32) axi4_lite_rd<-mkAXI4_Lite_Master_Rd(2);



    AXI4_Stream_Rd#(512,0) axis_rd<-mkAXI4_Stream_Rd(2);
    mkConnection(src.m_axis_fab, packer.s_axis_fab[0]);
    mkConnection(packer.m_axis_fab, axis_rd.fab);
    mkConnection(axi4_lite_wr.fab, cfg.s_axi_wr_fab);
    mkConnection(axi4_lite_rd.fab, cfg.s_axi_rd_fab);

    rule cfg1;
        for(Integer i=0;i<1;i=i+1)begin
            packer.cfg[i].pkt_hdr(cfg.cfg[i].value);
        end
    endrule

    


    mkAutoFSM(
        seq
            repeat(100)
            action
                let x<-axis_rd.pkg.get();
                $display(x.data);
                //$display(packer._configured);
            endaction
        endseq
    );
endmodule