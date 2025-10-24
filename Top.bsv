import RfdcPacker::*;
import AXI4_Stream::*;
import Vector::*;
import GetPut::*;
import Connectable::*;
import StmtFSM::*;
import fake_src::*;




module mkTop(Empty);
    FakeSrc fs <- mkFakeSrcWithHdr;
    AXI4_Stream_Rd#(AxisDataWidth, 0) s_axis<-mkAXI4_Stream_Rd(1);

    mkConnection(fs.m_axis_fab, s_axis.fab);
    Reg#(Int#(32)) i<-mkReg(0);
    mkAutoFSM(
        seq
            // action
            //     packer.set_dst_mac(48'h01_02_03_04_05_06);
            //     packer.set_src_mac(48'h01_02_03_04_05_06);
            //     packer.set_dst_ip(32'h10_20_30_40);
            //     packer.set_src_ip(32'h10_20_30_41);
            //     packer.set_dst_port(16'h5566);
            //     packer.set_src_port(16'h7788);
            // endaction
            for(i<=0;i<260;i<=i+1)
            action
                let x<-s_axis.pkg.get();
                RfDCFrame y=unpack(x.data);
                $display(fshow(y));
                if(x.last)
                $display("========");
            endaction

            //$display($format("%x",{8'haa,8'h55}));
            $display($format("%x", pack(compose_metadata(64'h123456))));
            $display($format("%x", {8'haa,8'h55}));
            action
                Bit#(256) b=pack(compose_metadata(64'h123456));
                $display($format("%x", b));
                MetaData md=unpack(b);
                $display($format("%x", md==compose_metadata(64'h123456)));
            endaction
        endseq
    );    
endmodule
