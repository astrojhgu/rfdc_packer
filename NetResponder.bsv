import AXI4_Stream::*;
import AXI4_Lite_Types::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Slave::*;
import Vector::*;
import StmtFSM::*;
import GetPut::*;
import BlueUtils::*;

typedef 512 AxisDataWidth;
typedef 32 AxiLiteDataWidth;
typedef 12 AxiLiteAddrWidth;



interface NetResponder;
    (* prefix="s_axis_eth" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) s_axis_eth_fab;

    (* prefix="m_axis_eth" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) m_axis_eth_fab;


    (* prefix="s_axis_arp" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) s_axis_arp_fab;

    (* prefix="m_axis_arp" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) m_axis_arp_fab;

endinterface


typedef struct{
    Bit#(48) host_mac;
    Bit#(32) host_ip;
} HostInfo deriving(Eq, Bits);

instance FShow#(HostInfo);
    function Fmt fshow(HostInfo x);
        Fmt f1=$format();
        f1=f1+$format("%x:%x:%x:%x:%x:%x\n", 
        x.host_mac[47:40],
        x.host_mac[39:32],
        x.host_mac[31:24],
        x.host_mac[23:16],
        x.host_mac[15:8],
        x.host_mac[7:0]
        );
        f1=f1+$format("%d.%d.%d.%d",
        x.host_ip[31:24],
        x.host_ip[23:16],
        x.host_ip[15:8],
        x.host_ip[7:0]
        );
        return f1;
    endfunction
endinstance


typedef struct{
    Bit#(16) htype;
    Bit#(16) ptype; 
    Bit#(8) hlen;
    Bit#(8) plen; 
    Bit#(16) oper;
    Bit#(48) sha;
    Bit#(32) spa;
    Bit#(48) tha;
    Bit#(32) tpa;
}ArpData deriving(Eq);

instance FShow#(ArpData);
    function Fmt fshow(ArpData ad);
        Fmt f1=$format();
        f1=f1+fshow("htype:")+fmt(ad.htype)+fshow("\n");
        f1=f1+fshow("ptype:")+fmt(ad.ptype)+fshow("\n");
        f1=f1+fshow("hlen:")+fmt(ad.hlen)+fshow("\n");
        f1=f1+fshow("plen:")+fmt(ad.plen)+fshow("\n");
        f1=f1+fshow("oper:")+fmt(ad.oper)+fshow("\n");
        f1=f1+fshow("sha:")+fmt(ad.sha)+fshow("\n");
        f1=f1+fshow("spa:")+fmt(ad.spa)+fshow("\n");
        f1=f1+fshow("tha:")+fmt(ad.tha)+fshow("\n");
        f1=f1+fshow("tpa:")+fmt(ad.tpa)+fshow("\n");
        return f1;
    endfunction
endinstance

instance Bits#(ArpData, 224);
    function Bit#(224) pack(ArpData ad);
        return {
                toggle_endianness(ad.tpa),
                toggle_endianness(ad.tha),
                toggle_endianness(ad.spa),
                toggle_endianness(ad.sha),
                toggle_endianness(ad.oper),
                toggle_endianness(ad.plen), 
                toggle_endianness(ad.hlen),
                toggle_endianness(ad.ptype), 
                toggle_endianness(ad.htype)};
    endfunction

    function ArpData unpack(Bit#(224) x);
        return ArpData{
            htype: toggle_endianness(x[15:0]),
            ptype: toggle_endianness(x[31:16]),
            hlen: toggle_endianness(x[39:32]),
            plen: toggle_endianness(x[47:40]),
            oper: toggle_endianness(x[63:48]),
            sha: toggle_endianness(x[111:64]),
            spa: toggle_endianness(x[143:112]),
            tha: toggle_endianness(x[191: 144]),
            tpa: toggle_endianness(x[223:192])
        };
    endfunction
endinstance

typedef struct{
    Bit#(48) dst_mac;
    Bit#(48) src_mac; 
    Bit#(16) ether_type;
    ArpData arp_data;//224
}ArpPacket deriving(Eq);

instance FShow#(ArpPacket);
    function Fmt fshow(ArpPacket ap);
        Fmt f=$format();
        f=f+fmt(ap.dst_mac)+fshow("\n");
        f=f+fmt(ap.src_mac)+fshow("\n");
        f=f+fmt(ap.ether_type)+fshow("\n");
        f=f+fshow(ap.arp_data)+fshow("\n");
        return f;
    endfunction
endinstance

instance Bits#(ArpPacket,336);
    function Bit#(336) pack(ArpPacket ap);
        return {pack(ap.arp_data),
            toggle_endianness(ap.ether_type),
            toggle_endianness(ap.src_mac),
            toggle_endianness(ap.dst_mac)};
    endfunction

    function ArpPacket unpack(Bit#(336) x);
        return ArpPacket{
            dst_mac: toggle_endianness(x[47:0]),
            src_mac: toggle_endianness(x[95:48]),
            ether_type: unpack(toggle_endianness(x[111:96])),
            arp_data: unpack(x[335:112])
        };
    endfunction
endinstance

Integer arp_pkt_size= valueOf(SizeOf#(ArpPacket));



interface ArpResponder#(numeric type n);
    (* prefix="s_axis_eth" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) s_axis_eth_fab;

    (* prefix="m_axis_eth" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_eth_fab;
    

    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Rd_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_rd_fab;
    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Wr_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_wr_fab;    
endinterface



module mkArpResponder(ArpResponder#(n));
    Vector#(n, Reg#(HostInfo)) host_info<-replicateM(mkReg(HostInfo{
        host_mac:48'h0,
        host_ip:32'h0
    }));

    Reg#(ArpPacket) tmp_arp_pkt<-mkReg(?);

    AXI4_Lite_Slave_Rd#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_rd<-mkAXI4_Lite_Slave_Rd(4);
    AXI4_Lite_Slave_Wr#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_wr<-mkAXI4_Lite_Slave_Wr(4);

    AXI4_Stream_Rd#(512, 0) s_axis<-mkAXI4_Stream_Rd(4);
    AXI4_Stream_Wr#(512, 0) m_axis<-mkAXI4_Stream_Wr(4);

    Reg#(Bit#(12)) wr_addr<-mkReg(0);
    Reg#(Bit#(12)) rd_addr<-mkReg(0);
    
    Bit#(9) wr_sel=wr_addr[10:2];
    Bit#(9) rd_sel=rd_addr[10:2];

    Bit#(2) wr_reg=wr_addr[1:0];
    Bit#(2) rd_reg=rd_addr[1:0];

    Reg#(Bit#(32)) wr_value<-mkReg(0);
    Reg#(Bit#(32)) rd_value<-mkReg(0);

    Reg#(Bit#(16)) i<-mkReg(0);

    mkAutoFSM(seq
        repeat(100) noAction;
        while(True)seq
            $display("====================");
            action
                let d<-s_axis.pkg.get();
                tmp_arp_pkt<=unpack(d.data[arp_pkt_size-1:0]);
            endaction
            $display("arp=", fshow(tmp_arp_pkt));
            for(i<=0;i!=fromInteger(valueOf(n));i<=i+1)seq
                $display(fshow(host_info[i].host_ip)," vs ", fshow(tmp_arp_pkt.arp_data.tpa));
                if(host_info[i].host_ip==tmp_arp_pkt.arp_data.tpa)seq
                    tmp_arp_pkt<=ArpPacket{
                        dst_mac:tmp_arp_pkt.src_mac,
                        src_mac:host_info[i].host_mac,
                        ether_type:tmp_arp_pkt.ether_type,
                        arp_data: ArpData{
                            htype: tmp_arp_pkt.arp_data.htype,
                            ptype: tmp_arp_pkt.arp_data.ptype,
                            hlen: tmp_arp_pkt.arp_data.hlen,
                            plen: tmp_arp_pkt.arp_data.plen,
                            oper: 2,
                            sha: host_info[i].host_mac,
                            spa: host_info[i].host_ip,
                            tha: tmp_arp_pkt.arp_data.sha,
                            tpa: tmp_arp_pkt.arp_data.spa
                        }
                    };
                    m_axis.pkg.put(AXI4_Stream_Pkg{
                        data: {176'h0,pack(tmp_arp_pkt)},
                        user: 0, 
                        keep: {
                            22'h3fffff, 
                            42'h3ffffffffff},
                        dest: 0, 
                        id: 0, 
                        last: True
                    });
                endseq
            endseq
        endseq
    endseq);

    //write
    mkAutoFSM(seq
        while(True)seq
            action
                let rq<-s_axi_wr.request.get();
                wr_addr<=rq.addr;
                wr_value<=rq.data;
            endaction
            if(wr_sel<fromInteger(valueOf(n)) && wr_reg<3)seq
                action
                    if (wr_addr==0) host_info[wr_sel].host_mac[47:16]<=wr_value;
                    else if(wr_addr==1) host_info[wr_sel].host_mac[15:0]<=wr_value[15:0];
                    else host_info[wr_sel].host_ip<=wr_value;
                endaction
                s_axi_wr.response.put(AXI4_Lite_Write_Rs_Pkg{
                    resp: OKAY
                });
            endseq
            else s_axi_wr.response.put(AXI4_Lite_Write_Rs_Pkg{
                    resp: DECERR
                });
            $display("hi=",fshow(host_info[0]));
        endseq
    endseq
    );


    //read
    mkAutoFSM(seq
        while(True)seq
            action
                let rq<-s_axi_rd.request.get();
                rd_addr<=rq.addr;
            endaction
            if(rd_sel<fromInteger(valueOf(n)) && rd_reg<3)seq
                action
                    if (rd_addr==0) rd_value<=host_info[rd_sel].host_mac[47:16];
                    else if(rd_addr==1) rd_value<={16'h0, host_info[rd_sel].host_mac[15:0]};
                    else rd_value<=host_info[rd_sel].host_ip;
                endaction
                s_axi_rd.response.put(AXI4_Lite_Read_Rs_Pkg{
                    data: rd_value,
                    resp: OKAY
                });
            endseq
            else s_axi_rd.response.put(AXI4_Lite_Read_Rs_Pkg{
                    data: 0,
                    resp: DECERR
                });
        endseq
        endseq
    );

    interface s_axis_eth_fab = s_axis.fab;
    interface m_axis_eth_fab = m_axis.fab;
    interface s_axi_rd_fab = s_axi_rd.fab;
    interface s_axi_wr_fab = s_axi_wr.fab;
endmodule

interface ArpResponderCfg;
    (*prefix="m_axi"*)
    interface AXI4_Lite_Master_Rd_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) m_axi_rd_fab;
    (*prefix="m_axi"*)
    interface AXI4_Lite_Master_Wr_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) m_axi_wr_fab;    
endinterface

(*synthesize*)
module mkArpResponder1(ArpResponder#(1));
    ArpResponder#(1) arp_resp<-mkArpResponder;
    return arp_resp;
endmodule

(*synthesize*)
module mkArpResponderCfg(ArpResponderCfg);
    AXI4_Lite_Master_Rd#(AxiLiteAddrWidth, AxiLiteDataWidth) m_axi_rd<-mkAXI4_Lite_Master_Rd(2);
    AXI4_Lite_Master_Wr#(AxiLiteAddrWidth, AxiLiteDataWidth) m_axi_wr<-mkAXI4_Lite_Master_Wr(2);

    mkAutoFSM(seq
        m_axi_wr.request.put(AXI4_Lite_Write_Rq_Pkg{
            addr:12'h0,
            data:32'h10_70_fd_b3,
            strb: 4'hf,
            prot: PRIV_INSECURE_INSTRUCTION
        });
        action
            let rs<-m_axi_wr.response.get();
            $display("resp:", rs.resp);
        endaction

        m_axi_wr.request.put(AXI4_Lite_Write_Rq_Pkg{
            addr:12'h1,
            data:32'h00_00_68_11,
            strb: 4'hf,
            prot: PRIV_INSECURE_INSTRUCTION
        });
        action
            let rs<-m_axi_wr.response.get();
            $display("resp:", rs.resp);
        endaction

        m_axi_wr.request.put(AXI4_Lite_Write_Rq_Pkg{
            addr:12'h2,
            data:32'h0a_64_0b_10,
            strb: 4'hf,
            prot: PRIV_INSECURE_INSTRUCTION
        });
        action
            let rs<-m_axi_wr.response.get();
            $display("resp:", rs.resp);
        endaction

        while(True)noAction;
    endseq);

    interface m_axi_rd_fab=m_axi_rd.fab;
    interface m_axi_wr_fab=m_axi_wr.fab;
endmodule
