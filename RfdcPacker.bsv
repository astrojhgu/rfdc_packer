import AXI4_Stream::*;
import AXI4_Lite_Slave::*;
import AXI4_Lite_Types::*;
import FIFO::*;
import GetPut::*;
import Vector::*;
import BlueUtils::*;
import StmtFSM::*;
import Probe::*;

typedef 512 AxisDataWidth;
typedef 8192 PayloadLength;
Int#(16) payload_length=fromInteger(valueOf(PayloadLength));
Integer axis_data_width=valueOf(AxisDataWidth);

interface RfdcPacker;
     (* prefix="s_axis" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) s_axis_fab;

    interface Get#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) get;
    
    method Action set_src_mac(Bit#(48) v);
    method Action set_dst_mac(Bit#(48) v);
    method Action set_src_ip(Bit#(32) v);
    method Action set_dst_ip(Bit#(32) v);
    method Action set_src_port(Bit#(16) v);
    method Action set_dst_port(Bit#(16) v);

    method Action set_reg(Bit#(8) addr, Bit#(32) v);

    method Bit#(48) get_src_mac();
    method Bit#(48) get_dst_mac();
    method Bit#(32) get_src_ip();
    method Bit#(32) get_dst_ip();
    method Bit#(16) get_src_port();
    method Bit#(16) get_dst_port();

    method Bit#(32) get_reg(Bit#(8) addr);
endinterface

typedef Bit#(112) EtherHdr;

function EtherHdr eth_hdr(Bit#(48) dst_mac, Bit#(48) src_mac);
    return {
        16'h0008
        //,src_mac[7:0],src_mac[15:8],src_mac[23:16],src_mac[31:24],src_mac[39:32],src_mac[47:40]
        //,dst_mac[7:0],dst_mac[15:8],dst_mac[23:16],dst_mac[31:24],dst_mac[39:32],dst_mac[47:40]
        , toggle_endianness(src_mac)
        , toggle_endianness(dst_mac)
    };
endfunction


function UInt#(20) add16(UInt#(20) x, UInt#(16) y);
    return x+extend(y);
endfunction


typedef Bit#(160) IPv4Hdr;
function IPv4Hdr ipv4_hdr(Bit#(32) dst_ip, Bit#(32) src_ip);
    Bit#(160) unchedked={
        //[159:128]
        //dst_ip[7:0],dst_ip[15:8],dst_ip[23:16],dst_ip[31:24],
        toggle_endianness(dst_ip)
        //[127:96]
        //src_ip[7:0],src_ip[15:8],src_ip[23:16],src_ip[31:24],
        ,toggle_endianness(src_ip)
        //[95:64]
        ,16'h0 //checksum
        ,8'd17 //udp
        ,8'hff//ttl

        //[63:32]
        ,16'b0000_0000_010_00000 //flags+fragment offset
        ,16'h0 //identification


        //31:0
        ,toggle_endianness(pack((payload_length+8+20)))
        ,8'h00//dscp+ecn
        ,8'h45//version+ihl
    };
    Vector#(10, UInt#(16)) to_check=unpack(unchedked);
    Bit#(20) r=pack(foldl(add16, 0, to_check));
    //return unchedked;
    Bit#(20) checksum1=extend(r[19:16])+extend(r[15:0]);
    Bit#(16) checksum= ~(extend(checksum1[19:16])+extend(checksum1[15:0]));
    return {unchedked[159:96], pack(checksum), unchedked[79:0]};
endfunction

typedef Bit#(64) UDPHdr;
function Bit#(64) udp_hdr(Bit#(16) dst_port, Bit#(16) src_port, UInt#(16) payload_length1);
    return {
        16'h0,
        toggle_endianness(pack(payload_length1+8)),
        toggle_endianness(dst_port),
        toggle_endianness(src_port)
    };
endfunction


typedef struct{
    Bit#(32) head_magic;
    Bit#(32) version;
    Bit#(32) port_id;
    Bit#(32) data_type;
    Bit#(64) pkt_cnt;
    Bit#(64) tail_magic;
}MetaData deriving(Eq, FShow);

instance Bits#(MetaData, 256);
    function Bit#(256) pack(MetaData x);
        return {
            (x.tail_magic)
            ,(x.pkt_cnt)
            ,(x.data_type)
            ,(x.port_id)
            ,(x.version)
            ,(x.head_magic)};
    endfunction

    function MetaData unpack(Bit#(MetaDataSize) x);
        return MetaData{
            head_magic: unpack((x[31:0])),
            version: unpack((x[63:32])),
            port_id: unpack((x[95:64])),
            data_type: unpack((x[127:96])),
            pkt_cnt: unpack((x[191:128])),
            tail_magic: unpack((x[255:192]))
        };
    endfunction
endinstance

function MetaData compose_metadata(Bit#(64) pkt_cnt);
    return MetaData{
        head_magic: 32'haa55aa55,
        version: 32'h11223344,
        port_id: 0,
        data_type: 16,
        pkt_cnt: pkt_cnt,
        tail_magic: 64'haa55aa55_44332211
    };
endfunction


typedef SizeOf#(MetaData) MetaDataSize;//256
typedef SizeOf#(EtherHdr) EtherHdrSize;//14*8=112
typedef SizeOf#(IPv4Hdr) IPv4HdrSize;//20*8=160
typedef SizeOf#(UDPHdr) UDPHdrSize;//8*8=64




typedef TAdd#(TAdd#(EtherHdrSize, IPv4HdrSize), TAdd#(UDPHdrSize, MetaDataSize)) HdrSize;
Integer hdr_size=valueOf(HdrSize);

typedef TSub#(HdrSize, AxisDataWidth) MetaDataPart2Size;
typedef TSub#(MetaDataSize, MetaDataPart2Size) MetaDataPart1Size;
Integer meta_data_part_1_size=valueOf(MetaDataPart1Size);
Integer meta_data_part_2_size=valueOf(MetaDataPart2Size);
Integer meta_data_size=valueOf(MetaDataSize);

typedef enum{
    PktHead1,
    PktHead2,
    PktBody,
    PktTail,
    PktLast
}PktState deriving(Eq, Bits, FShow);

(*synthesize*)
module mkRfdcPacker(RfdcPacker);
    AXI4_Stream_Rd#(AxisDataWidth, 0) s_axis<-mkAXI4_Stream_Rd(2);

    Reg#(Bit#(AxisDataWidth)) stage1<-mkReg(0);
    Reg#(Bit#(AxisDataWidth)) stage2<-mkReg(0);
    Reg#(Bit#(AxisDataWidth)) inBuffer<-mkReg(0);
    Reg#(Bool) phase <-mkReg(False);

    
    Bit#(8) dst_mac_addr_lo_16=8'h00;
    Bit#(8) dst_mac_addr_hi_32=8'h04;
    Reg#(Bit#(48)) dst_mac<-mkReg(0);

    Bit#(8) src_mac_addr_lo_16=8'h08;
    Bit#(8) src_mac_addr_hi_32=8'h0c;
    Reg#(Bit#(48)) src_mac<-mkReg(0);
    
    Bit#(8) dst_ip_addr=8'h10;
    Reg#(Bit#(32)) dst_ip<-mkReg(0);

    Bit#(8) src_ip_addr=8'h14;
    Reg#(Bit#(32)) src_ip<-mkReg(0);

    Bit#(8) dst_port_addr=8'h18;
    Reg#(Bit#(16)) dst_port<-mkReg(0);

    Bit#(8) src_port_addr=8'h1c;
    Reg#(Bit#(16)) src_port<-mkReg(0);

    
    //Reg#(Bit#(64)) cnt<-mkReg(0);
    Reg#(MetaData) meta_data<-mkReg(compose_metadata(0));
    Reg#(Bit#(8)) iter_i<-mkReg(0);
    Reg#(PktState) state<-mkReg(PktHead1);

    Reg#(Bit#(16)) sub_frame_cnt<-mkReg(0);

    Reg#(Bit#(400)) header_buffer<-mkReg(0);

    FIFO#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) out_fifo<-mkSizedFIFO(2);
    //Probe#(PktState) state_probe<-mkProbe;

    function Bit#(336) calc_net_header();
        return {
            udp_hdr(dst_port,src_port,fromInteger(valueOf(PayloadLength))),
            ipv4_hdr(dst_ip, src_ip),
            eth_hdr(dst_mac, src_mac)
        };
    endfunction

    rule each;
        case (state)
            PktHead1:begin
                state<=PktHead2;
                $display("PktHead1");
                let d<-s_axis.pkg.get();
                stage1<=d.data;
                out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: { pack(meta_data)[meta_data_part_1_size-1:0], calc_net_header()},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: False
                    }
                );
            end
            PktHead2:begin
                state<=PktBody;
                iter_i<=0;
                $display("PktHead2");
                let d<-s_axis.pkg.get;
                stage2<=d.data;
                Bit#(MetaDataPart2Size) p1=pack(meta_data)[meta_data_size-1: meta_data_part_1_size];
                Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage1[axis_data_width-meta_data_part_2_size-1:0 ];
                out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: {p2, p1},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: False
                    }
                );
            end
            PktBody:begin
                $display("PktBody", iter_i);
                iter_i<=iter_i+1;
                if (iter_i==8'h7e) state<=PktTail;
                if ((iter_i&'h1)==0)begin
                    Bit#(MetaDataPart2Size) p1=stage1[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                    Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage2[axis_data_width-meta_data_part_2_size-1:0 ];
                    
                    out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: {p2, p1},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: False
                    }
                    );
                    let d<-s_axis.pkg.get;
                    stage1<=d.data;
                end
                else begin
                    Bit#(MetaDataPart2Size) p1=stage2[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                    Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage1[axis_data_width-meta_data_part_2_size-1:0 ];
                    
                    out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: {p2, p1},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: False
                    }
                    );

                    let d<-s_axis.pkg.get;
                    stage2<=d.data;
                end
            end
            PktTail:begin
                $display("PktTail");
                state<=PktLast;
                Bit#(MetaDataPart2Size) p1=stage1[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage2[axis_data_width-meta_data_part_2_size-1:0 ];
                
                out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: {p2, p1},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: False
                    }
                );
            end
            PktLast:begin
                $display("PktLast");
                state<=PktHead1;
                Bit#(MetaDataPart2Size) p1=stage2[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                Vector#(27, Bit#(16)) tail=replicate(16'haa55);

                out_fifo.enq(
                    AXI4_Stream_Pkg{
                        data: {pack(tail),p1},
                        user: 0, 
                        keep: 64'hffff_ffff_ffff_ffff,
                        dest: 0,
                        id: 0,
                        last: True
                    }
                );

                meta_data.pkt_cnt<=meta_data.pkt_cnt+1;
                $display("==");
            end
        endcase
    endrule


    interface AXI4_Stream_Rd_Fab s_axis_fab=s_axis.fab;
    //interface AXI4_Stream_Wr_Fab m_axis_fab=m_axis.fab;
    interface Get get=toGet(out_fifo);
    
    method Action set_src_mac(Bit#(48) v);
        src_mac<=v;
    endmethod
    method Action set_dst_mac(Bit#(48) v);
        dst_mac<=v;
    endmethod
    method Action set_src_ip(Bit#(32) v);
        src_ip<=v;
    endmethod
    method Action set_dst_ip(Bit#(32) v);
        dst_ip<=v;
    endmethod
    method Action set_src_port(Bit#(16) v);
        src_port<=v;
    endmethod
    method Action set_dst_port(Bit#(16) v);
        dst_port<=v;
    endmethod

    method Action set_reg(Bit#(8) addr, Bit#(32) v);
        case (addr)
            dst_mac_addr_hi_32:dst_mac[47:16]<=v;
            dst_mac_addr_lo_16:dst_mac[15:0]<=v[15:0];
            src_mac_addr_hi_32:src_mac[47:16]<=v;
            src_mac_addr_lo_16:src_mac[15:0]<=v[15:0];
            dst_ip_addr: dst_ip<=v;
            src_ip_addr: src_ip<=v;
            dst_port_addr: dst_port<=v[15:0];
            src_port_addr: src_port<=v[15:0];
        endcase
    endmethod

    method Bit#(48) get_src_mac=src_mac;
    method Bit#(48) get_dst_mac=dst_mac;
    method Bit#(32) get_src_ip=src_ip;
    method Bit#(32) get_dst_ip=dst_ip;
    method Bit#(16) get_src_port=src_port;
    method Bit#(16) get_dst_port=dst_port;

    method Bit#(32) get_reg(Bit#(8) addr);
        let x=case (addr)
            dst_mac_addr_hi_32:dst_mac[47:16];
            dst_mac_addr_lo_16:extend(dst_mac[15:0]);
            src_mac_addr_hi_32:src_mac[47:16];
            src_mac_addr_lo_16:extend(src_mac[15:0]);
            dst_ip_addr: dst_ip;
            src_ip_addr: src_ip;
            dst_port_addr: extend(dst_port);
            src_port_addr: extend(src_port);
            default: 32'hffffffff;
        endcase;
        return x;
    endmethod
endmodule
