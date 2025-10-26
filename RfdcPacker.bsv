import AXI4_Stream::*;
import AXI4_Lite_Slave::*;
import AXI4_Lite_Master::*;
import AXI4_Lite_Types::*;
import Connectable::*;
import FIFO::*;
import Clocks::*;
import GetPut::*;
import Vector::*;
import BlueUtils::*;
import StmtFSM::*;
import Probe::*;

typedef 512 AxisDataWidth;
typedef 8192 PayloadLength;
typedef 32 AxiLiteDataWidth;
typedef 11 AxiLiteAddrWidth;


Int#(16) payload_length=fromInteger(valueOf(PayloadLength));
Integer axis_data_width=valueOf(AxisDataWidth);
Integer axi_lite_data_width=valueOf(AxiLiteDataWidth);
Integer axi_lite_addr_width=valueOf(AxiLiteAddrWidth);

typedef struct {
    Vector#(32, Bit#(16)) value;
}RfDCFrame deriving(Eq, Bits);

function Fmt disphex(Bit#(16) x);
    return $format("%x%x ", x[7:0], x[15:8]);
endfunction


instance FShow#(RfDCFrame);
    function Fmt fshow(RfDCFrame f);
        Fmt f1=$format();
        for(Integer i=0;i<32;i=i+1)begin
            f1=f1+disphex(f.value[i]);
        end
        return f1;
    endfunction
endinstance



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

    //(*always_enabled, always_ready*)
    method Action configured(Bit#(1) v);
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
// function IPv4Hdr ipv4_hdr(Bit#(32) dst_ip, Bit#(32) src_ip);
//     Bit#(160) unchecked={
//         //[159:128]
//         //dst_ip[7:0],dst_ip[15:8],dst_ip[23:16],dst_ip[31:24],
//         toggle_endianness(dst_ip)
//         //[127:96]
//         //src_ip[7:0],src_ip[15:8],src_ip[23:16],src_ip[31:24],
//         ,toggle_endianness(src_ip)
//         //[95:64]
//         ,16'h0 //checksum
//         ,8'd17 //udp
//         ,8'hff//ttl

//         //[63:32]
//         ,16'b0000_0000_010_00000 //flags+fragment offset
//         ,16'h0 //identification


//         //31:0
//         ,toggle_endianness(pack((payload_length+8+20)))
//         ,8'h00//dscp+ecn
//         ,8'h45//version+ihl
//     };
//     Vector#(10, UInt#(16)) to_check=unpack(unchecked);
//     Bit#(20) r=pack(foldl(add16, 0, to_check));
//     //return unchecked;
//     Bit#(20) checksum1=extend(r[19:16])+extend(r[15:0]);
//     Bit#(16) checksum= ~(extend(checksum1[19:16])+extend(checksum1[15:0]));
//     return {unchecked[159:96], pack(checksum), unchecked[79:0]};
// endfunction

interface IPv4HdrCheckSum;
    method Action put(Bit#(32) dst_ip, Bit#(32) src_ip);
    method Bit#(160) get;
endinterface

module mkIPv4HdrCheckSum(IPv4HdrCheckSum);
    Reg#(Bit#(32)) dst_ip_<-mkReg(0);
    Reg#(Bit#(32)) src_ip_<-mkReg(0);
    Bit#(160) unchecked={
        //[159:128]
        //dst_ip[7:0],dst_ip[15:8],dst_ip[23:16],dst_ip[31:24],
        toggle_endianness(dst_ip_)
        //[127:96]
        //src_ip[7:0],src_ip[15:8],src_ip[23:16],src_ip[31:24],
        ,toggle_endianness(src_ip_)
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
    Vector#(10, Bit#(16)) to_check=unpack(unchecked);
    Reg#(Bit#(20)) r <-mkReg(0);
    Reg#(Bit#(20)) checksum1<-mkReg(0);
    Reg#(Bit#(16)) checksum<-mkReg(0);
    Bit#(160) result={unchecked[159:96], pack(checksum), unchecked[79:0]};

    Reg#(UInt#(8)) i<-mkReg(0);

    FSM fsm <-mkFSM(
        seq
            r<=0;
            for(i<=0;i!=10;i<=i+1)action
                r<=r+extend(to_check[i]);
            endaction
            checksum1<=extend(r[19:16])+extend(r[15:0]);
            checksum<=~(extend(checksum1[19:16])+extend(checksum1[15:0]));
        endseq
    );

    method Action put(Bit#(32) dst_ip, Bit#(32) src_ip) if (fsm.done());
        dst_ip_<=dst_ip;
        src_ip_<=src_ip;
        fsm.start();
    endmethod
    method Bit#(160) get if(fsm.done())=result;
endmodule

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

    
    Bit#(8) dst_mac_addr_hi_32=8'h00;
    Bit#(8) dst_mac_addr_lo_16=8'h04;
    Reg#(Bit#(48)) dst_mac<-mkReg(0);

    Bit#(8) src_mac_addr_hi_32=8'h08;
    Bit#(8) src_mac_addr_lo_16=8'h0c;
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


    // Clock current_clk<-exposeCurrentClock();
    // Reset current_rst<-exposeCurrentReset();

    FIFO#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) out_fifo<-mkSizedFIFO(2);
    //SyncFIFOIfc#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) out_fifo <- mkSyncFIFO(4, current_clk, current_rst, current_clk);

    Reg#(Bool) configured_<-mkReg(False);
    Reg#(Bool) old_configured_<-mkReg(False);
    Reg#(Bool) ipv4_hdr_updated<-mkReg(False);

    //Probe#(PktState) state_probe<-mkProbe;
    Reg#(IPv4Hdr) ipv4_hdr<-mkReg(0);
    Bit#(336) net_header={
        udp_hdr(dst_port,src_port,fromInteger(valueOf(PayloadLength))),
        ipv4_hdr,
        eth_hdr(dst_mac, src_mac)
    };
    
    IPv4HdrCheckSum ipv4_checksum<-mkIPv4HdrCheckSum;

    rule each;
        old_configured_<=configured_;
        if(!old_configured_) begin
            meta_data.pkt_cnt<=0;
            //net_header<=calc_net_header();
            //ipv4_hdr<=ipv4_checksum.get;
            ipv4_checksum.put(dst_ip, src_ip);
            ipv4_hdr_updated<=False;
        end
        else if(!ipv4_hdr_updated)
        begin
            ipv4_hdr<=ipv4_checksum.get;
            ipv4_hdr_updated<=True;
        end
        else
        begin
            case (state)
                PktHead1:begin
                    state<=PktHead2;
                    //$display("PktHead1");
                    let d<-s_axis.pkg.get();
                    stage1<=d.data;

                    RfDCFrame y=unpack(d.data);
                    //$display("read:",fshow(y));
                    y=unpack({ pack(meta_data)[meta_data_part_1_size-1:0], net_header});
                    //$display("write:",fshow(y));
                    out_fifo.enq(
                        AXI4_Stream_Pkg{
                            data: { pack(meta_data)[meta_data_part_1_size-1:0], net_header},
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
                    //$display("PktHead2");
                    let d<-s_axis.pkg.get;
                    RfDCFrame y=unpack(d.data);
                    //$display("read:",fshow(y));
                    stage2<=d.data;
                    Bit#(MetaDataPart2Size) p1=pack(meta_data)[meta_data_size-1: meta_data_part_1_size];
                    Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage1[axis_data_width-meta_data_part_2_size-1:0 ];

                    y=unpack({p2,p1});
                    //$display("writ:",fshow(y));
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
                    //$display("PktBody", iter_i);
                    iter_i<=iter_i+1;
                    if (iter_i==125) state<=PktTail;
                    if ((iter_i&'h1)==0)begin
                        let d<-s_axis.pkg.get;
                        RfDCFrame y=unpack(d.data);
                        //$display("read:",fshow(y));
                        stage1<=d.data;

                        Bit#(MetaDataPart2Size) p1=stage1[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                        Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage2[axis_data_width-meta_data_part_2_size-1:0 ];
                        y=unpack({p2,p1});
                        //$display("writ:", fshow(y));
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
                    else begin
                        let d<-s_axis.pkg.get;
                        RfDCFrame y=unpack(d.data);
                        //$display("read:",fshow(y));
                        stage2<=d.data;

                        Bit#(MetaDataPart2Size) p1=stage2[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                        Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage1[axis_data_width-meta_data_part_2_size-1:0 ];
                        y=unpack({p2,p1});
                        //$display("writ:", fshow(y));
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
                end
                PktTail:begin
                    //$display("PktTail");
                    state<=PktLast;
                    Bit#(MetaDataPart2Size) p1=stage1[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                    Bit#(TSub#(AxisDataWidth, MetaDataPart2Size)) p2=stage2[axis_data_width-meta_data_part_2_size-1:0 ];
                    RfDCFrame y=unpack({p2,p1});
                    //$display("writ:", fshow(y));
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
                    //$display("PktLast");
                    state<=PktHead1;
                    Bit#(MetaDataPart2Size) p1=stage2[axis_data_width-1: axis_data_width-meta_data_part_2_size];
                    Vector#(27, Bit#(16)) tail=replicate(16'haa55);
                    RfDCFrame y=unpack({pack(tail),p1});
                    //$display("writ:", fshow(y));
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
                end
            endcase
        end
    endrule


    interface s_axis_fab=s_axis.fab;
    //interface AXI4_Stream_Wr_Fab m_axis_fab=m_axis.fab;
    interface get=toGet(out_fifo);
    
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

    method Action configured(Bit#(1) v);
        configured_<=unpack(v);
    endmethod
endmodule

interface RfdcPackerN#(numeric type n_inputs);
    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Rd_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_rd_fab;
    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Wr_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_wr_fab;

    (* prefix="m_axis" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_fab;

    (* prefix="s_axis" *)
    interface Vector#(n_inputs,AXI4_Stream_Rd_Fab#(AxisDataWidth, 0)) s_axis_fab;
    
    (*always_enabled,always_ready*)
    method Action configured(Bit#(1) v);
endinterface


function AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) extract_axis_fab(RfdcPacker x);
    return x.s_axis_fab;
endfunction


module mkRfdcPackerN(RfdcPackerN#(n));
    Clock current_clk<-exposeCurrentClock;
    Reset current_rst<-exposeCurrentReset;

    Vector#(n, RfdcPacker) packers<-replicateM(mkRfdcPacker);
    AXI4_Stream_Wr#(AxisDataWidth,0) m_axis <-mkAXI4_Stream_Wr(4);
    AXI4_Lite_Slave_Rd#(AxiLiteAddrWidth,AxiLiteDataWidth) s_axi_rd<-mkAXI4_Lite_Slave_Rd(2);
    AXI4_Lite_Slave_Wr#(AxiLiteAddrWidth,AxiLiteDataWidth) s_axi_wr<-mkAXI4_Lite_Slave_Wr(2);
    Reg#(UInt#(8)) input_idx<-mkReg(0);

    rule each;
        let d<-packers[input_idx].get.get();
        m_axis.pkg.put(d);
        if(d.last)begin
            if (input_idx==fromInteger(valueOf(n))-1) input_idx<=0;
            else input_idx<=input_idx+1;
        end
    endrule

    function Bool is_valid_addr(Bit#(3) sel);
        return fromInteger(valueOf(n)-1) >= sel;
    endfunction

    Reg#(Bit#(3)) write_sel<-mkReg(0);
    Reg#(Bit#(8)) write_addr<-mkReg(0);
    Reg#(Bit#(32)) write_value<-mkReg(0);

    Reg#(Bit#(3)) read_sel<-mkReg(0);
    Reg#(Bit#(8)) read_addr<-mkReg(0);
    Reg#(Bit#(32)) read_value<-mkReg(0);


    
    // rule read_cfg;
    //     let rq<-s_axi_rd.request.get();
    //     Bit#(11) addr=rq.addr;
    //     Bit#(3) sel=addr[10:8];
    //     Bit#(8) addr1=addr[7:0];
    //     if(is_valid_addr(addr))begin
    //         Bit#(32) result=packers[sel].get_reg(addr1);
    //         let rp=AXI4_Lite_Read_Rs_Pkg{
    //             data: result,
    //             resp: OKAY
    //         };
    //         s_axi_rd.response.put(rp);
    //     end
    //     else
    //         s_axi_rd.response.put(
    //             AXI4_Lite_Read_Rs_Pkg{
    //                 data:0,
    //                 resp: DECERR
    //             }
    //         );
    // endrule

    // rule write_cfg;
    //     let rq<-s_axi_wr.request.get();
    //     Bit#(11) addr=rq.addr;
    //     Bit#(32) data=rq.data;
    //     Bit#(3) sel=addr[10:8];
    //     Bit#(8) addr1=addr[7:0];
    //     if(is_valid_addr(addr))begin
    //         packers[sel].set_reg(addr1,data);
    //         s_axi_wr.response.put(AXI4_Lite_Write_Rs_Pkg{
    //             resp: OKAY
    //         });
    //     end
    //     else
    //         s_axi_wr.response.put(AXI4_Lite_Write_Rs_Pkg{
    //             resp: DECERR
    //         });
    // endrule
    
    mkAutoFSM(
        seq            
            while(True)seq
                action
                    let rq<-s_axi_rd.request.get();
                    read_addr<=rq.addr[7:0];
                    read_sel<=rq.addr[10:8];
                endaction
                if(is_valid_addr(read_sel))seq
                    read_value<=packers[read_sel].get_reg(read_addr);
                    s_axi_rd.response.put(AXI4_Lite_Read_Rs_Pkg{
                        data: read_value,
                        resp: OKAY
                    });
                endseq
                else
                    s_axi_rd.response.put(
                        AXI4_Lite_Read_Rs_Pkg{
                            data:0,
                            resp: DECERR
                        }
                    );
            endseq        
        endseq
    );

    mkAutoFSM(
        seq            
            while(True)seq
                action
                    let rq<-s_axi_wr.request.get();
                    write_addr<=rq.addr[7:0];
                    write_sel<=rq.addr[10:8];
                    write_value<=rq.data;
                endaction
                if(is_valid_addr(write_sel))seq
                    packers[write_sel].set_reg(write_addr, write_value);
                    s_axi_wr.response.put(AXI4_Lite_Write_Rs_Pkg{
                        resp: OKAY
                    });
                endseq
                else
                    s_axi_wr.response.put(
                        AXI4_Lite_Write_Rs_Pkg{
                            resp: DECERR
                        }
                    );
            endseq        
        endseq
    );


    interface s_axis_fab=map(extract_axis_fab, packers);
    interface s_axi_rd_fab = s_axi_rd.fab;
    interface s_axi_wr_fab = s_axi_wr.fab;
    interface m_axis_fab=m_axis.fab;

    method Action configured(Bit#(1) v);
        for(Integer i=0;i<valueOf(n);i=i+1)packers[i].configured(v);
    endmethod
endmodule


interface AutoRfdcPackerN#(numeric type n_inputs);
    (* prefix="m_axis" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_fab;

    (* prefix="s_axis" *)
    interface Vector#(n_inputs,AXI4_Stream_Rd_Fab#(AxisDataWidth, 0)) s_axis_fab;
endinterface

module mkAutoRfdcPackerN(AutoRfdcPackerN#(n));
    AXI4_Lite_Master_Wr#(11, 32) axi4_lite_wr<-mkAXI4_Lite_Master_Wr(2);
    AXI4_Lite_Master_Rd#(11, 32) axi4_lite_rd<-mkAXI4_Lite_Master_Rd(2);
    RfdcPackerN#(n) packers<-mkRfdcPackerN;
    Reg#(Bool) configured<-mkReg(False);
    Reg#(Bit#(8)) i<-mkReg(0);

    mkConnection(axi4_lite_wr.fab, packers.s_axi_wr_fab);
    mkConnection(axi4_lite_rd.fab, packers.s_axi_rd_fab);

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

    
    rule cfg;
        packers.configured(pack(configured));
    endrule

    mkAutoFSM(
        seq  
            configured<=False;
            for(i<=0;i!=fromInteger(valueOf(n));i<=i+1)
            seq
                config_packer(truncate(i), 8'h00, 32'h10_70_fd_b3);
                config_packer(truncate(i), 8'h04, 32'h00_00_68_de);
                config_packer(truncate(i), 8'h08, 32'h10_70_fd_b3);
                config_packer(truncate(i), 8'h0c, 32'h00_00_68_11);

                config_packer(truncate(i), 8'h10, 32'h0a_64_0b_01);
                config_packer(truncate(i), 8'h14, 32'h0a_64_0b_10);
                config_packer(truncate(i), 8'h18, {24'h00_00_11, i});
                config_packer(truncate(i), 8'h1c, {24'h00_00_12, i});
            endseq
            configured<=True;
        endseq
    ); 


    interface m_axis_fab=packers.m_axis_fab;
    interface s_axis_fab=packers.s_axis_fab;
endmodule

(*synthesize*)
module mkAutoRfdcPacker2(AutoRfdcPackerN#(2));
    AutoRfdcPackerN#(2) packers<-mkAutoRfdcPackerN;
    return packers;
endmodule

(*synthesize*)
module mkAutoRfdcPacker4(AutoRfdcPackerN#(4));
    AutoRfdcPackerN#(4) packers<-mkAutoRfdcPackerN;
    return packers;
endmodule

(*synthesize*)
module mkAutoRfdcPacker8(AutoRfdcPackerN#(8));
    AutoRfdcPackerN#(8) packers<-mkAutoRfdcPackerN;
    return packers;
endmodule
