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
typedef 12 AxiLiteAddrWidth;


Int#(16) payload_length=fromInteger(valueOf(PayloadLength));

Integer axis_data_width=valueOf(AxisDataWidth);
Integer axi_lite_data_width=valueOf(AxiLiteDataWidth);
Integer axi_lite_addr_width=valueOf(AxiLiteAddrWidth);

typedef struct{
    Bit#(48) dst_mac;
    Bit#(48) src_mac;
    Bit#(32) dst_ip;
    Bit#(32) src_ip;
    Bit#(16) dst_port;
    Bit#(16) src_port;
    Bit#(16) ipv4_checksum;
}PktHdr deriving(Eq, Bits, FShow);

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
            //f1=f1+disphex(f.value[i]);
            f1=f1+$format("%x%x ", f.value[i][7:0], f.value[i][15:8]);
        end
        return f1;
    endfunction
endinstance

interface PktHdrCfgee;
    (*always_enabled, always_ready*)
    method Action pkt_hdr(PktHdr hdr);
endinterface

interface RfdcPacker;
     (* prefix="s_axis" *)
    interface AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) s_axis_fab;

    interface Get#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) get;

    interface PktHdrCfgee cfg;
endinterface

typedef Bit#(112) EtherHdr;

function EtherHdr eth_hdr(Bit#(48) dst_mac, Bit#(48) src_mac);
    return {
        16'h0008
        //,src_mac[7:0],src_mac[15:8],src_mac[23:16],src_mac[31:24],src_mac[39:32],src_mac[47:40]
        //,dst_mac[7:0],dst_mac[15:8],dst_mac[23:16],dst_mac[31:24],dst_mac[39:32],dst_mac[47:40]
        , src_mac
        , dst_mac
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
    method Bit#(16) get;
endinterface


typedef Bit#(64) UDPHdr;
function Bit#(64) udp_hdr(Bit#(16) dst_port, Bit#(16) src_port, UInt#(16) payload_length1);
    return {
        16'h0,
        toggle_endianness(pack(payload_length1+8)),
        dst_port,
        src_port
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



function MetaData compose_metadata(Bit#(64) pkt_cnt, Bit#(32) port_id);
    return MetaData{
        head_magic: 32'haa55aa55,
        version: 32'h11223344,
        port_id: port_id,
        data_type: 16,
        pkt_cnt: pkt_cnt,
        tail_magic: 64'haa55aa55_44332211
    };
endfunction


typedef SizeOf#(MetaData) MetaDataSize;//256
typedef SizeOf#(EtherHdr) EtherHdrSize;//14*8=112
typedef SizeOf#(IPv4Hdr) IPv4HdrSize;//20*8=160
typedef SizeOf#(UDPHdr) UDPHdrSize;//8*8=64

typedef TAdd#(PayloadLength, TDiv#(SizeOf#(MetaData), 8)) UdpUserDataLengthInBytes;
UInt#(16) udp_user_data_length_in_bytes=fromInteger(valueOf(UdpUserDataLengthInBytes));



typedef TAdd#(TAdd#(EtherHdrSize, IPv4HdrSize), TAdd#(UDPHdrSize, MetaDataSize)) HdrSize;
Integer hdr_size=valueOf(HdrSize);

typedef TSub#(HdrSize, AxisDataWidth) MetaDataPart2Size;
typedef TSub#(MetaDataSize, MetaDataPart2Size) MetaDataPart1Size;
Integer meta_data_part_1_size=valueOf(MetaDataPart1Size);
Integer meta_data_part_2_size=valueOf(MetaDataPart2Size);
Integer meta_data_size=valueOf(MetaDataSize);


module mkIPv4HdrCheckSum(IPv4HdrCheckSum);
    Reg#(Bit#(32)) dst_ip_<-mkReg(0);
    Reg#(Bit#(32)) src_ip_<-mkReg(0);
    Bit#(160) unchecked={
        //[159:128]
        //dst_ip[7:0],dst_ip[15:8],dst_ip[23:16],dst_ip[31:24],
        dst_ip_
        //[127:96]
        //src_ip[7:0],src_ip[15:8],src_ip[23:16],src_ip[31:24],
        ,src_ip_
        //[95:64]
        ,16'h0 //checksum
        ,8'd17 //udp
        ,8'hff//ttl

        //[63:32]
        ,16'b0000_0000_010_00000 //flags+fragment offset
        ,16'h0 //identification


        //31:0
        ,toggle_endianness(pack((udp_user_data_length_in_bytes+8+20)))
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
    method Bit#(16) get=pack(checksum);
endmodule


typedef enum{
    PktHead1,
    PktHead2,
    PktBody,
    PktTail,
    PktLast
}PktState deriving(Eq, Bits, FShow);


module mkRfdcPacker#(Integer port_id)(RfdcPacker);
    AXI4_Stream_Rd#(AxisDataWidth, 0) s_axis<-mkAXI4_Stream_Rd(4);

    Reg#(Bit#(AxisDataWidth)) stage1<-mkReg(0);
    Reg#(Bit#(AxisDataWidth)) stage2<-mkReg(0);
    Reg#(Bit#(AxisDataWidth)) inBuffer<-mkReg(0);

    Wire#(PktHdr) _pkt_hdr<-mkBypassWire;
    
    //Reg#(Bit#(64)) cnt<-mkReg(0);
    Reg#(MetaData) meta_data<-mkReg(compose_metadata(0, fromInteger(port_id)));
    Reg#(Bit#(8)) iter_i<-mkReg(0);
    Reg#(PktState) state<-mkReg(PktHead1);

    Reg#(Bit#(16)) sub_frame_cnt<-mkReg(0);

    Reg#(Bit#(400)) header_buffer<-mkReg(0);


    // Clock current_clk<-exposeCurrentClock();
    // Reset current_rst<-exposeCurrentReset();

    FIFO#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) out_fifo<-mkSizedFIFO(4);
    //SyncFIFOIfc#(AXI4_Stream_Pkg#(AxisDataWidth, 0)) out_fifo <- mkSyncFIFO(4, current_clk, current_rst, current_clk);

    Bit#(160) ipv4_hdr=
        {
            //[159:128]
            //dst_ip[7:0],dst_ip[15:8],dst_ip[23:16],dst_ip[31:24],
            _pkt_hdr.dst_ip
            //[127:96]
            //src_ip[7:0],src_ip[15:8],src_ip[23:16],src_ip[31:24],
            ,_pkt_hdr.src_ip
            //[95:64]
            ,_pkt_hdr.ipv4_checksum
            ,8'd17 //udp
            ,8'hff//ttl

            //[63:32]
            ,16'b0000_0000_010_00000 //flags+fragment offset
            ,16'h0 //identification

            //31:0
            ,toggle_endianness(pack((udp_user_data_length_in_bytes+8+20)))
            ,8'h00//dscp+ecn
            ,8'h45//version+ihl
        };



    //Probe#(PktState) state_probe<-mkProbe;
    Bit#(336) net_header={
        udp_hdr(_pkt_hdr.dst_port,_pkt_hdr.src_port,udp_user_data_length_in_bytes),
        ipv4_hdr,
        eth_hdr(_pkt_hdr.dst_mac, _pkt_hdr.src_mac)
    };
    
    

    rule each;
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
    
    interface PktHdrCfgee cfg;
        method Action pkt_hdr(PktHdr hdr);
            _pkt_hdr<=hdr;
        endmethod
    endinterface
    
endmodule



interface RfdcPackerN#(numeric type n_inputs);
    (*prefix="cfg"*)
    interface Vector#(n_inputs, PktHdrCfgee) cfg;

    (* prefix="m_axis" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_fab;

    (* prefix="s_axis" *)
    interface Vector#(n_inputs,AXI4_Stream_Rd_Fab#(AxisDataWidth, 0)) s_axis_fab;
    
endinterface


function AXI4_Stream_Rd_Fab#(AxisDataWidth, 0) extract_axis_fab(RfdcPacker x);
    return x.s_axis_fab;
endfunction


module mkRfdcPackerN(RfdcPackerN#(n));
    Clock current_clk<-exposeCurrentClock;
    Reset current_rst<-exposeCurrentReset;
    

    Vector#(n, RfdcPacker) packers<-genWithM(mkRfdcPacker);
    AXI4_Stream_Wr#(AxisDataWidth,0) m_axis <-mkAXI4_Stream_Wr(4);
    
    Reg#(UInt#(8)) input_idx<-mkReg(0);

    rule each;
        let d<-packers[input_idx].get.get();
        m_axis.pkg.put(d);
        if(d.last)begin
            if (input_idx==fromInteger(valueOf(n))-1) input_idx<=0;
            else input_idx<=input_idx+1;
        end
    endrule

    function PktHdrCfgee extract_cfg(RfdcPacker x);
        return x.cfg;
    endfunction
    
    interface s_axis_fab=map(extract_axis_fab, packers);
    interface m_axis_fab=m_axis.fab;
    interface cfg=map(extract_cfg, packers);

endmodule


interface PktHdrCfger;
    (*always_enabled, always_ready*)
    method PktHdr value;
endinterface


interface RfdcPackerNCfg#(numeric type n_inputs);
    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Rd_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_rd_fab;
    (*prefix="s_axi"*)
    interface AXI4_Lite_Slave_Wr_Fab#(AxiLiteAddrWidth, AxiLiteDataWidth) s_axi_wr_fab;

    (*prefix="cfg"*)
    interface Vector#(n_inputs, PktHdrCfger) cfg;
endinterface




module mkRfdcPackerNCfg(RfdcPackerNCfg#(n));
    AXI4_Lite_Slave_Wr#(AxiLiteAddrWidth, 32) axi4_slave_lite_wr<-mkAXI4_Lite_Slave_Wr(2);
    AXI4_Lite_Slave_Rd#(AxiLiteAddrWidth, 32) axi4_slave_lite_rd<-mkAXI4_Lite_Slave_Rd(2);

    Vector#(n, IPv4HdrCheckSum) cks<-replicateM(mkIPv4HdrCheckSum);



    Vector#(n, Reg#(PktHdr)) pkt_hdrs <- replicateM(mkReg(PktHdr{
        dst_mac: 48'ha5a5a5a5a5a5,
        src_mac: 48'h5555aaaa5555,
        dst_ip: 32'h0a0a0a0a,
        src_ip: 32'ha0a0a0a0,
        dst_port: 16'h 1122,
        src_port: 16'h 3344,
        ipv4_checksum: 16'h0
    }));

    Vector#(n, Reg#(PktHdr)) pkt_hdrs_checked <- replicateM(mkReg(PktHdr{
        dst_mac: 48'ha5a5a5a5a5a5,
        src_mac: 48'h5555aaaa5555,
        dst_ip: 32'h0a0a0a0a,
        src_ip: 32'ha0a0a0a0,
        dst_port: 16'h 1122,
        src_port: 16'h 3344,
        ipv4_checksum: 16'h0
    }));

    Reg#(Bit#(32)) axi4_data_buf<-mkReg(?);
    
    Reg#(Bit#(8)) i<-mkReg(0);

    rule checksum;
        for(Integer i=0;i!=valueOf(n);i=i+1)begin
            cks[i].put(pkt_hdrs[i].dst_ip, pkt_hdrs[i].src_ip);
        end
    endrule

    rule get_checksum;
        for(Integer i=0;i!=valueOf(n);i=i+1)begin
            pkt_hdrs_checked[i]<=PktHdr{
                dst_mac: pkt_hdrs[i].dst_mac,
                src_mac: pkt_hdrs[i].src_mac,
                dst_ip: pkt_hdrs[i].dst_ip,
                src_ip: pkt_hdrs[i].src_ip,
                dst_port: pkt_hdrs[i].dst_port,
                src_port: pkt_hdrs[i].src_port,
                ipv4_checksum: cks[i].get()
            };
        end
    endrule

    
    mkAutoFSM(//recv cfg
        seq
            while(True)
            seq
                action
                    let req<-axi4_slave_lite_wr.request.get();
                    let sel=req.addr[11:8];
                    let addr=req.addr[7:0];
                    case (addr) matches
                        8'h04: pkt_hdrs[sel].dst_mac[31:0]<=req.data;
                        8'h08: pkt_hdrs[sel].dst_mac[47:32]<=req.data[15:0];
                        
                        8'h0c: pkt_hdrs[sel].src_mac[31:0]<=req.data;
                        8'h10: pkt_hdrs[sel].src_mac[47:32]<=req.data[15:0];
                        8'h14: pkt_hdrs[sel].dst_ip <=req.data;
                        8'h18: pkt_hdrs[sel].src_ip <=req.data;
                        8'h1c: pkt_hdrs[sel].dst_port <= req.data[15:0];
                        8'h20: pkt_hdrs[sel].src_port <= req.data[15:0];
                    endcase
                endaction
                axi4_slave_lite_wr.response.put(
                    AXI4_Lite_Write_Rs_Pkg{
                    resp: OKAY
                }
                );
            endseq
        endseq
    );

    mkAutoFSM(//query cfg
        seq
            while(True)
            seq
                action
                    let req<-axi4_slave_lite_rd.request.get();
                    let sel=req.addr[11:8];
                    let addr=req.addr[7:0];
                    case (addr) matches
                        8'h04: axi4_data_buf<=pkt_hdrs[sel].dst_mac[31:0];
                        8'h08: axi4_data_buf<={16'h0, pkt_hdrs[sel].dst_mac[47:32]};

                        8'h0c: axi4_data_buf<=pkt_hdrs[sel].src_mac[31:0];
                        8'h10: axi4_data_buf<={16'h0, pkt_hdrs[sel].src_mac[47:32]};
                        8'h14: axi4_data_buf<=pkt_hdrs[sel].dst_ip;
                        8'h18: axi4_data_buf<=pkt_hdrs[sel].src_ip;
                        8'h1c: axi4_data_buf<={16'h0, pkt_hdrs[sel].dst_port};
                        8'h20: axi4_data_buf<={16'h0, pkt_hdrs[sel].src_port};
                    endcase
                endaction
                axi4_slave_lite_rd.response.put(
                    AXI4_Lite_Read_Rs_Pkg{
                    resp: OKAY,
                    data: axi4_data_buf
                }
                );
            endseq
        endseq
    );

    function PktHdrCfger extract_cfg(Reg#(PktHdr) x);
        return(
            interface PktHdrCfger;
                method PktHdr value= x;
            endinterface
        );
    endfunction

    interface s_axi_rd_fab=axi4_slave_lite_rd.fab;
    interface s_axi_wr_fab=axi4_slave_lite_wr.fab;

    interface cfg=map(
        extract_cfg
        , pkt_hdrs_checked);

endmodule


(*synthesize*)
module mkRfdcPacker8Cfg(RfdcPackerNCfg#(8));
    RfdcPackerNCfg#(8) cfg<-mkRfdcPackerNCfg;
    return cfg;
endmodule

(*synthesize*)
module mkRfdcPacker8(RfdcPackerN#(8));
    RfdcPackerN#(8) packer <- mkRfdcPackerN;
    return packer;
endmodule
