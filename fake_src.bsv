import AXI4_Stream::*;
import FIFO::*;
import GetPut::*;
import Vector::*;
import BlueUtils::*;
import StmtFSM::*;

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


interface FakeSrc;
    (* prefix="m_axis" *)
    interface AXI4_Stream_Wr_Fab#(AxisDataWidth, 0) m_axis_fab;
endinterface

module mkFakeSrc(FakeSrc);
    AXI4_Stream_Wr#(512, 0) maxis<-mkAXI4_Stream_Wr(1);
    Reg#(Int#(32)) i<-mkReg(0);
    Reg#(Vector#(32, Bit#(16))) data<-mkReg(genWith(gen_data));

    rule each;
        maxis.pkg.put(AXI4_Stream_Pkg{
            data:pack(data), 
            keep:64'hffff_ffff_ffff_ffff,
            dest: 0,
            id: 0, 
            last: False
        });
        data<=next_data(data);
    endrule
    interface AXI4_Stream_Wr_Fab m_axis_fab=maxis.fab;
endmodule
