import AXI4_Stream::*;
import GetPut::*;
interface AXI4Sink;
    (*prefix="s_axis"*)
    interface AXI4_Stream_Rd_Fab#(512,0) fab;
endinterface

(*synthesize*)
module mkAXI4Sink(AXI4Sink);
    AXI4_Stream_Rd#(512,0) rd<-mkAXI4_Stream_Rd(2);

    rule each;
        let d<-rd.pkg.get();
    endrule

    interface fab=rd.fab;
endmodule