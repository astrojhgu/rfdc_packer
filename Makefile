# 顶层模块名
TOP     = mkTop
BSC     = bsc
SRC     = Top.bsv RfdcPacker.bsv FakeSrc.bsv AXISink.bsv NetResponder.bsv AXISGate.bsv

# 搜索路径（含BlueAXI、BlueLib等）
BSCPATH = -p "+:.:../BlueAXI/src:../BlueLib/src:../BlueUtils"

# 输出目录
OUTDIR  = build

# VCD 文件
VCD     = dump.vcd

# 默认目标：生成 Verilog
all: verilog

# 生成 Verilog
verilog: $(OUTDIR)/$(TOP).v

$(OUTDIR)/$(TOP).v: $(SRC)
	mkdir -p $(OUTDIR)
	$(BSC) -verilog -u -g $(TOP) $(BSCPATH) -vdir $(OUTDIR) -bdir $(OUTDIR) Top.bsv

# 生成 Bluesim 仿真可执行文件
sim: $(SRC)
	mkdir -p $(OUTDIR)
	$(BSC) -sim -u -g $(TOP) $(BSCPATH) -bdir $(OUTDIR) -simdir $(OUTDIR) Top.bsv
	$(BSC) -sim -e $(TOP) -o $(OUTDIR)/sim -bdir $(OUTDIR) -simdir $(OUTDIR)

# 运行仿真并生成 VCD 波形
run: sim
	cd $(OUTDIR) && ./sim +bsim_vcd=$(VCD)

# 打开 gtkwave
wave: $(OUTDIR)/$(VCD)
	gtkwave $(OUTDIR)/$(VCD)

# 清理
clean:
	rm -rf $(OUTDIR)
