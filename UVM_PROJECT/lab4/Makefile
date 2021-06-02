#############################
# User variables
#############################
TB       = tb
SEED     = 1
TESTNAME ?= mcdf_data_consistence_basic_test
CTEST   ?= 0
DFILES   = ../mcdf_v2/{RR_arbiter.v,sync_dff_fifo.v,slave_node.v,reg_if.v,formater.v,mcdf.v}
VFILES  += apb_pkg/apb_pkg.sv chnl_pkg.sv fmt_pkg.sv mcdf_rgm_pkg_ref.sv mcdf_pkg_ref.sv tb.sv
CFILES   = dpi_ref.c


#############################
# Environment variables
#############################
VCOMP    = vlogan -full64 -ntb_opts uvm-1.2 -sverilog -timescale=1ps/1ps -nc -l comp.log +incdir+apb_pkg +incdir+../mcdf_v2
ELAB     = vcs -full64 -ntb_opts uvm-1.2 -debug_all -l elab.log -sim_res=1ps 
RUN      = $(TB).simv -l run.log -sml +ntb_random_seed=$(SEED) +UVM_TESTNAME=$(TESTNAME)

ifeq ($(CTEST),1)
	ELAB += dpi_ref.c
endif

comp:
	$(VCOMP) 
	$(VCOMP) $(DFILES) $(VFILES)

elab: comp
	$(ELAB) -top $(TB) -o $(TB).simv

run:
	$(RUN) 

rung:
	$(RUN) -gui

clean:
	rm -rf AN.DB DVEfiles csrc *.simv *.simv.daidir *.simv.vdb ucli.key
	rm -rf *.log* *.vpd *.h urgReport

