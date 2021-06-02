#############################
# User variables
#############################
TB       = apb_tb
SEED     = 1
TESTNAME ?= apb_single_transaction_test
VFILES  += apb_pkg_ref/{apb_pkg.sv,apb_tb.sv}


#############################
# Environment variables
#############################
VCOMP    = vlogan -full64 -ntb_opts uvm-1.2 -sverilog -timescale=1ps/1ps -nc -l comp.log +incdir+apb_pkg_ref
ELAB     = vcs -full64 -ntb_opts uvm-1.2 -debug_all -l elab.log -sim_res=1ps 
RUN      = $(TB).simv -l run.log -sml +ntb_random_seed=$(SEED) +UVM_TESTNAME=$(TESTNAME)

comp:
	$(VCOMP) 
	$(VCOMP) $(VFILES)

elab: comp
	$(ELAB) -top $(TB) -o $(TB).simv

run:
	$(RUN) 

rung:
	$(RUN) -gui

clean:
	rm -rf AN.DB DVEfiles csrc *.simv *.simv.daidir *.simv.vdb ucli.key
	rm -rf *.log* *.vpd *.h urgReport

