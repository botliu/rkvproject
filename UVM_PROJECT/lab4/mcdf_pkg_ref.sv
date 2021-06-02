package mcdf_pkg;

  import uvm_pkg::*;
  `include "uvm_macros.svh"
  import apb_pkg::*;
  import chnl_pkg::*;
  import fmt_pkg::*;
  import mcdf_rgm_pkg::*;

  typedef enum {RW_LEN, RW_PRIO, RW_EN, RD_AVAIL} mcdf_field_t;

  // MCDF reference model
  class mcdf_refmod extends uvm_component;
    mcdf_rgm rgm;
    uvm_blocking_get_peek_port #(chnl_mon_trans) in_bgpk_ports[4];
    uvm_tlm_analysis_fifo #(fmt_trans) out_tlm_fifos[4];

    `uvm_component_utils(mcdf_refmod) 
    function new (string name = "mcdf_refmod", uvm_component parent);
      super.new(name, parent);
      foreach(in_bgpk_ports[i]) in_bgpk_ports[i] = new($sformatf("in_bgpk_ports[%0d]", i), this);
      foreach(out_tlm_fifos[i]) out_tlm_fifos[i] = new($sformatf("out_tlm_fifos[%0d]", i), this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if(!uvm_config_db#(mcdf_rgm)::get(this,"","rgm", rgm)) begin
        `uvm_fatal("GETRGM","cannot get RGM handle from config DB")
      end
    endfunction

    task run_phase(uvm_phase phase);
      fork
        do_packet(0);
        do_packet(1);
        do_packet(2);
        do_packet(3);
      join
    endtask

    task do_packet(int ch);
      fmt_trans ot;
      chnl_mon_trans it;
      forever begin
        this.in_bgpk_ports[ch].peek(it);
        ot = new();
        ot.length = rgm.get_reg_field_length(ch);
        ot.ch_id = rgm.get_reg_field_id(ch);
        ot.data = new[ot.length+3];
        foreach(ot.data[m]) begin
          if(m == 0) begin
            ot.data[m] = (ot.ch_id<<24) + (ot.length<<16);
            ot.parity = ot.data[m];
          end 
          else if(m == ot.data.size()-1) begin
            ot.data[m] = ot.parity;
          end
          else begin
            this.in_bgpk_ports[ch].get(it);
            ot.data[m] = it.data;
            ot.parity ^= it.data;
          end
        end
        this.out_tlm_fifos[ch].put(ot);
      end
    endtask
  endclass: mcdf_refmod

  // MCDF checker (scoreboard)

  class mcdf_checker extends uvm_scoreboard;
    local int err_count;
    local int total_count;
    local int chnl_count[4];
    local virtual chnl_intf chnl_vifs[4]; 
    local virtual mcdf_intf mcdf_vif;
    local mcdf_refmod refmod;
    mcdf_rgm rgm;

    uvm_tlm_analysis_fifo #(chnl_mon_trans) chnl_tlm_fifos[4];
    uvm_tlm_analysis_fifo #(fmt_trans) fmt_tlm_fifo;

    uvm_blocking_get_port #(fmt_trans) exp_bg_ports[4];

    `uvm_component_utils(mcdf_checker)

    function new (string name = "mcdf_checker", uvm_component parent);
      super.new(name, parent);
      this.err_count = 0;
      this.total_count = 0;
      foreach(this.chnl_count[i]) this.chnl_count[i] = 0;
      foreach(chnl_tlm_fifos[i]) chnl_tlm_fifos[i] = new($sformatf("chnl_tlm_fifos[%0d]", i), this);
      fmt_tlm_fifo = new("fmt_tlm_fifo", this);
      foreach(exp_bg_ports[i]) exp_bg_ports[i] = new($sformatf("exp_bg_ports[%0d]", i), this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      // get virtual interface
      if(!uvm_config_db#(virtual mcdf_intf)::get(this,"","mcdf_vif", mcdf_vif)) begin
        `uvm_fatal("GETVIF","cannot get vif handle from config DB")
      end
      foreach(chnl_vifs[i]) begin
        if(!uvm_config_db#(virtual chnl_intf)::get(this,"",$sformatf("chnl_vifs[%0d]",i), chnl_vifs[i])) begin
          `uvm_fatal("GETVIF","cannot get vif handle from config DB")
        end
      end
      if(!uvm_config_db#(mcdf_rgm)::get(this,"","rgm", rgm)) begin
        `uvm_fatal("GETRGM","cannot get RGM handle from config DB")
      end
      this.refmod = mcdf_refmod::type_id::create("refmod", this);
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      foreach(refmod.in_bgpk_ports[i]) refmod.in_bgpk_ports[i].connect(chnl_tlm_fifos[i].blocking_get_peek_export);
      foreach(exp_bg_ports[i]) begin
        exp_bg_ports[i].connect(refmod.out_tlm_fifos[i].blocking_get_export);
      end
    endfunction

    task run_phase(uvm_phase phase);
      fork
        this.do_channel_disable_check(0);
        this.do_channel_disable_check(1);
        this.do_channel_disable_check(2);
        this.do_channel_disable_check(3);
        this.do_data_compare();
      join
    endtask

    task do_data_compare();
      fmt_trans expt, mont;
      bit cmp;
      int ch_idx;
      forever begin
        this.fmt_tlm_fifo.get(mont);
        ch_idx = this.rgm.get_chnl_index(mont.ch_id);
        this.exp_bg_ports[ch_idx].get(expt);
        cmp = mont.compare(expt);   
        this.total_count++;
        this.chnl_count[ch_idx]++;
        if(cmp == 0) begin
          this.err_count++; #1ns;
          `uvm_info("[CMPERR]", $sformatf("monitored formatter data packet:\n %s", mont.sprint()), UVM_MEDIUM)
          `uvm_info("[CMPERR]", $sformatf("expected formatter data packet:\n %s", expt.sprint()), UVM_MEDIUM)
          `uvm_error("[CMPERR]", $sformatf("%0dth times comparing but failed! MCDF monitored output packet is different with reference model output", this.total_count))
        end
        else begin
          `uvm_info("[CMPSUC]",$sformatf("%0dth times comparing and succeeded! MCDF monitored output packet is the same with reference model output", this.total_count), UVM_LOW)
        end
      end
    endtask

    task do_channel_disable_check(int id);
      forever begin
        @(posedge this.mcdf_vif.clk iff (this.mcdf_vif.rstn && this.mcdf_vif.mon_ck.chnl_en[id]===0));
        if(this.chnl_vifs[id].mon_ck.ch_valid===1 && this.chnl_vifs[id].mon_ck.ch_wait===0)
          `uvm_error("[CHKERR]", "ERROR! when channel disabled, wait signal low when valid high") 
      end
    endtask

    function void report_phase(uvm_phase phase);
      string s;
      super.report_phase(phase);
      s = "\n---------------------------------------------------------------\n";
      s = {s, "CHECKER SUMMARY \n"}; 
      s = {s, $sformatf("total comparison count: %0d \n", this.total_count)}; 
      foreach(this.chnl_count[i]) s = {s, $sformatf(" channel[%0d] comparison count: %0d \n", i, this.chnl_count[i])};
      s = {s, $sformatf("total error count: %0d \n", this.err_count)}; 
      foreach(this.chnl_tlm_fifos[i]) begin
        if(this.chnl_tlm_fifos[i].size() != 0)
          s = {s, $sformatf("WARNING:: chnl_tlm_fifos[%0d] is not empty! size = %0d \n", i, this.chnl_tlm_fifos[i].size())}; 
      end
      if(this.fmt_tlm_fifo.size() != 0)
          s = {s, $sformatf("WARNING:: fmt_tlm_fifo is not empty! size = %0d \n", this.fmt_tlm_fifo.size())}; 
      s = {s, "---------------------------------------------------------------\n"};
      `uvm_info(get_type_name(), s, UVM_LOW)
    endfunction
  endclass: mcdf_checker

  class mcdf_virtual_sequencer extends uvm_sequencer #(uvm_sequence_item);
    apb_master_sequencer reg_sqr;
    fmt_sequencer fmt_sqr;
    chnl_sequencer chnl_sqrs[4];
    mcdf_rgm rgm;
    virtual mcdf_intf mcdf_vif;
    `uvm_component_utils(mcdf_virtual_sequencer)
    function new (string name = "mcdf_virtual_sequencer", uvm_component parent);
      super.new(name, parent);
    endfunction
    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if(!uvm_config_db#(virtual mcdf_intf)::get(this,"","mcdf_vif", mcdf_vif)) begin
        `uvm_fatal("GETVIF","cannot get vif handle from config DB")
      end
      if(!uvm_config_db#(mcdf_rgm)::get(this,"","rgm", rgm)) begin
        `uvm_fatal("GETRGM","cannot get RGM handle from config DB")
      end
    endfunction
  endclass

  class reg2mcdf_adapter extends uvm_reg_adapter;
    `uvm_object_utils(reg2mcdf_adapter)
    function new(string name = "reg2mcdf_adapter");
      super.new(name);
      provides_responses = 1;
    endfunction
    function uvm_sequence_item reg2bus(const ref uvm_reg_bus_op rw);
      apb_transfer t = apb_transfer::type_id::create("t");
      t.trans_kind = (rw.kind == UVM_WRITE) ? WRITE : READ;
      t.addr = rw.addr;
      t.data = rw.data;
      t.idle_cycles = 1;
      return t;
    endfunction
    function void bus2reg(uvm_sequence_item bus_item, ref uvm_reg_bus_op rw);
      apb_transfer t;
      if (!$cast(t, bus_item)) begin
        `uvm_fatal("CASTFAIL","Provided bus_item is not of the correct type")
        return;
      end
      rw.kind = (t.trans_kind == WRITE) ? UVM_WRITE : UVM_READ;
      rw.addr = t.addr;
      rw.data = t.data;
      rw.status = t.trans_status == OK ? UVM_IS_OK : UVM_NOT_OK;
    endfunction
  endclass

  class mcdf_bus_analyzer extends uvm_component;
    mcdf_rgm rgm;
    realtime time_window;
    uvm_tlm_analysis_fifo #(apb_transfer) reg_tlm_fifo;
    uvm_tlm_analysis_fifo #(chnl_mon_trans) chnl_tlm_fifos[4];
    uvm_tlm_analysis_fifo #(fmt_trans) fmt_tlm_fifo;
    `uvm_component_utils(mcdf_bus_analyzer)
    function new (string name = "mcdf_bus_analyzer", uvm_component parent);
      super.new(name, parent);
      reg_tlm_fifo = new("reg_tlm_fifo", this);
      foreach(chnl_tlm_fifos[i]) chnl_tlm_fifos[i] = new($sformatf("chnl_tlm_fifos[%0d]", i), this);
      fmt_tlm_fifo = new("fmt_tlm_fifo", this);
      time_window = 1000ns;
    endfunction
    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if(!uvm_config_db#(mcdf_rgm)::get(this,"","rgm", rgm)) begin
        `uvm_fatal("GETRGM","cannot get RGM handle from config DB")
      end
    endfunction
    task run_phase(uvm_phase phase);
      fork
        do_reg_analysis();
        do_data_analysis();
      join
    endtask
    // register access anlysis
    task do_reg_analysis();
      apb_transfer t;
      uvm_reg r;
      forever begin
        reg_tlm_fifo.get(t);
        r = rgm.map.get_reg_by_offset(t.addr - MCDF_REG_ADDR_START);
        `uvm_info("REGANA", $sformatf("%s REG %s with DATA 'h%8x",t.trans_kind, r.get_type_name(), t.data), UVM_LOW)
      end
    endtask
    // channle data input and formatter data output performance analysis
    task do_data_analysis();
      fmt_trans t, tq[$];
      realtime delay;
      real bandwidth;
      forever begin
        #time_window;
        while(fmt_tlm_fifo.try_get(t)) begin
          calculate_delay(t, delay);
          tq.push_back(t);
        end
        calculate_bandwidth(tq, bandwidth);
      end
    endtask

    task calculate_delay(fmt_trans ft, output realtime delay);
      chnl_mon_trans ct;
      int ch_idx = this.rgm.get_chnl_index(ft.ch_id);
      forever begin
        chnl_tlm_fifos[ch_idx].get(ct);
        if(ct.data == ft.data[1]) begin
          delay = ft.start_time - ct.start_time;
          `uvm_info("DATAANA", $sformatf("New packet first data 'h%8x from CHNL[%0d] to FORMATTER delay is %.2f ns", ct.data, ct.id, (delay/1.0ns)), UVM_LOW)
          break;
        end
      end
    endtask

    task calculate_bandwidth(fmt_trans tq[$], output real bandwidth);
      fmt_trans t;
      int data_num = 0;
      foreach(tq[i]) begin
        data_num += tq[i].length + 3;
      end
      // bandwidth = bits_transferred/time(ns) = N(Gb)
      bandwidth = (data_num * 32) / (time_window/1.0ns);
      `uvm_info("DATAANA", $sformatf("From time %0t to %0t, MCDF formatter bandwidth is %.2f Gb", $time-time_window, $time, bandwidth), UVM_LOW)
    endtask
  endclass

  // MCDF top environment
  class mcdf_env extends uvm_env;
    chnl_agent chnl_agts[4];
    apb_master_agent reg_agt;
    fmt_agent fmt_agt;
    mcdf_checker chker;
    mcdf_virtual_sequencer virt_sqr;
    mcdf_rgm rgm;
    reg2mcdf_adapter adapter;
    mcdf_bus_analyzer analyzer;
    uvm_reg_predictor #(apb_transfer) predictor;

    `uvm_component_utils(mcdf_env)

    function new (string name = "mcdf_env", uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      this.chker = mcdf_checker::type_id::create("chker", this);
      foreach(chnl_agts[i]) begin
        this.chnl_agts[i] = chnl_agent::type_id::create($sformatf("chnl_agts[%0d]",i), this);
      end
      this.reg_agt = apb_master_agent::type_id::create("reg_agt", this);
      this.fmt_agt = fmt_agent::type_id::create("fmt_agt", this);
      virt_sqr = mcdf_virtual_sequencer::type_id::create("virt_sqr", this);
      rgm = mcdf_rgm::type_id::create("rgm", this);
      rgm.build();
      uvm_config_db#(mcdf_rgm)::set(this,"*","rgm", rgm);
      adapter = reg2mcdf_adapter::type_id::create("adapter", this);
      predictor = uvm_reg_predictor#(apb_transfer)::type_id::create("predictor", this);
      analyzer = mcdf_bus_analyzer::type_id::create("analyzer", this);
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      foreach(chnl_agts[i]) begin
        chnl_agts[i].monitor.mon_ana_port.connect(chker.chnl_tlm_fifos[i].analysis_export);
        chnl_agts[i].monitor.mon_ana_port.connect(analyzer.chnl_tlm_fifos[i].analysis_export);
      end
      reg_agt.monitor.item_collected_port.connect(analyzer.reg_tlm_fifo.analysis_export);
      fmt_agt.monitor.mon_ana_port.connect(chker.fmt_tlm_fifo.analysis_export);
      fmt_agt.monitor.mon_ana_port.connect(analyzer.fmt_tlm_fifo.analysis_export);
      virt_sqr.reg_sqr = reg_agt.sequencer;
      virt_sqr.fmt_sqr = fmt_agt.sequencer;
      foreach(virt_sqr.chnl_sqrs[i]) virt_sqr.chnl_sqrs[i] = chnl_agts[i].sequencer;
      rgm.map.set_sequencer(reg_agt.sequencer, adapter);
      reg_agt.monitor.item_collected_port.connect(predictor.bus_in);
      predictor.map = rgm.map;
      predictor.adapter = adapter;
    endfunction
  endclass: mcdf_env

  class mcdf_base_virtual_sequence extends uvm_sequence #(uvm_sequence_item);
    chnl_data_sequence chnl_data_seq;
    fmt_config_sequence fmt_config_seq;
    mcdf_rgm rgm;

    `uvm_object_utils(mcdf_base_virtual_sequence)
    `uvm_declare_p_sequencer(mcdf_virtual_sequencer)

    function new (string name = "mcdf_base_virtual_sequence");
      super.new(name);
    endfunction

    virtual task body();
      `uvm_info(get_type_name(), "=====================STARTED=====================", UVM_LOW)
      rgm = p_sequencer.rgm;

      this.do_reg();
      this.do_formatter();
      this.do_data();

      `uvm_info(get_type_name(), "=====================FINISHED=====================", UVM_LOW)
    endtask

    // do register configuration
    virtual task do_reg();
      //User to implment the task in the child virtual sequence
    endtask

    // do external formatter down stream slave configuration
    virtual task do_formatter();
      //User to implment the task in the child virtual sequence
    endtask

    // do data transition from 3 channel slaves
    virtual task do_data();
      //User to implment the task in the child virtual sequence
    endtask

    function bit diff_value(int val1, int val2, string id = "value_compare");
      if(val1 != val2) begin
        `uvm_error("[CMPERR]", $sformatf("ERROR! %s val1 %8x != val2 %8x", id, val1, val2)) 
        return 0;
      end
      else begin
        `uvm_info("[CMPSUC]", $sformatf("SUCCESS! %s val1 %8x == val2 %8x", id, val1, val2), UVM_LOW)
        return 1;
      end
    endfunction

    task wait_cycles(int n);
      repeat(n) @(posedge p_sequencer.mcdf_vif.clk);
    endtask
  endclass

  // MCDF base test
  class mcdf_base_test extends uvm_test;
    mcdf_env env;

    `uvm_component_utils(mcdf_base_test)

    function new(string name = "mcdf_base_test", uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      env = mcdf_env::type_id::create("env", this);
    endfunction

    function void end_of_elaboration_phase(uvm_phase phase);
      super.end_of_elaboration_phase(phase);
      uvm_root::get().set_report_verbosity_level_hier(UVM_HIGH);
      uvm_root::get().set_report_max_quit_count(1);
      uvm_root::get().set_timeout(10ms);
    endfunction

    task run_phase(uvm_phase phase);
      // NOTE:: raise objection to prevent simulation stopping
      phase.raise_objection(this);
      this.run_top_virtual_sequence();
      // NOTE:: drop objection to request simulation stopping
      phase.drop_objection(this);
    endtask

    virtual task run_top_virtual_sequence();
      // User to implement this task in the child tests
    endtask
  endclass: mcdf_base_test

  class mcdf_data_consistence_basic_virtual_sequence extends mcdf_base_virtual_sequence;
    `uvm_object_utils(mcdf_data_consistence_basic_virtual_sequence)
    function new (string name = "mcdf_data_consistence_basic_virtual_sequence");
      super.new(name);
    endfunction
    task do_reg();
      bit[31:0] wr_val, rd_val;
      uvm_status_e status;
      //reset the register block
      @(negedge p_sequencer.mcdf_vif.rstn);
      rgm.reset();
      @(posedge p_sequencer.mcdf_vif.rstn);
      this.wait_cycles(10);
      // slv3 with len=64, en=1
      // slv2 with len=32, en=1
      // slv1 with len=16, en=1
      // slv0 with len=8,  en=1
      wr_val = ('b1<<3) + ('b1<<2) + ('b1<<1) + 1;
      rgm.slv_en.write(status, wr_val);
      rgm.slv_en.read(status, rd_val);
      void'(this.diff_value(wr_val, rd_val, "SLV_EN_REG"));
      wr_val = (63<<24) + (31<<16) + (15<<8) + 7;
      rgm.slv_len.write(status, wr_val);
      rgm.slv_len.read(status, rd_val);
      void'(this.diff_value(wr_val, rd_val, "SLV_LEN_REG"));
    endtask
    task do_formatter();
      `uvm_do_on_with(fmt_config_seq, p_sequencer.fmt_sqr, {fifo == LONG_FIFO; bandwidth == HIGH_WIDTH;})
    endtask
    task do_data();
      fork
        `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[0], {ntrans==100; ch_id==0; data_nidles==0; pkt_nidles==1; data_size==64;})
        `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[1], {ntrans==100; ch_id==1; data_nidles==1; pkt_nidles==4; data_size==64;})
        `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[2], {ntrans==100; ch_id==2; data_nidles==2; pkt_nidles==8; data_size==64;})
        `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[3], {ntrans==100; ch_id==3; data_nidles==1; pkt_nidles==2; data_size==64;})
      join
      #10us; // wait until all data haven been transfered through MCDF
    endtask
  endclass: mcdf_data_consistence_basic_virtual_sequence

  class mcdf_data_consistence_basic_test extends mcdf_base_test;

    `uvm_component_utils(mcdf_data_consistence_basic_test)

    function new(string name = "mcdf_data_consistence_basic_test", uvm_component parent);
      super.new(name, parent);
    endfunction

    task run_top_virtual_sequence();
      mcdf_data_consistence_basic_virtual_sequence top_seq = new();
      top_seq.start(env.virt_sqr);
    endtask
  endclass: mcdf_data_consistence_basic_test

  class mcdf_full_random_virtual_sequence extends mcdf_base_virtual_sequence;
    `uvm_object_utils(mcdf_base_virtual_sequence)
    function new (string name = "mcdf_base_virtual_sequence");
      super.new(name);
    endfunction

    task do_reg();
      bit[31:0] wr_val;
      uvm_status_e status;

      // //reset the register block
      @(negedge p_sequencer.mcdf_vif.rstn);
      rgm.reset();
      @(posedge p_sequencer.mcdf_vif.rstn);
      this.wait_cycles(10);

      // // Randomize SLV_EN_REG 
      // void'(std::randomize(wr_val) with {wr_val inside {['b0:'b1111]};});
      // rgm.slv_en.write(status, wr_val);

      // // Randomize SLV_ID
      // void'(std::randomize(wr_val) with {
      //   wr_val[ 7: 0] inside {[ 0 : 63]};
      //   wr_val[15: 8] inside {[64 :127]};
      //   wr_val[23:16] inside {[128:191]};
      //   wr_val[31:24] inside {[192:255]};
      // });
      // rgm.slv_id.write(status, wr_val);

      // // Randomize SLV_LEN
      // void'(std::randomize(wr_val) with {
      //   wr_val[ 7: 0] dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};
      //   wr_val[15: 8] dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};
      //   wr_val[23:16] dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};
      //   wr_val[31:24] dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};
      // });
      // rgm.slv_len.write(status, wr_val);
      
      //TODO randomize the register model and then use update() function to do
      //register stream access
      void'(rgm.slv_en.en.randomize() with {value inside {['b0:'b1111]};});
      void'(rgm.slv_id.slv0_id.randomize() with {value inside {[ 0 : 63]};});
      void'(rgm.slv_id.slv1_id.randomize() with {value inside {[64 :127]};});
      void'(rgm.slv_id.slv2_id.randomize() with {value inside {[128:191]};});
      void'(rgm.slv_id.slv3_id.randomize() with {value inside {[192:255]};});
      void'(rgm.slv_len.slv0_len.randomize() with {value dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};});
      void'(rgm.slv_len.slv1_len.randomize() with {value dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};});
      void'(rgm.slv_len.slv2_len.randomize() with {value dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};});
      void'(rgm.slv_len.slv3_len.randomize() with {value dist {0:/10, [1:7]:/20, [8:63]:/20,[64:254]:/40, 255:/10};});
      //update them via uvm_reg_block::update()
      rgm.update(status);
      //wait until the registers in DUT have been updated
      this.wait_cycles(10);
    endtask
    task do_formatter();
      `uvm_do_on_with(fmt_config_seq, p_sequencer.fmt_sqr, {fifo inside {SHORT_FIFO, ULTRA_FIFO}; bandwidth inside {LOW_WIDTH, ULTRA_WIDTH};})
    endtask
    task do_data();
      fork
        send_chnl_data(0);
        send_chnl_data(1);
        send_chnl_data(2);
        send_chnl_data(3);
      join
      #10us; // wait until all data haven been transfered through MCDF
    endtask
    task send_chnl_data(int ch);
      int fd_en = rgm.slv_en.en.get();
      int fd_len = rgm.get_reg_field_length(ch);
      if(fd_en[ch]) begin
        `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[ch], 
          {ntrans inside {[200:300]}; 
           ch_id == ch; 
           data_nidles inside {[0:3]}; 
           pkt_nidles inside {1,2,4,8}; 
           data_size == fd_len+1;
         })
      end
    endtask
  endclass: mcdf_full_random_virtual_sequence

  class mcdf_full_random_test extends mcdf_base_test;

    `uvm_component_utils(mcdf_full_random_test)

    function new(string name = "mcdf_full_random_test", uvm_component parent);
      super.new(name, parent);
    endfunction

    task run_top_virtual_sequence();
      mcdf_full_random_virtual_sequence top_seq = new();
      top_seq.start(env.virt_sqr);
    endtask
  endclass: mcdf_full_random_test

  class mcdf_reg_builtin_virtual_sequence extends mcdf_base_virtual_sequence;
    `uvm_object_utils(mcdf_reg_builtin_virtual_sequence)
    function new (string name = "mcdf_reg_builtin_virtual_sequence");
      super.new(name);
    endfunction

    task do_reg();
      uvm_reg_hw_reset_seq reg_rst_seq = new(); 
      uvm_reg_bit_bash_seq reg_bit_bash_seq = new();
      uvm_reg_access_seq reg_acc_seq = new();

      // wait reset asserted and release
      @(negedge p_sequencer.mcdf_vif.rstn);
      @(posedge p_sequencer.mcdf_vif.rstn);

      `uvm_info("BLTINSEQ", "register reset sequence started", UVM_LOW)
      rgm.reset();
      reg_rst_seq.model = rgm;
      reg_rst_seq.start(p_sequencer.reg_sqr);
      `uvm_info("BLTINSEQ", "register reset sequence finished", UVM_LOW)

      `uvm_info("BLTINSEQ", "register bit bash sequence started", UVM_LOW)
      // reset hardware register and register model
      p_sequencer.mcdf_vif.rstn <= 'b0;
      repeat(5) @(posedge p_sequencer.mcdf_vif.clk);
      p_sequencer.mcdf_vif.rstn <= 'b1;
      rgm.reset();
      reg_bit_bash_seq.model = rgm;
      reg_bit_bash_seq.start(p_sequencer.reg_sqr);
      `uvm_info("BLTINSEQ", "register bit bash sequence finished", UVM_LOW)

      `uvm_info("BLTINSEQ", "register access sequence started", UVM_LOW)
      // reset hardware register and register model
      p_sequencer.mcdf_vif.rstn <= 'b0;
      repeat(5) @(posedge p_sequencer.mcdf_vif.clk);
      p_sequencer.mcdf_vif.rstn <= 'b1;
      rgm.reset();
      reg_acc_seq.model = rgm;
      reg_acc_seq.start(p_sequencer.reg_sqr);
      `uvm_info("BLTINSEQ", "register access sequence finished", UVM_LOW)
    endtask
  endclass: mcdf_reg_builtin_virtual_sequence

  class mcdf_reg_builtin_test extends mcdf_base_test;

    `uvm_component_utils(mcdf_reg_builtin_test)

    function new(string name = "mcdf_reg_builtin_test", uvm_component parent);
      super.new(name, parent);
    endfunction

    function void end_of_elaboration_phase(uvm_phase phase);
      super.end_of_elaboration_phase(phase);
      env.reg_agt.vif.has_checks = 0;
    endfunction

    task run_top_virtual_sequence();
      mcdf_reg_builtin_virtual_sequence top_seq = new();
      top_seq.start(env.virt_sqr);
    endtask
  endclass: mcdf_reg_builtin_test

  //-------------------------------------------------------------------
  // For DPI-C interaction test setup
  //-------------------------------------------------------------------
  /* parameter int unsigned MCDF_REG_ADDR_START = 'h0000 ;
  parameter int unsigned MCDF_REG_ADDR_END   = 'h0FFF ;
  parameter int unsigned MCDF_CH0_ADDR_START = 'h1000 ;
  parameter int unsigned MCDF_CH0_ADDR_END   = 'h10FF ;
  parameter int unsigned MCDF_CH1_ADDR_START = 'h1100 ;
  parameter int unsigned MCDF_CH1_ADDR_END   = 'h11FF ;
  parameter int unsigned MCDF_CH2_ADDR_START = 'h1200 ;
  parameter int unsigned MCDF_CH2_ADDR_END   = 'h12FF ;
  parameter int unsigned MCDF_CH3_ADDR_START = 'h1300 ;
  parameter int unsigned MCDF_CH3_ADDR_END   = 'h13FF ;
  parameter int unsigned MCDF_FMT_ADDR_START = 'h2000 ;
  parameter int unsigned MCDF_FMT_ADDR_END   = 'h2FFF ;

  typedef enum {MCDF_REG, MCDF_CH0, MCDF_CH1, MCDF_CH2, MCDF_CH3, MCDF_FMT, MCDF_RESERVED} mcdf_access_target_t;
  typedef struct {mcdf_access_target_t target; 
                  bit[31:0] addr_start; 
                  bit[31:0] addr_end;
                } mcdf_addr_range_t;

  class mcdf_dpi_c_adapter extends uvm_sequence#(uvm_sequence_item);;
    protected mcdf_addr_range_t mcdf_addr_ranges[$];
    apb_master_single_write_sequence apb_write_seq;
    apb_master_single_read_sequence apb_read_seq;
    chnl_data_sequence chnl_data_seq;
    `uvm_object_utils(mcdf_dpi_c_adapter)
    `uvm_declare_p_sequencer(mcdf_virtual_sequencer)
    function new (string name = "mcdf_dpi_c_adapter");
      super.new(name);
      mcdf_addr_ranges.push_back('{MCDF_REG, MCDF_REG_ADDR_START, MCDF_REG_ADDR_END});
      mcdf_addr_ranges.push_back('{MCDF_CH0, MCDF_CH0_ADDR_START, MCDF_CH0_ADDR_END});
      mcdf_addr_ranges.push_back('{MCDF_CH1, MCDF_CH1_ADDR_START, MCDF_CH1_ADDR_END});
      mcdf_addr_ranges.push_back('{MCDF_CH2, MCDF_CH2_ADDR_START, MCDF_CH2_ADDR_END});
      mcdf_addr_ranges.push_back('{MCDF_CH3, MCDF_CH3_ADDR_START, MCDF_CH3_ADDR_END});
      mcdf_addr_ranges.push_back('{MCDF_FMT, MCDF_FMT_ADDR_START, MCDF_FMT_ADDR_END});
    endfunction

    protected function mcdf_access_target_t get_accesss_target(int unsigned addr);
      foreach(mcdf_addr_ranges[i]) begin
        if(this.is_in_addr_range(addr, mcdf_addr_ranges[i]))
          return mcdf_addr_ranges[i].target;
      end
      return MCDF_RESERVED;
    endfunction

    protected function bit is_in_addr_range(int unsigned addr, mcdf_addr_range_t range);
      if(addr >= range.addr_start && addr <= range.addr_end)
        return 1;
      else
        return 0;
    endfunction

    task writew(int unsigned addr, int unsigned data);
      mcdf_access_target_t target = this.get_accesss_target(addr);
      case(target)
        MCDF_REG: `uvm_do_on_with(apb_write_seq, p_sequencer.reg_sqr, {addr == local::addr; data==local::data;})
        MCDF_CH0: `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[0], 
                                  {ntrans==1; ch_id==0; data_nidles==0; pkt_nidles==1; data_size==1; data[0]==local::data;})
        MCDF_CH1: `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[1], 
                                  {ntrans==1; ch_id==1; data_nidles==0; pkt_nidles==1; data_size==1; data[0]==local::data;})
        MCDF_CH2: `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[2], 
                                  {ntrans==1; ch_id==2; data_nidles==0; pkt_nidles==1; data_size==1; data[0]==local::data;})
        MCDF_CH3: `uvm_do_on_with(chnl_data_seq, p_sequencer.chnl_sqrs[3], 
                                  {ntrans==1; ch_id==3; data_nidles==0; pkt_nidles==1; data_size==1; data[0]==local::data;})
        default: `uvm_error("ADDRERR", $sformatf("invalid address 'h%8x", addr)) // {MCDF_FMT, MCDF_RESERVED}
      endcase
    endtask

    task readw(int unsigned addr, output int unsigned data);
      mcdf_access_target_t target = this.get_accesss_target(addr);
      case(target)
        MCDF_REG: begin `uvm_do_on_with(apb_read_seq, p_sequencer.reg_sqr, {addr == local::addr;}) data = apb_read_seq.data; end
        MCDF_FMT: ;
        default: `uvm_error("ADDRERR", $sformatf("invalid address 'h%8x", addr)) // {MCDF_CH0, MCDF_CH1, MCDF_CH2, MCDF_3, MCDF_RESERVED}
      endcase
    endtask

    task wait_cycles(int n);
      repeat(n) @(posedge p_sequencer.mcdf_vif.clk);
    endtask
  endclass: mcdf_dpi_c_adapter

  mcdf_dpi_c_adapter dpi_proxy;
  import "DPI-C" context task dpi_c_thread();
  export "DPI-C" writew                               = task writew;
  export "DPI-C" readw                                = task readw;
  export "DPI-C" wait_cycles                          = task wait_cycles;
  export "DPI-C" report_info                          = task report_info;
  export "DPI-C" report_info_para                     = task report_info_para;
  export "DPI-C" report_warning                       = task report_warning;
  export "DPI-C" report_error                         = task report_error;
  export "DPI-C" report_fatal                         = task report_fatal;
  export "DPI-C" wait_time                            = task wait_time;
  export "DPI-C" finish                               = task finish;

  task automatic writew(int unsigned addr, int unsigned data);
    dpi_proxy.writew(addr, data);
  endtask

  task automatic readw(int unsigned addr, output int unsigned data);
    dpi_proxy.readw(addr, data);
  endtask

  task automatic wait_cycles(int n);
    dpi_proxy.wait_cycles(n);
  endtask

  task automatic report_info(string msg);
    `uvm_info("DPI-C", msg, UVM_NONE)
  endtask
  
  task automatic report_info_para(string msg, int para);
    `uvm_info("DPI-C", $psprintf("\"%s\", \"%h\")", msg, para), UVM_NONE)
  endtask
  
  task automatic report_warning(string msg);
    `uvm_warning("DPI-C", msg)
  endtask
  
  task automatic report_error(string msg);
    `uvm_error("DPI-C", msg)
  endtask
  
  task automatic report_fatal(string msg);
    `uvm_fatal("DPI-C", msg)
  endtask

  task automatic finish();
    `uvm_info("DPI-C", "finish", UVM_NONE)
    $finish();
  endtask

  task automatic wait_time(int nb, string unit);
    case (unit)
      "fs": repeat(nb) #(1fs);
      "ps": repeat(nb) #(1ps);
      "ns": repeat(nb) #(1ns);
      "us": repeat(nb) #(1us);
      "ms": repeat(nb) #(1ms);
      default: `uvm_error("DPI-C", "Unknown time unit in wait_time")
    endcase
  endtask

  class mcdf_dpi_c_test extends mcdf_base_test;

    mcdf_dpi_c_adapter dpi_c_adapter;
    `uvm_component_utils(mcdf_dpi_c_test)

    function new(string name = "mcdf_dpi_c_test", uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      dpi_c_adapter = mcdf_dpi_c_adapter::type_id::create("dpi_c_adapter");
      dpi_proxy = dpi_c_adapter;
    endfunction

    function void end_of_elaboration_phase(uvm_phase phase);
      super.end_of_elaboration_phase(phase);
      env.reg_agt.vif.has_checks = 0;
    endfunction

    task run_top_virtual_sequence();
      bit[31:0] wr_val, rd_val;
      dpi_c_adapter.start(env.virt_sqr);
      @(negedge env.virt_sqr.mcdf_vif.rstn);
      env.rgm.reset();
      @(posedge env.virt_sqr.mcdf_vif.rstn);
      // slv3 with len=64, en=1
      // slv2 with len=32, en=1
      // slv1 with len=16, en=1
      // slv0 with len=8,  en=1
      ////wr_val = ('b1<<3) + ('b1<<2) + ('b1<<1) + 1;
      ////dpi_proxy.writew(MCDF_REG_ADDR_START+'h00, wr_val);
      ////dpi_proxy.readw(MCDF_REG_ADDR_START+'h00, rd_val);
      ////wr_val = (63<<24) + (31<<16) + (15<<8) + 7;
      ////dpi_proxy.writew(MCDF_REG_ADDR_START+'h0C, wr_val);
      ////dpi_proxy.readw(MCDF_REG_ADDR_START+'h0C, rd_val);

      ////for(int i=0; i<16; i++) begin
      ////  for(int j=0; j<8; j++) begin
      ////    dpi_proxy.writew(MCDF_CH0_ADDR_START, (i<<16)+j);
      ////  end
      ////end
      ////for(int i=0; i<16; i++) begin
      ////  for(int j=0; j<16; j++) begin
      ////    dpi_proxy.writew(MCDF_CH1_ADDR_START, (i<<16)+j);
      ////  end
      ////end
      ////for(int i=0; i<16; i++) begin
      ////  for(int j=0; j<32; j++) begin
      ////    dpi_proxy.writew(MCDF_CH2_ADDR_START, (i<<16)+j);
      ////  end
      ////end
      ////for(int i=0; i<16; i++) begin
      ////  for(int j=0; j<64; j++) begin
      ////    dpi_proxy.writew(MCDF_CH3_ADDR_START, (i<<16)+j);
      ////  end
      ////end
      ////#1us;
      dpi_c_thread();
    endtask
  endclass: mcdf_dpi_c_test */

endpackage
