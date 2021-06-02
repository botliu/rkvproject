
`ifndef APB_MASTER_SEQ_LIB_SV
`define APB_MASTER_SEQ_LIB_SV

//------------------------------------------------------------------------------
// SEQUENCE: default
//------------------------------------------------------------------------------
typedef class apb_transfer;
typedef class apb_master_sequencer;

class example_apb_master_seq extends uvm_sequence #(apb_transfer);

    function new(string name=""); 
      super.new(name);
    endfunction : new


  `uvm_object_utils(example_apb_master_seq)    
  
    apb_transfer this_transfer;

    virtual task body();
      `uvm_info(get_type_name(),"Starting example sequence", UVM_HIGH)
       `uvm_do(this_transfer) 
	
      `uvm_info(get_type_name(),$psprintf("Done example sequence: %s",this_transfer.convert2string()), UVM_HIGH)
 
    endtask
  
endclass : example_apb_master_seq

// USER: Add your sequences here

`endif // apb_MASTER_SEQ_LIB_SV

