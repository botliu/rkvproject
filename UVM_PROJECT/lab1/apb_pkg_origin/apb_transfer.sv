
`ifndef APB_TRANSFER_SV
`define APB_TRANSFER_SV

//------------------------------------------------------------------------------
//
// transfer enums, parameters, and events
//
//------------------------------------------------------------------------------
typedef enum {APB_BOGUS_VAL } apb_trans_kind;


//------------------------------------------------------------------------------
//
// CLASS: apb_transfer
//
//------------------------------------------------------------------------------

class apb_transfer extends uvm_sequence_item;
  // USER: Add transaction fields
  rand apb_trans_kind      trans_kind; 

   // USER: Add constraint blocks
  `uvm_object_utils_begin(apb_transfer)
    `uvm_field_enum     (apb_trans_kind, trans_kind, UVM_ALL_ON)
    // USER: Register fields here
  `uvm_object_utils_end

  // new - constructor
  function new (string name = "apb_transfer_inst");
    super.new(name);
  endfunction : new


endclass : apb_transfer

`endif // APB_TRANSFER_SV

