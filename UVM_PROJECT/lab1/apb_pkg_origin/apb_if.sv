
`ifndef APB_IF_SV
`define APB_IF_SV

interface apb_if (input clk, input rstn);

  // Control flags
  bit                has_checks = 1;
  bit                has_coverage = 1;

  // Actual Signals 
  // USER: Add interface signals

  clocking cb @(posedge clk);
   // USER: Add clocking block detail
  endclocking : cb

  modport DUT ( input clk
		// USER: Add dut I/O
              );
  modport TB  ( clocking cb);


  // Coverage and assertions to be implemented here.
  // USER: Add assertions/coverage here

endinterface : apb_if

`endif // APB_IF_SV
