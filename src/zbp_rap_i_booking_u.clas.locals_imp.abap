class lhc_booking definition inheriting from cl_abap_behavior_handler.
  private section.

    methods update for modify
      importing entities for update booking.

    methods delete for modify
      importing keys for delete booking.

    methods read for read
      importing keys for read booking result result.

    methods rba_Travel for read
      importing keys_rba for read booking\_Travel full result_requested result result link association_links.

endclass.

class lhc_booking implementation.

  method update.
  endmethod.

  method delete.
  endmethod.

  method read.
  endmethod.

  method rba_Travel.
  endmethod.

endclass.
