------------------------------------------------------------------------
-- Iderium.Media.Frame
------------------------------------------------------------------------
-- Purpose:
--    This package handles finite signal parts, called "frames".
-- Concept:
--    A frame is a vector of samples which can be grabbed from a signal,
--    as well as broadcasted as a signal (either forward or backward). 
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Iderium.Media.Signal;
with Iderium.Resource;

generic

   -- Also defines a sample type to be used.
   with package Signal is new Iderium.Media.Signal (<>);

package Iderium.Media.Frame is

   -- INSTANCE ---------------------------------------------------------

   type Instance is array (Integer range <>) of Signal.Sample_Type;

   ---------------------------------------------------------------------
   -- Grab
   ---------------------------------------------------------------------
   -- Purpose:
   --    Captures `Output'Length` samples from `Input` and stores them
   --    in `Output` frame.
   ---------------------------------------------------------------------
   generic
      type Signal_Type is new Signal.Instance with private;
   procedure Grab (Input : in out Signal_Type; Output : out Instance);

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

   -- BROADCAST --------------------------------------------------------

   package Broadcast is

      -- INSTANCE ------------------------------------------------------

      type Direction is (Forward, Backward);

      type Instance (Source : not null access Frame.Instance;
                       Pass : Direction) is 
        new Signal.Instance with private;

      ------------------------------------------------------------------
      -- Capture
      ------------------------------------------------------------------
      -- Purpose:
      --    Captures a broadcasted frame.
      ------------------------------------------------------------------
      overriding
      procedure Capture (Broadcast : in out Instance);
      pragma Inline (Capture);

   private

      -- INSTANCE ------------------------------------------------------

      ------------------------------------------------------------------
      -- Initialize
      ------------------------------------------------------------------
      -- Purpose:
      --    Selects an appropriate start index (`First` or `Last`).
      ------------------------------------------------------------------
      function Initialize (Pass : Direction; First, Last : Integer)
        return Integer;

      type Instance (Source : not null access Frame.Instance;
                       Pass : Direction) is new Signal.Instance with
         record
            Index : Integer := 
              Initialize (Pass, Source'First, Source'Last);
         end record;

   end Broadcast;

end Iderium.Media.Frame;
