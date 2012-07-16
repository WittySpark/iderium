------------------------------------------------------------------------
-- Iderium.Media.Frame
------------------------------------------------------------------------
-- Purpose:
-- Concept:
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

   generic
      
   package 
   
   ---------------------------------------------------------------------
   -- Grab
   ---------------------------------------------------------------------
   -- Purpose:
   ---------------------------------------------------------------------
   generic
      type Signal_Type is new Signal.Instance with private;
   procedure Grab (Input : in out Signal_Type; 
                  Output : out Instance);

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

   -- BROADCAST --------------------------------------------------------

   package Broadcast is

      type Direction is (Forward, Backward);

      type Instance (Source : not null access Frame.Instance;
                    Passage : Direction) is
        new Signal.Instance with private;

      overriding
      procedure Capture (Broadcast : in out Instance);
      pragma Inline (Capture);

   private

      function Initialize (Passage : Direction;
                       First, Last : Integer) return Integer;

      type Instance (Frame : not null access Instance;
                   Passage : Direction) is new Signal.Instance with
         record
            Current : Integer := Initialize (Passage,
                                         Frame'First,
                                          Frame'Last);
         end record;

   end Broadcast;

end Iderium.Media.Frame;
