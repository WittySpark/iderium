------------------------------------------------------------------------
-- Iderium.Media.Buffer
------------------------------------------------------------------------
-- Purpose:
-- Concept:
------------------------------------------------------------------------

with Iderium.Media.Frame;

generic

   -- Also defines a sample type to be used.
   with package Frame is new Iderium.Media.Frame (<>);

   -- The more `Capacity` is, the longer `Push` works fast, and 
   -- the more memory is used.
   Capacity : Natural := 0;

package Iderium.Media.Buffer is

   -- INSTANCE ---------------------------------------------------------

   type Instance (Size : Natural) is private;

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Purpose:
   --    .
   ---------------------------------------------------------------------
   function Get (Buffer : Instance) return Frame.Instance;
   pragma Inline (Get);

   ---------------------------------------------------------------------
   -- Put
   ---------------------------------------------------------------------
   -- Purpose:
   --    .
   ---------------------------------------------------------------------
   procedure Put (Buffer : in out Instance; Custom : Frame.Instance);
   pragma Inline (Put);

   ---------------------------------------------------------------------
   -- Push
   ---------------------------------------------------------------------
   -- Purpose:
   --    .
   ---------------------------------------------------------------------
   procedure Push (Buffer : in out Instance; 
                   Sample : Frame.Signal.Sample_Type);
   pragma Inline (Push);

private

   -- INSTANCE ---------------------------------------------------------

   -- `Current` points to the last available slot in `Data`.
   -- Current frame is stored in `Data(Current + 1 .. Current + Size)`.
   type Instance (Size : Natural) is
      record
         Data    : Frame.Instance (-Capacity + 1 .. Size);
         Current : Integer := 0;
      end record;

end Iderium.Media.Buffer;
