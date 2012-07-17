------------------------------------------------------------------------
-- Iderium.Media.Buffer
------------------------------------------------------------------------
-- Implementation notes:
------------------------------------------------------------------------

package body Iderium.Media.Buffer is

   -- INSTANCE ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   function Get (Buffer : Instance) return Frame.Instance is
      Data    : Frame.Instance renames Buffer.Data;
      Current : Integer renames Buffer.Current;
   begin
      return Data(Current + 1 .. Current + Buffer.Size);
   end Get;

   ---------------------------------------------------------------------
   -- Put
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   procedure Put (Buffer : in out Instance; Custom : Frame.Instance) is
      Data    : Frame.Instance renames Buffer.Data;
      Current : Integer renames Buffer.Current;
   begin
      Data(Current + 1 .. Current + Buffer.Size) := Custom;
   end Put;

   ---------------------------------------------------------------------
   -- Push
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   procedure Push (Buffer : in out Instance; 
                   Sample : Frame.Signal.Sample_Type) is
      Data    : Frame.Instance renames Buffer.Data;
      Current : Integer renames Buffer.Current;
   begin
      if Data'Length > 0 then
         if Current < Data'First then
            -- We need to shift the data manually.
            Data(2 .. Buffer.Size) := 
                 Data(Current + 1 .. Current + Buffer.Size - 1);
            Current := 1;
         end if;
         Data(Current) := Sample;
         Current := Current - 1;
      end if;
   end Push;

end Iderium.Media.Buffer;
