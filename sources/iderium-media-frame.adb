------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    None.
------------------------------------------------------------------------

package body Iderium.Media.Frame is

   -- INSTANCE ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Grab
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    None.
   ---------------------------------------------------------------------
   procedure Grab (Input : in out Signal_Type; Output : out Instance) is
   begin
      for I in Output'Range loop
         Capture (Input);
         exit when not Input.Active;
         Output(I) := Input.Sample;
      end loop;
   end Grab;

   -- BROADCAST --------------------------------------------------------

   package body Broadcast is

      -- INSTANCE ------------------------------------------------------

      ------------------------------------------------------------------
      -- Capture
      ------------------------------------------------------------------
      -- Implementation notes:
      --    Does not report about premature signal termination.
      ------------------------------------------------------------------
      procedure Capture (Broadcast : in out Instance) is
      begin
         Broadcast.Active := Broadcast.Index in Broadcast.Source'Range;
         if Broadcast.Active then
            Broadcast.Sample := Broadcast.Source(Broadcast.Index);
            case Broadcast.Pass is
               when Forward =>
                  Broadcast.Index := Broadcast.Index + 1;
               when Backward =>
                  Broadcast.Index := Broadcast.Index - 1;
            end case;
         end if;
      end Capture;

      ------------------------------------------------------------------
      -- Initialize
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      function Initialize (Pass : Direction; First, Last : Integer)
        return Integer is
      begin
         case Pass is
            when Forward =>
               return First;
            when Backward =>
               return Last;
         end case;
      end Initialize;

   end Broadcast;

end Iderium.Media.Frame;
