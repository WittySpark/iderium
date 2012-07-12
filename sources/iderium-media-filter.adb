------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    None.
-- TODO: implement `Rotate` procedure.
------------------------------------------------------------------------

package body Iderium.Media.Filter is

   -- BUFFER -----------------------------------------------------------

   package body Buffer is

      ------------------------------------------------------------------
      -- Dot
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      procedure Dot (Vector : Arrays.Real_Vector; 
                     Buffer : Instance; 
                     Output : in out Signal.Sample_Type) is
      begin
         for I in 1 .. Buffer.Size loop
            Output := Output + Vector(Vector'First + I - 1) * 
              Buffer.Data(Buffer.Current + I);
         end loop;
      end Dot;

      ------------------------------------------------------------------
      -- Push
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      procedure Push (Buffer : in out Instance;
                      Sample : Signal.Sample_Type) is
         Data : Buffer_Data renames Buffer.Data;
         Current : Integer renames Buffer.Current;
      begin
         if Data'Length > 0 then
            if Current < Data'First then
               -- We need to shift the buffer manually.
               Data(2 .. Buffer.Size) := 
                 Data(Current + 1 .. Current + Buffer.Size - 1);
               Current := 1;
            end if;
            Data(Current) := Sample;
            Current := Current - 1;
         end if;
      end Push;

      ------------------------------------------------------------------
      -- Fill
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      procedure Fill (Buffer : in out Instance;
                      Sample : Signal.Sample_Type) is
         Current : Integer renames Buffer.Current;
      begin
         Buffer.Data(Current + 1 .. Current + Buffer.Size) := 
           (others => Sample);
      end Fill;

      ------------------------------------------------------------------
      -- Mix
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      -- TODO: Should we require unary minus operator to be defined?
      ------------------------------------------------------------------
      procedure Mix (Buffer : in out Instance; 
                     Sample : Signal.Sample_Type;
                     Factor : Arrays.Real) is
         use type Arrays.Real;
         Data : Buffer_Data renames Buffer.Data;
         Current : Natural renames Buffer.Current;
      begin
         for I in Current + 1 .. Current + Buffer.Size loop
            Data(I) := Data(I) + Factor * (Sample + (-1.0) * Data(I));
         end loop;
      end Mix;

      ------------------------------------------------------------------
      -- Rotate
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      procedure Rotate (Buffer : in out Instance; 
                        Matrix : Arrays.Real_Matrix) is
      begin
         null;
      end Rotate;

   end Buffer;

   -- OUTPUT -----------------------------------------------------------

   ---------------------------------------------------------------------
   -- Capture
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    None.
   ---------------------------------------------------------------------
   overriding
   procedure Capture (Filter : in out Output) is
   begin
      Capture (Filter.Input.all);
      Filter.Active := Filter.Input.Active;
      if Filter.Active then
         Filter.Sample := Filter.Context.A * Filter.Input.Sample;
         Buffer.Dot (Filter.Context.B, Filter.Context.I, Filter.Sample);
         Buffer.Dot (Filter.Context.C, Filter.Context.F, Filter.Sample);
         Buffer.Push (Filter.Context.I, Filter.Input.Sample);
         Buffer.Push (Filter.Context.F, Filter.Sample);
      end if;
   end Capture;

end Iderium.Media.Filter;
