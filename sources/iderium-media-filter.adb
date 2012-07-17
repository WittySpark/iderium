------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    .
------------------------------------------------------------------------

package body Iderium.Media.Filter is

   -- BUFFER -----------------------------------------------------------

   ---------------------------------------------------------------------
   -- Dot
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   procedure Dot (Left   : Arrays.Real_Vector; 
                  Right  : Buffer.Instance; 
                  Result : in out Frame.Signal.Sample_Type) is
      Current : Frame.Instance := Buffer.Get (Right);
   begin
      for I in Current'Range loop
         Result := Result + 
           Left(Left'First + I - Current'First) * Current(I);
      end loop;
   end Dot;

   -- INSTANCE ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Equilibrium
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   function Equilibrium (Filter : Instance) return Arrays.Real is

      use type Arrays.Real;

      -- Computes a sum of all elements of `X`.
      function Sum (X : Arrays.Real_Vector) return Arrays.Real is
         Result : Arrays.Real := 0.0;
      begin
         for I in X'Range loop
            Result := Result + X(I);
         end loop;
         return Result;
      end Sum;

   begin
      return (Filter.A + Sum (Filter.B)) / (1.0 - Sum (Filter.C));
   end Equilibrium;

   -- OUTPUT -----------------------------------------------------------

   ---------------------------------------------------------------------
   -- Capture
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    .
   ---------------------------------------------------------------------
   overriding
   procedure Capture (Filter : in out Output) is
   begin
      Capture (Filter.Input.all);
      Filter.Active := Filter.Input.Active;
      if Filter.Active then
         Filter.Sample := Filter.Context.A * Filter.Input.Sample;
         Dot (Filter.Context.B, Filter.Context.I, Filter.Sample);
         Dot (Filter.Context.C, Filter.Context.F, Filter.Sample);
         Buffer.Push (Filter.Context.I, Filter.Input.Sample);
         Buffer.Push (Filter.Context.F, Filter.Sample);
      end if;
   end Capture;

end Iderium.Media.Filter;
