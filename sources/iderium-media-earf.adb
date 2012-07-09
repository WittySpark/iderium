------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    None.
------------------------------------------------------------------------

package body Iderium.Media.Earf is

   function Create (Base : Filter.Instance) return Instance is

      use type Filter.Arrays.Real;

      function Sum (X : Filter.Arrays.Real_Vector) 
        return Filter.Arrays.Real is
         Result : Filter.Arrays.Real := 0.0;
      begin
         for I in X'Range loop
            Result := Result + X(I);
         end loop;
         return Result;
      end Sum;

   begin
      return Instance'(Filter.Instance'(Base) with 
        R => Sum(Base.A) / (1.0 - Sum(Base.B)));
   end Create;


   overriding
   procedure Capture (Earf : in out Output) is
      use Filter;
   begin
      Capture (Earf.Input.all);
      Capture (Earf.Edges.all);
      Earf.Active := Earf.Input.Active;
      if Earf.Active then
         Buffer.Mix (Earf.Context.I, Earf.Input.Sample, 
           Earf.Edges.Sample);
         Buffer.Mix (Earf.Context.F, Earf.Context.R * Earf.Input.Sample,
           Earf.Edges.Sample);
         Earf.Sample := Earf.Context.A(0) * Earf.Input.Sample;
         Buffer.Dot (Earf.Context.A(1 .. Earf.Context.M), 
           Earf.Context.I, Earf.Sample);
         Buffer.Dot (Earf.Context.B, Earf.Context.F, Earf.Sample);
         Buffer.Push (Earf.Context.I, Earf.Input.Sample);
         Buffer.Push (Earf.Context.F, Earf.Sample);
      end if;
   end Capture;

end Iderium.Media.Earf;
