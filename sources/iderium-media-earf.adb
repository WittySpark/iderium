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
        M => Base.M,
        N => Base.N,
        R => (Base.A + Sum (Base.B)) / (1.0 - Sum (Base.C)));
   end Create;

   procedure Free (Name : in out Instance_Access) is
   begin
      Deallocate (Name);
   end Free;


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
         Earf.Sample := Earf.Context.A * Earf.Input.Sample;
         Buffer.Dot (Earf.Context.B, Earf.Context.I, Earf.Sample);
         Buffer.Dot (Earf.Context.C, Earf.Context.F, Earf.Sample);
         Buffer.Push (Earf.Context.I, Earf.Input.Sample);
         Buffer.Push (Earf.Context.F, Earf.Sample);
      end if;
   end Capture;

end Iderium.Media.Earf;
