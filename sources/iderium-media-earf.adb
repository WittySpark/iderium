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

      Base_Copy : Filter.Instance_Access := new Filter.Instance'(Base);
   begin
      return Instance'(Base => Filter.Resource.Create (Base_Copy), 
        R => (Base.A + Sum (Base.B)) / (1.0 - Sum (Base.C)));
   end Create;

   procedure Free (Name : in out Instance_Access) is
   begin
      Deallocate (Name);
   end Free;


   overriding
   procedure Capture (Earf : in out Output) is
      use Filter;
      Base : Filter.Instance_Access := 
        Filter.Resource.Get (Earf.Context.Base);
   begin
      Capture (Earf.Input.all);
      Capture (Earf.Edges.all);
      Earf.Active := Earf.Input.Active;
      if Earf.Active then
         Buffer.Mix (Base.I, Earf.Input.Sample, Earf.Edges.Sample);
         Buffer.Mix (Base.F, Earf.Context.R * Earf.Input.Sample, 
           Earf.Edges.Sample);
         Earf.Sample := Base.A * Earf.Input.Sample;
         Buffer.Dot (Base.B, Base.I, Earf.Sample);
         Buffer.Dot (Base.C, Base.F, Earf.Sample);
         Buffer.Push (Base.I, Earf.Input.Sample);
         Buffer.Push (Base.F, Earf.Sample);
      end if;
   end Capture;

end Iderium.Media.Earf;
