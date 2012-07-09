------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    None.
-- TODO: implement `Rotate` procedure.
------------------------------------------------------------------------

package body Iderium.Media.Filter is

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
      --    The more `Capacity` is, the longer `Push` works fast.
      ------------------------------------------------------------------
      procedure Push (Buffer : in out Instance;
                      Sample : Signal.Sample_Type) is
         Beginning : Natural := Buffer.Capacity - Buffer.Size;
      begin
         if Buffer.Current = 0 then
            -- We need to shift the buffer manually.
            Buffer.Data(Beginning + 1 .. Buffer.Capacity) := 
              Buffer.Data(1 .. Buffer.Size);
            Buffer.Current := Beginning;
         end if;
         Buffer.Data(Buffer.Current) := Sample;
         Buffer.Current := Buffer.Current - 1;
      end Push;

      ------------------------------------------------------------------
      -- Fill
      ------------------------------------------------------------------
      -- Implementation notes:
      --    None.
      ------------------------------------------------------------------
      procedure Fill (Buffer : in out Instance;
                      Sample : Signal.Sample_Type) is
         Current : Natural renames Buffer.Current;
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
         Current : Natural renames Buffer.Current;
      begin
         for I in Current + 1 .. Current + Buffer.Size loop
            Buffer.Data(I) := Buffer.Data(I) + 
              Factor * (Sample + (-1.0) * Buffer.Data(I));
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
         Filter.Sample := Filter.Context.A(0) * Filter.Input.Sample;
         Buffer.Dot (Filter.Context.A(1 .. Filter.Context.M), 
           Filter.Context.I, Filter.Sample);
         Buffer.Dot (Filter.Context.B, Filter.Context.F, Filter.Sample);
         Buffer.Push (Filter.Context.I, Filter.Input.Sample);
         Buffer.Push (Filter.Context.F, Filter.Sample);
      end if;
   end Capture;

end Iderium.Media.Filter;
