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
            Output := Output + 
                      Vector(Vector'First + I - 1) * 
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
         Data    : Buffer_Data renames Buffer.Data;
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
         Data    : Buffer_Data renames Buffer.Data;
         Current : Integer renames Buffer.Current;
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

   -- INSTANCE ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Equilibrium
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    None.
   ---------------------------------------------------------------------
   function Equilibrium (Filter : Instance) return Arrays.Real is

      use type Arrays.Real;

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

   -- PAIR -------------------------------------------------------------

   package body Pair is

      function Create (Scheme : Connection_Scheme;
            Forward, Backward : Filter.Instance) return Instance is

         -- Evaluates a reversal matrix.
         function Evaluate_Reversal return Matrix_Resource.Instance is
            use Arrays;
            Q : Real_Matrix (1 .. Forward.N, 1 .. Forward.N) :=
              (others => 0.0);
            T : Real_Matrix (1 .. Forward.N, 1 .. Forward.N);
            M : Matrix_Access;
         begin
            -- Fill `Q`.
            Q(1, 1) := Forward.C(1);
            for I in 2 .. Forward.N loop
               Q(1, I)     := Forward.C(I);
               Q(I, I - 1) := 1.0;
            end loop;
            -- Compute `T`.
            
         end Evaluate_Reversal;

         Result : Instance (Scheme);
         Forward_Copy  : Filter.Instance_Access := 
           new Filter.Instance'(Forward);
         Backward_Copy : Filter.Instance_Access := 
           new Filter.Instance'(Backward);
      begin
         Result.Forward  := Resource.Create (Forward_Copy);
         Result.Backward := Resource.Create (Backward_Copy);
         Result.Forward_Equilibrium  := Equilibrium (Forward);
         Result.Backward_Equilibrium := Equilibrium (Backward);
         if Scheme = Sequential then
            Result.Reversal := Evaluate_Reversal;
         end if;
      end Create;

      procedure Apply (Pair : Instance; 
                       Data : in out Frame.Instance) is
      begin
         null;
      end Apply;

   end Pair;

end Iderium.Media.Filter;
