------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Implementation notes:
--    TODO: implement `Rotate` procedure.
------------------------------------------------------------------------

package body Iderium.Media.Filter is

   -- BUFFER -----------------------------------------------------------

   package body Buffer is

      ------------------------------------------------------------------
      -- Dot
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
      --    TODO: Should we require unary minus operator to be defined?
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

      ------------------------------------------------------------------
      -- Create
      ------------------------------------------------------------------
      function Create (Scheme : Connection_Scheme;
            Forward, Backward : Filter.Instance) return Instance is

         -- Computes a reversal matrix.
         function Reversal return Matrix_Resource.Instance is
            use Arrays;
            Q : Real_Matrix (1 .. Forward.N, 1 .. Forward.N) :=
              (others => (others => 0.0));
            P : Real_Matrix (1 .. Forward.N, 1 .. Forward.N);
            S : Real_Matrix (1 .. Forward.N, 1 .. Forward.N);
            T : Real_Vector (1 .. Forward.N);
            R : Real_Vector (1 .. Forward.N);
            Result : Matrix_Access := 
              new Real_Matrix (1 .. Backward.N, 1 .. Forward.N);
         begin
            -- Fill `Q`.
            Q(1, 1) := Forward.C(1);
            for I in 2 .. Forward.N loop
               Q(1, I)     := Forward.C(I);
               Q(I, I - 1) := 1.0;
            end loop;
            -- Compute `T`.
            T := Backward.A * Unit_Vector (1, Forward.N);
            P := Unit_Matrix (Forward.N);
            for I in 1 .. Backward.M loop
               P := Q * P; -- Q**I
               for J in 1 .. Forward.N loop
                  T(J) := T(J) + Backward.B(I) * P(1, J);
               end loop;
            end loop;
            -- Compute `S`.
            P := Unit_Matrix (Forward.N);
            for I in 1 .. Backward.N loop
               P := Q * P; -- Q**I
               S := S - Backward.C(I) * P;
            end loop;
            -- Compute `R` and form `Result`.
            R := Solve (S, T);
            for I in 1 .. Backward.N loop
               R := R * Q;
               for J in 1 .. Forward.N loop
                  Result(I, J) := R(J);
               end loop;
            end loop;
            -- Safely return `Result`.
            return Matrix_Resource.Create (Result);
         end Reversal;

         Result        : Instance (Scheme);
         Forward_Copy  : Filter.Instance_Access := 
           new Filter.Instance'(Forward);
         Backward_Copy : Filter.Instance_Access := 
           new Filter.Instance'(Backward);
      begin
         Result.Forward  := Resource.Create (Forward_Copy);
         Result.Backward := Resource.Create (Backward_Copy);
         Result.Forward_Equilibrium  := Equilibrium (Forward);
         Result.Backward_Equilibrium := Equilibrium (Backward);
         if Scheme = Sequential and Forward.N > 0 then
            Result.Reversal := Reversal;
         end if;
         return Result;
      end Create;

      ------------------------------------------------------------------
      -- Apply
      ------------------------------------------------------------------
      -- Implementation notes:
      ------------------------------------------------------------------
      procedure Apply (Pair : Instance; 
                       Data : in out Frame.Instance) is
         Forward_Data_Broadcast : Frame.Broadcast (Data, Forward);
         Forward_Filter : Filter.Instance_Access := 
           Filter.Resource.Get (Pair.Forward);
         Forward_Filter_Output : Filter.Output (Forward_Filter,
                                        Forward_Data_Broadcast);
      begin
         -- Forward pass.
         Filter.Buffer.Fill (Forward_Filter.I, Data(Data'First));
         Filter.Buffer.Fill (Forward_Filter.F, 
           Pair.Forward_Equilibrium * Data(Data'First));
         loop
            
      end Apply;

   end Pair;

end Iderium.Media.Filter;
