package body Iderium.Media.Filter.Pair is

   function Create (Some : Filter.Instance) return Equilibrated is
      Some_Copy : Filter.Instance_Access := new Filter.Instance'(Some);
   begin
      return Equilibrated'(Base => Filter.Resource.Create (Some_Copy),
                              E => Filter.Equilibrium (Some));
   end Create;

   procedure Reset (Some : in out Equilibrated; Value : Signal.Sample_Type) is
   begin
      null;
   end Reset;
   
   ---------------------------------------------------------------------
   -- Create
   ---------------------------------------------------------------------
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

      Result : Instance (Scheme);
   begin
      Result.Forward  := Equilibrate (Forward);
      Result.Backward := Equilibrate (Backward);
      if Scheme = Sequential and Forward.N > 0 then
         Result.Reversal := Reversal;
      end if;
      return Result;
   end Create;

   ---------------------------------------------------------------------
   -- Apply
   ---------------------------------------------------------------------
   -- Implementation notes:
   ---------------------------------------------------------------------
   procedure Apply (Pair : Instance; Data : Frame.Instance_Access) is

      use Frame;

      Swap : Resource.Instance;

      function Get_Forward_Target return Instance_Access is
      begin
         if Pair.Scheme = Parallel then
            Swap := Resource.Create (new Instance'(Data.all));
            return Resource.Get (Swap);
         else
            return Data;
         end if;
      end Get_Forward_Target;

      function Prepare return Filter.Instance_Access is
      begin
         null;
      end Prepare;

      -- Forward target.
      FT : Frame.Instance_Access := Get_Forward_Target;
      -- Forward broadcast.
      FB : aliased Frame.Broadcast.Instance (FT, 
                        Frame.Broadcast.Forward);
      -- Forward filter.
      FF : Filter.Instance_Access := Prepare (Pair.Forward);
      -- Forward output.
      FO : Filter.Output (FF, FB'Access);
   begin
      -- Forward pass.
      if Pair.Scheme = Parallel then
         null;
      end if;
   end Apply;

end Iderium.Media.Filter.Pair;
