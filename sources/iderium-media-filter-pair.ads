------------------------------------------------------------------------
-- Iderium.Media.Filter.Pair
------------------------------------------------------------------------

generic

   with package Frame is new Iderium.Media.Frame (Signal);

package Iderium.Media.Filter.Pair is

   -- INSTANCE ---------------------------------------------------------

   type Connection_Scheme is (Parallel, Sequential);

   type Instance (Scheme : Connection_Scheme) is private;

   function Create (Scheme : Connection_Scheme;
         Forward, Backward : Filter.Instance) return Instance;

   procedure Apply (Pair : Instance; Data : Frame.Instance_Access);

private

   -- EQUILIBRATED -----------------------------------------------------

   type Equilibrated is new Filter.Instance with 
      record
         Base        : Filter.Resource.Instance;
         E : Arrays.Real;
      end record;

   function Create (Some : Filter.Instance) return Equilibrated;

   --procedure Equilibrate (Some : in out Equilibrated);
   
   -- INSTANCE ---------------------------------------------------------

   type Matrix_Access is access Arrays.Real_Matrix;

   procedure Free is
     new Ada.Unchecked_Deallocation (Arrays.Real_Matrix, 
                                          Matrix_Access);

   package Matrix_Resource is new Iderium.Resource (Matrix_Access);

   type Instance (Scheme : Connection_Scheme) is
      record
         Forward, Backward : Equilibrated;
         case Scheme is
            when Parallel =>
               null;
            when Sequential =>
              Reversal : Matrix_Resource.Instance;
         end case;
      end record;

end Iderium.Media.Filter.Pair;
