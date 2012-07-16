------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Purpose:
--    This package allows you to work with linear recursive systems.
-- Concept:
--    A linear recursive system can be decomposed into causal and
--    anti-causal parts, which form a filter pair.
--    Causal filters are used to process infinite signals, and 
--    filter pairs are used to process finite signal frames.
--    A causal filter transforms the input samples as follows:
--      out := A * in + <B, I> + <C, F>,
--    where `out` is current output sample,
--           `in` is current input sample, 
--            `I` is current input buffer (last M input samples),
--            `F` is current feedback buffer (last N output samples),
--            `A` is the main coefficient,
--            `B` is a vector of input coefficients,
--            `C` is a vector of feedback coefficients,
--       `<*, *>` denotes a dot product.
--    The filter's transfer function (in terms of Z-transform) H(z) is
--      (A + \sum_{i=1}^{M} B(i) z^-i) / (1 - \sum_{i=1}^N C(i) z^-i).
--    A filter pair transforms a signal frame depending on the
--    connection scheme it uses. There are two connection schemes:
--      - parallel: out := Forward[in] + Backward[in],
--      - sequential: out := Backward[Forward[in]].
--    A filter pair assumes a nearest-neighbour extrapolation outside
--    the input frame and correctly handles a boundary condition when
--    the sequential connection scheme is used.
------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Unchecked_Deallocation;
with Iderium.Media.Frame;
with Iderium.Media.Signal;
with Iderium.Resource;

generic

   -- Also defines a real type to be used.
   with package Arrays is new Ada.Numerics.Generic_Real_Arrays (<>);

   -- Also defines a sample type to be used.
   with package Signal is new Iderium.Media.Signal (<>);

   -- This operator must be defined on samples.
   with function "*" (Left : Arrays.Real; Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- This operator must be defined on samples.
   with function "+" (Left, Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- A concrete input signal type to work with.
   type Input_Type is new Signal.Instance with private;

   -- The more `Buffer_Capacity` is, the longer `Push` works fast,
   -- and the more memory is used.
   Buffer_Capacity : Natural := 0;

package Iderium.Media.Filter is

   ---------------------------------------------------------------------
   -- BUFFER -----------------------------------------------------------
   ---------------------------------------------------------------------

   package Buffer is

      -- INSTANCE ------------------------------------------------------

      -- Can be seen as a vector of length `Size`.
      type Instance (Size : Natural) is private;

      ------------------------------------------------------------------
      -- Dot
      ------------------------------------------------------------------
      -- Purpose:
      --    Adds <`Vector`, `Buffer`> to `Output`.
      ------------------------------------------------------------------
      procedure Dot (Vector : Arrays.Real_Vector; 
                     Buffer : Instance; 
                     Output : in out Signal.Sample_Type);
      pragma Inline (Dot);

      ------------------------------------------------------------------
      -- Push
      ------------------------------------------------------------------
      -- Purpose:
      --    Shifts `Buffer` down and puts `Sample` to the first slot.
      ------------------------------------------------------------------
      procedure Push (Buffer : in out Instance;
                      Sample : Signal.Sample_Type);
      pragma Inline (Push);

      ------------------------------------------------------------------
      -- Fill
      ------------------------------------------------------------------
      -- Purpose:
      --    Assigns `Sample` to each element of `Buffer`.
      ------------------------------------------------------------------
      procedure Fill (Buffer : in out Instance;
                      Sample : Signal.Sample_Type);
      pragma Inline (Fill);

      ------------------------------------------------------------------
      -- Mix
      ------------------------------------------------------------------
      -- Purpose:
      --    Replaces `Buffer` with the following linear combination:
      --      (1.0 - `Factor`) * `Buffer` + `Factor` * `Sample`.
      ------------------------------------------------------------------
      procedure Mix (Buffer : in out Instance; 
                     Sample : Signal.Sample_Type;
                     Factor : Arrays.Real);
      pragma Inline (Mix);

      ------------------------------------------------------------------
      -- Rotate
      ------------------------------------------------------------------
      -- Purpose:
      --    Multiplies column vector `Buffer` on the left by `Matrix`.
      ------------------------------------------------------------------
      procedure Rotate (Buffer : in out Instance; 
                        Matrix : Arrays.Real_Matrix);
      pragma Inline (Rotate);

   private

      -- INSTANCE ------------------------------------------------------

      type Buffer_Data is 
        array (Integer range <>) of Signal.Sample_Type;

      -- `Current` points to the last available slot in `Data`.
      -- Current buffer is stored in `Data(Current+1..Current+Size)`.
      type Instance (Size : Natural) is
         record
            Data    : Buffer_Data (-Buffer_Capacity + 1 .. Size);
            Current : Integer := 0;
         end record;

   end Buffer;

   -- INSTANCE ---------------------------------------------------------

   type Instance (M : Natural; N : Natural) is 
      tagged record
         A : Arrays.Real;
         B : Arrays.Real_Vector (1 .. M);
         C : Arrays.Real_Vector (1 .. N);
         I : Buffer.Instance (M);
         F : Buffer.Instance (N);
      end record;

   ---------------------------------------------------------------------
   -- Equilibrium
   ---------------------------------------------------------------------
   -- Purpose:
   --    Returns a real constant R such, that:
   --      R * in = A * in + <B, {in}> + <C, {R * in}>,
   --    where {x} is a vector [x x .. x]'.
   --    Primarily used in constant signal frame extrapolation.
   ---------------------------------------------------------------------
   function Equilibrium (Filter : Instance) return Arrays.Real;

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

   -- OUTPUT -----------------------------------------------------------

   type Output (Context : not null access Instance;
                  Input : not null access Input_Type) is
     new Signal.Instance with null record;

   overriding
   procedure Capture (Filter : in out Output);
   pragma Inline (Capture);

   ---------------------------------------------------------------------
   -- PAIR -------------------------------------------------------------
   ---------------------------------------------------------------------

   package Pair is

      -- INSTANCE ------------------------------------------------------

      type Connection_Scheme is (Parallel, Sequential);

      type Instance (Scheme : Connection_Scheme) is private;

      function Create (Scheme : Connection_Scheme;
            Forward, Backward : Filter.Instance) return Instance;

      generic
         with package Frame is new Iderium.Media.Frame (Signal);
      procedure Apply (Pair : Instance; Data : in out Frame.Instance);

   private

      -- INSTANCE ------------------------------------------------------

      type Matrix_Access is access Arrays.Real_Matrix;

      procedure Free is
        new Ada.Unchecked_Deallocation (Arrays.Real_Matrix, 
                                             Matrix_Access);

      package Matrix_Resource is new Iderium.Resource (Matrix_Access);

      type Instance (Scheme : Connection_Scheme) is
         record
            Forward  : Filter.Resource.Instance;
            Backward : Filter.Resource.Instance;
            FE, BE   : Arrays.Real;
            case Scheme is
               when Parallel =>
                  null;
               when Sequential =>
                 Reversal : Matrix_Resource.Instance;
            end case;
         end record;

   end Pair;

private



end Iderium.Media.Filter;
