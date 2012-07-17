------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Purpose:
--    This package allows you to work with causal recursive filters.
-- Concept:
-- TODO   A linear recursive system can be decomposed into causal and
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
with Iderium.Media.Buffer;
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

   -- The more `Buffer_Capacity` is, the longer `Push` works fast,
   -- and the more memory is used.
   Buffer_Capacity : Natural := 0;

package Iderium.Media.Filter is

   -- BUFFER -----------------------------------------------------------

   package Frame is new Iderium.Media.Frame (Signal);

   package Buffer is new Iderium.Media.Buffer (Frame, Buffer_Capacity);

   ---------------------------------------------------------------------
   -- Dot
   ---------------------------------------------------------------------
   -- Purpose:
   --    Adds <`Left`, `Right`> to `Result`.
   ---------------------------------------------------------------------
   procedure Dot (Left   : Arrays.Real_Vector; 
                  Right  : Buffer.Instance; 
                  Result : in out Signal.Sample_Type);
   pragma Inline (Dot);

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
   --    Returns a real constant E such, that:
   --      E = A + <B, {1}> + <C, {E}>,
   --    where {x} is a vector [x x .. x].
   --    Primarily used in constant signal frame extrapolation.
   ---------------------------------------------------------------------
   function Equilibrium (Filter : Instance) return Arrays.Real;

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

   -- OUTPUT -----------------------------------------------------------

   type Output (Context : not null access Instance;
                  Input : not null access Signal.Instance'Class) is
     new Signal.Instance with null record;

   ---------------------------------------------------------------------
   -- Capture
   ---------------------------------------------------------------------
   -- Purpose:
   --    Performs one step of the recursive filtering.
   --    Updates `Filter`'s buffers.
   ---------------------------------------------------------------------
   overriding
   procedure Capture (Filter : in out Output);
   pragma Inline (Capture);

end Iderium.Media.Filter;
