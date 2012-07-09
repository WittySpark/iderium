------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Purpose:
--    This package provides a generic causal recursive filter interface.
--    It works with arbitrary real types and signal types.
-- Concept:
--    A filter takes input signal samples and transforms them to:
--      out := A * in + <B, I> + <C, F>,
--    where 
--          `out` is current output sample,
--           `in` is current input sample, 
--            `I` is current input buffer (last M input samples),
--            `F` is current feedback buffer (last N output samples),
--            `A` is the main coefficient,
--            `B` are input coefficients,
--            `C` are feedback coefficients,
--        `<*,*>` denotes a dot-product.
--    The filter's transfer function (in terms of Z-transform) H(z) is
--      (A + \sum_{i=1}^{M} B(i) z^-i) / (1 - \sum_{i=1}^N C(i) z^-i).
------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;
with Iderium.Media.Signal;

generic

   -- Also defines a real type to be used by filter.
   with package Arrays is new Ada.Numerics.Generic_Real_Arrays (<>);

   -- Also defines a sample type to be used by filter.
   with package Signal is new Iderium.Media.Signal (<>);

   -- This operation must be defined on samples.
   with function "*" (Left : Arrays.Real; Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- This operation must be defined on samples.
   with function "+" (Left : Signal.Sample_Type; 
                     Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- A concrete input signal type to work with.
   type Input_Type is new Signal.Instance with private;

   -- The more `Buffer_Capacity` is, the longer `Push` works fast,
   -- and the more memory is used.
   Buffer_Capacity : Natural := 0;

package Iderium.Media.Filter is

   package Buffer is

      -- It works as a sliding vector of length `Size`.
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
      --    Shifts `Buffer` and puts `Sample` to its first slot.
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
      --      (1 - `Factor`) * `Buffer` + `Factor` * `Sample`.
      ------------------------------------------------------------------
      procedure Mix (Buffer : in out Instance; 
                     Sample : Signal.Sample_Type;
                     Factor : Arrays.Real);
      pragma Inline (Mix);

      ------------------------------------------------------------------
      -- Rotate
      ------------------------------------------------------------------
      -- Purpose:
      --    Multiplies the whole `Buffer` by `Matrix`.
      ------------------------------------------------------------------
      procedure Rotate (Buffer : in out Instance; 
                        Matrix : Arrays.Real_Matrix);
      pragma Inline (Rotate);

   private

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


   type Instance (M : Natural; N : Natural) is 
      tagged record
         A : Arrays.Real;
         B : Arrays.Real_Vector (1 .. M);
         C : Arrays.Real_Vector (1 .. N);
         I : Buffer.Instance (M);
         F : Buffer.Instance (N);
      end record;


   type Output (Context : not null access Instance;
                  Input : not null access Input_Type) is
     new Signal.Instance with null record;

   overriding
   procedure Capture (Filter : in out Output);
   pragma Inline (Capture);

end Iderium.Media.Filter;
