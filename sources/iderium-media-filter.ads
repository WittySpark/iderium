------------------------------------------------------------------------
-- Iderium.Media.Filter
------------------------------------------------------------------------
-- Purpose:
--    This package provides a generic causal recursive filter interface.
--    It works with arbitrary real types and signal types.
-- Concept:
--    A filter takes input signal and transforms it in the form:
--      out := A(0) * in + <A(1..M), I> + <B(1..N), F>,
--    where 
--          `out` is current output sample,
--           `in` is current input sample, 
--            `I` is current input buffer (last M input samples),
--            `F` is current feedback buffer (last N output samples),
--      `A(0..M)` are input coefficients,
--      `B(1..N)` are feedback coefficients,
--        `<*,*>` denotes a dot-product.
--    In terms of Z-transform, filter's transfer function is given by:
--      F(z) = (\sum_{i=0}^{M} A(i) z^-i) / (\sum_{i=1}^N B(i) z^-i).
------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;
with Iderium.Media.Signal;

generic

   -- Also defines the real type to be used by filter.
   with package Arrays is new Ada.Numerics.Generic_Real_Arrays (<>);

   -- Also defines the sample type to be used by filter.
   with package Signal is new Iderium.Media.Signal (<>);

   -- This operation must be defined on samples.
   with function "*" (Left : Arrays.Real; Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- This operation must be defined on samples.
   with function "+" (Left : Signal.Sample_Type; 
                     Right : Signal.Sample_Type)
     return Signal.Sample_Type is <>;

   -- A concrete input signal type.
   type Input_Type is new Signal.Instance with private;

package Iderium.Media.Filter is

   -- Allows you to control a filter's buffer.
   package Buffer is

      -- It works as a sliding vector of length `Size`.
      -- `Capacity` should be comparable to the length of 
      -- the signals to be processed and must not be less
      -- than `Size`.
      type Instance (Size : Natural; Capacity : Natural) is private;

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
        array (Natural range <>) of Signal.Sample_Type;

      -- `Current` points to the last available slot in `Data`.
      -- Current buffer is stored in `Data(Current+1..Current+Size)`.
      type Instance (Size : Natural; Capacity : Natural) is
         record
            Data    : Buffer_Data (1 .. Capacity);
            Current : Integer := Capacity - Size;
         end record;

   end Buffer;


   -- Filter descriptor (or context).
   -- `M` defines a number of input coefficients,
   -- `N` defines a number of feedback coefficients,
   -- `Capacity` is used for both buffers.
   type Instance (M : Natural; N : Natural; Capacity : Natural) is 
      tagged record
         A : Arrays.Real_Vector (0 .. M);
         B : Arrays.Real_Vector (1 .. N);
         I : Buffer.Instance (M, Capacity);
         F : Buffer.Instance (N, Capacity);
      end record;


   type Output (Context : not null access Instance;
                  Input : not null access Input_Type) is
     new Signal.Instance with null record;

   overriding
   procedure Capture (Filter : in out Output);
   pragma Inline (Capture);

end Iderium.Media.Filter;
