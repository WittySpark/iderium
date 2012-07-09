------------------------------------------------------------------------
-- Iderium.Media.Earf
------------------------------------------------------------------------
-- Purpose:
--    This package provides a generic causal recursive filter interface.
--    It works with arbitrary real types and signal types.
-- Concept:
------------------------------------------------------------------------

with Iderium.Media.Signal;
with Iderium.Media.Filter;

generic

   with package Filter is new Iderium.Media.Filter (<>);

   with package Real_Signal is
     new Iderium.Media.Signal (Sample_Type => Filter.Arrays.Real);

   type Edges_Type is new Real_Signal.Instance with private;

package Iderium.Media.Earf is

   type Instance is new Filter.Instance with private;

   function Create (Base : Filter.Instance) return Instance;


   type Output (Context : not null access Instance; 
                  Input : not null access Filter.Input_Type;
                  Edges : not null access Edges_Type) is
     new Filter.Signal.Instance with null record;

   overriding
   procedure Capture (Earf : in out Output);

private

   type Instance is new Filter.Instance with
      record
         R : Filter.Arrays.Real;
      end record;

end Iderium.Media.Earf;
