------------------------------------------------------------------------
-- Iderium.Media.Earf
------------------------------------------------------------------------
-- Purpose:
-- Concept:
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Iderium.Media.Signal;
with Iderium.Media.Filter;
with Iderium.Resource;

generic

   with package Filter is new Iderium.Media.Filter (<>);

   with package Real_Signal is
     new Iderium.Media.Signal (Sample_Type => Filter.Arrays.Real);

   type Edges_Type is new Real_Signal.Instance with private;

package Iderium.Media.Earf is

   type Instance is private;

   function Create (Base : Filter.Instance) return Instance;

   type Instance_Access is access Instance;

   procedure Free (Name : in out Instance_Access);


   package Resource is new Iderium.Resource (Instance_Access);


   type Output (Context : not null access Instance; 
                  Input : not null access Filter.Input_Type;
                  Edges : not null access Edges_Type) is
     new Filter.Signal.Instance with null record;

   overriding
   procedure Capture (Earf : in out Output);
   pragma Inline (Capture);

private

   type Instance is
      record
         Base : Filter.Resource.Instance;
         R    : Filter.Arrays.Real;
      end record;

   procedure Deallocate is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

end Iderium.Media.Earf;
