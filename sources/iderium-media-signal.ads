------------------------------------------------------------------------
-- Iderium.Media.Signal
------------------------------------------------------------------------
-- Purpose:
--   This package allows you to work with an abstract signals.
-- Concept:
--   A signal is a sequence of samples.
--   In order to get the next sample of a signal you call `Capture` 
--   procedure. The captured sample then is stored in `Sample`. 
--   If there are no more samples in the signal, then `Active` is set 
--   to `false`. Otherwise, of course, `Active` is set to `true`.
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Iderium.Resource;

generic

   type Sample_Type is private;

package Iderium.Media.Signal is

   -- INSTANCE ---------------------------------------------------------

   type Instance is 
      abstract tagged limited record
         Active : Boolean;
         Sample : Sample_Type;
      end record;

   ---------------------------------------------------------------------
   -- Capture
   ---------------------------------------------------------------------
   -- Purpose:
   --    Extracts the next sample from `Signal`, updating its fields.
   --    Since is abstract, should be overridden.
   ---------------------------------------------------------------------
   procedure Capture (Signal : in out Instance) is abstract;

   type Instance_Access is access Instance'Class;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance'Class, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

end Iderium.Media.Signal;
