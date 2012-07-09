------------------------------------------------------------------------
-- Iderium.Media.Signal
------------------------------------------------------------------------
-- Purpose:
--   This package allows you to work with an abstract signals.
-- Concept:
--   A signal is a sequence of samples.
--   When you capture a signal, the next sample from the signal is 
--   extracted to the `Sample`. If there are no samples anymore, then 
--   Active is set to FALSE.
------------------------------------------------------------------------

with Iderium.Resource;

generic

   type Sample_Type is private;

package Iderium.Media.Signal is

   type Instance is 
      abstract tagged limited record
         Active : Boolean := True;
         Sample : Sample_Type;
      end record;

   procedure Capture (Signal : in out Instance) is abstract;

end Iderium.Media.Signal;
