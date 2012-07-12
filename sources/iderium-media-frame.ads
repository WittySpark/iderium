with Ada.Unchecked_Deallocation;
with Iderium.Media.Signal;
with Iderium.Resource;

generic

   type Sample_Type is private;

package Iderium.Media.Frame is

   type Instance is array (Integer range <>) of Sample_Type;

   generic
      with package Signal is new Iderium.Media.Signal (Sample_Type);
      type Signal_Type is new Signal.Instance with private;
   procedure Capture (Input : in out Signal_Type; 
                     Output : out Instance);

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);

   package Resource is new Iderium.Resource (Instance_Access);

end Iderium.Media.Frame;
