with Ada.Unchecked_Deallocation;
with Iderium.Media.Signal;
with Iderium.Resource;

generic

   with package Signal is new Iderium.Media.Signal (<>);

package Iderium.Media.Frame is

   type Instance is array (Integer range <>) of Signal.Sample_Type;

   generic
      type Signal_Type is new Signal.Instance with private;
   procedure Grab (Input : in out Signal_Type; 
                     Output : out Instance);

   type Instance_Access is access Instance;

   procedure Free is
     new Ada.Unchecked_Deallocation (Instance, Instance_Access);


   package Resource is new Iderium.Resource (Instance_Access);


   type Broadcast_Direction is (Forward, Backward);
   
   type Broadcast (Frame : not null access Instance;
               Direction : Broadcast_Direction) is
     new Signal.Instance with private;

   overriding
   procedure Capture (B : in out Broadcast);
   pragma Inline (Capture);

private

   function Initialize_Broadcast (Direction : Broadcast_Direction;
                            First, Last : Integer) return Integer;

   type Broadcast (Frame : not null access Instance;
               Direction : Broadcast_Direction) is
     new Signal.Instance with
      record
         Current : Integer := Initialize_Broadcast (Direction,
                                                    Frame'First,
                                                    Frame'Last);
      end record;

end Iderium.Media.Frame;
