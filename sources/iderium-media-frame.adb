package body Iderium.Media.Frame is

   procedure Grab (Input : in out Signal_Type; 
                     Output : out Instance) is
   begin
      for I in Output'Range loop
         Capture (Input);
         exit when not Input.Active;
         Output(I) := Input.Sample;
      end loop;
   end Grab;


   procedure Capture (B : in out Broadcast) is
   begin
      B.Active := B.Current in B.Frame'Range;
      if B.Active then
         B.Sample := B.Frame(B.Current);
         case B.Direction is
            when Forward =>
               B.Current := B.Current + 1;
            when Backward =>
               B.Current := B.Current - 1;
         end case;
      end if;
   end Capture;


   function Initialize_Broadcast (Direction : Broadcast_Direction;
                                First, Last : Integer) return Integer is
   begin
      case Direction is
         when Forward =>
            return First;
         when Backward =>
            return Last;
      end case;
   end Initialize_Broadcast;

end Iderium.Media.Frame;
