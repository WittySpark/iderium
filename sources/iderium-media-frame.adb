package body Iderium.Media.Frame is

   procedure Capture (Input : in out Signal_Type; 
                     Output : out Instance) is
   begin
      for I in Output'Range loop
         Capture (Input);
         exit when not Input.Active;
         Output(I) := Input.Sample;
      end loop;
   end Capture;

end Iderium.Media.Frame;
