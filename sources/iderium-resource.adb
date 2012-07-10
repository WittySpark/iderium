------------------------------------------------------------------------
-- Iderium.Resource
------------------------------------------------------------------------
-- Implementation notes:
--    The implementation uses a concept of reference counting.
------------------------------------------------------------------------

package body Iderium.Resource is

   ---------------------------------------------------------------------
   -- Is_Empty
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    The resource is empty iff it does not have a user counter.
   ---------------------------------------------------------------------
   function Is_Empty (Resource : Instance) return Boolean is
   begin
      return Resource.User_Count = null;
   end Is_Empty;

   ---------------------------------------------------------------------
   -- Create
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    Allocates a new user counter.
   ---------------------------------------------------------------------
   function Create (Object : Object_Type) return Instance is
      Result : Instance;
   begin
      Result.User_Count := new Counter'(1);
      Result.Object := Object;
      return Result;
   end Create;

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    Raises `Invalid_Resource` exception when the resource is empty.
   ---------------------------------------------------------------------
   function Get (Resource : Instance) return Object_Type is
   begin
      if Is_Empty (Resource) then
         raise Invalid_Resource;
      end if;
      return Resource.Object;
   end Get;


   ---------------------------------------------------------------------
   -- Adjust
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    Just increases the counter.
   ---------------------------------------------------------------------
   procedure Adjust (Resource : in out Instance) is
   begin
      if not Is_Empty (Resource) then
         Resource.User_Count.all := Resource.User_Count.all + 1;
      end if;
   end Adjust;

   ---------------------------------------------------------------------
   -- Finalize
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    The last user calls `Free` procedure.
   --    The user counter then is set to NULL automatically.
   ---------------------------------------------------------------------
   procedure Finalize (Resource : in out Instance) is
   begin
      if not Is_Empty (Resource) then
         if Resource.User_Count.all = 1 then
            Free (Resource.User_Count);
            Free (Resource.Object); 
         else
            Resource.User_Count.all := Resource.User_Count.all - 1;
         end if;
      end if;
   end Finalize;

end Iderium.Resource;
