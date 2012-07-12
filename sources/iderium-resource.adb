------------------------------------------------------------------------
-- Iderium.Resource
------------------------------------------------------------------------
-- Implementation notes:
--    The implementation uses a concept of reference counting.
------------------------------------------------------------------------

package body Iderium.Resource is

   -- INSTANCE ---------------------------------------------------------

   ---------------------------------------------------------------------
   -- Void
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    The resource is empty iff it does not have a reference counter.
   ---------------------------------------------------------------------
   function Void (Resource : Instance) return Boolean is
   begin
      return Resource.References = null;
   end Void;

   ---------------------------------------------------------------------
   -- Create
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    Allocates a new reference counter.
   ---------------------------------------------------------------------
   function Create (Object : Object_Type) return Instance is
      Result : Instance;
   begin
      Result.References := new Counter'(1);
      Result.Object     := Object;
      return Result;
   end Create;

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    Raises `Invalid_Resource` when the resource is empty.
   ---------------------------------------------------------------------
   function Get (Resource : Instance) return Object_Type is
   begin
      if Void (Resource) then
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
      if not Void (Resource) then
         Resource.References.all := Resource.References.all + 1;
      end if;
   end Adjust;

   ---------------------------------------------------------------------
   -- Finalize
   ---------------------------------------------------------------------
   -- Implementation notes:
   --    The reference counter then is set to `null` automatically.
   ---------------------------------------------------------------------
   procedure Finalize (Resource : in out Instance) is
   begin
      if not Void (Resource) then
         if Resource.References.all = 1 then
            Free (Resource.References);
            Free (Resource.Object); 
         else
            Resource.References.all := Resource.References.all - 1;
         end if;
      end if;
   end Finalize;

end Iderium.Resource;
