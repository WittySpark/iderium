------------------------------------------------------------------------
-- Iderium.Resource
------------------------------------------------------------------------
-- Purpose:
--    This package allows you to share dynamically allocated memory 
--    safely between several users (without double frees or leaks) and 
--    automatically release it when it is no longer needed.
-- Concept:
--    A resource may contain the only object of a particular type.
--    A resource is empty by default and contains no object.
--    You can create a new resource, storing the given access
--    to allocated object. The object will be deallocated automatically.
--    You can safely adjust one resource to another.
--    You can retrieve the access to the object stored in the resource.
------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic

   -- The type of the objects to store in the resources.
   type Object_Type (<>) is limited private;
   type Object_Access_Type is access Object_Type;

package Iderium.Resource is

   type Instance is private;

   ---------------------------------------------------------------------
   -- Is_Empty
   ---------------------------------------------------------------------
   -- Purpose:
   --    Returns TRUE iff the given resource is empty.
   -- Exceptions:
   --    None.
   ---------------------------------------------------------------------
   function Is_Empty (Resource : Instance) return Boolean;
   pragma Inline (Is_Empty);

   ---------------------------------------------------------------------
   -- Create
   ---------------------------------------------------------------------
   -- Purpose:
   --    Creates a new resource, storing the given access to an object.
   -- Exceptions:
   --    None.
   ---------------------------------------------------------------------
   function Create (Object : Object_Access_Type) return Instance;
   pragma Inline (Create);

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Purpose:
   --    Returns the access to the object stored in the given resource.
   --    Returns NULL for an empty resource.
   -- Exceptions:
   --    None.
   ---------------------------------------------------------------------
   function Get (Resource : Instance) return Object_Access_Type;
   pragma Inline (Get);

private

   procedure Free is 
     new Ada.Unchecked_Deallocation (Object_Type, Object_Access_Type);


   type Counter is new Natural;
   type Counter_Access is access Counter;

   procedure Free is 
     new Ada.Unchecked_Deallocation (Counter, Counter_Access);


   type Instance is new Ada.Finalization.Controlled with
      record
         User_Count : Counter_Access := null;
         Object     : Object_Access_Type;
      end record;

   procedure Adjust (Resource : in out Instance);
   pragma Inline (Adjust);

   procedure Finalize (Resource : in out Instance);
   pragma Inline (Finalize);

end Iderium.Resource;
