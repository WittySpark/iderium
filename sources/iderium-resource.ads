------------------------------------------------------------------------
-- Iderium.Resource
------------------------------------------------------------------------
-- Purpose:
--    This package allows you to share an object among several users.
--    The last user calls `Free` procedure for the object.
-- Concept:
--    The most often use of this package - safe dynamically allocated
--    memory sharing, when `Object_Type` is an access type. Though
--    it could be utilized for other purposes, such as file handle
--    sharing, etc.
------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic

   -- The type of the objects to store in the resources.
   type Object_Type is private;

   -- The procedure to call when the stored object is no longer needed.
   with procedure Free (Object : in out Object_Type) is <>;

package Iderium.Resource is

   -- INSTANCE ---------------------------------------------------------

   type Instance is private;

   Invalid_Resource : exception;

   ---------------------------------------------------------------------
   -- Void
   ---------------------------------------------------------------------
   -- Purpose:
   --    Returns `True` iff the given resource is empty.
   -- Exceptions:
   --    None.
   ---------------------------------------------------------------------
   function Void (Resource : Instance) return Boolean;
   pragma Inline (Void);

   ---------------------------------------------------------------------
   -- Create
   ---------------------------------------------------------------------
   -- Purpose:
   --    Creates a new resource storing the given object.
   -- Exceptions:
   --    None.
   ---------------------------------------------------------------------
   function Create (Object : Object_Type) return Instance;
   pragma Inline (Create);

   ---------------------------------------------------------------------
   -- Get
   ---------------------------------------------------------------------
   -- Purpose:
   --    Returns the object stored in the given resource.
   -- Exceptions:
   --    Invalid_Resource.
   ---------------------------------------------------------------------
   function Get (Resource : Instance) return Object_Type;
   pragma Inline (Get);

private

   -- COUNTER ----------------------------------------------------------

   type Counter is new Natural;

   type Counter_Access is access Counter;

   procedure Free is 
     new Ada.Unchecked_Deallocation (Counter, Counter_Access);

   -- INSTANCE ---------------------------------------------------------

   type Instance is new Ada.Finalization.Controlled with
      record
         References : Counter_Access := null;
         Object     : Object_Type;
      end record;

   procedure Adjust (Resource : in out Instance);
   pragma Inline (Adjust);

   procedure Finalize (Resource : in out Instance);
   pragma Inline (Finalize);

end Iderium.Resource;
