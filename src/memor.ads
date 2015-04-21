package Memor is

   pragma Preelaborate;

   type Database_Reference is private;
   Null_Database_Reference : constant Database_Reference;

   type Root_Record_Type is abstract tagged private;

   function Reference (Item : Root_Record_Type)
                       return Database_Reference;

   procedure Locking (Enabled : Boolean);

   type Root_Database_Type is abstract tagged private;

--     function Reference (Item   : Root_Database_Type;
--                         Object : Root_Record_Type'Class)
--                         return Database_Reference
--                         is abstract;

   function Database_Class_Name (Item : Root_Database_Type) return String
                                 is abstract;

   procedure Update (Item : Root_Database_Type;
                     Ref  : Memor.Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Root_Record_Type'Class))
     is abstract;

   function Object_Database (Item : Root_Record_Type)
                             return Root_Database_Type'Class
                             is abstract;

   type Identifier_Record_Type is interface;

   function Identifier (Item : Identifier_Record_Type) return String
     is abstract;

private

   type Database_Reference is new Natural;
   Null_Database_Reference : constant Database_Reference := 0;

   subtype Real_Database_Reference is
     Database_Reference range 1 .. Database_Reference'Last;

   type Root_Record_Type is abstract tagged
      record
         Reference : Database_Reference;
      end record;

   function Locking return Boolean;

   type Root_Database_Type is abstract tagged null record;

end Memor;
