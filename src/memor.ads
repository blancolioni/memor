private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Memor is

   pragma Preelaborate;

   type Database_Reference is private;
   Null_Database_Reference : constant Database_Reference;

   function To_String
     (Reference : Database_Reference)
      return String;

   type Root_Record_Type is abstract tagged limited private;

   function Reference (Item : Root_Record_Type'Class)
                       return Database_Reference;

   procedure After_Change
     (Item : Root_Record_Type)
   is null;

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
                       procedure (Item : not null access
                                    Root_Record_Type'Class))
   is abstract;

   function Element
     (Database  : Root_Database_Type;
      Reference : Database_Reference)
      return access constant Root_Record_Type'Class
      is abstract;

   type Object_Update_Interface is interface;

   procedure Update (Item : Object_Update_Interface) is abstract;

   type Memor_Update_List is private;

   procedure Add_Update
     (List   : in out Memor_Update_List;
      Update : Object_Update_Interface'Class);

   procedure Execute_Updates
     (List : in out Memor_Update_List);

   type Memor_Database is access all Root_Database_Type'Class;

   function Object_Database (Item : Root_Record_Type)
                             return Memor_Database
                             is abstract;

   type Identifier_Record_Type is limited interface;

   function Identifier (Item : Identifier_Record_Type) return String
     is abstract;

private

   type Database_Reference is new Natural;
   Null_Database_Reference : constant Database_Reference := 0;

   subtype Real_Database_Reference is
     Database_Reference range 1 .. Database_Reference'Last;

   type Root_Record_Type is abstract tagged limited
      record
         Ref : Database_Reference;
      end record;

   function Locking return Boolean;

   type Root_Database_Type is abstract tagged null record;

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Object_Update_Interface'Class);

   type Memor_Update_List is
     new Update_Lists.List with null record;

end Memor;
