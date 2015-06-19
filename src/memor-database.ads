private with Ada.Containers.Vectors;

generic
   Class_Name : String;
   type Element_Type is new Root_Record_Type with private;
   type Element_Reference is access constant Element_Type'Class;
package Memor.Database is

   type Cursor is private;

   Null_Cursor : constant Cursor;

   function Has_Element (Item : Cursor) return Boolean;
   function Reference (Item : Cursor) return Element_Reference;

   function Create (Creator : not null access procedure
                      (Item : in out Element_Type'Class))
                    return Database_Reference;

   procedure Create (Creator : not null access procedure
                       (Item : in out Element_Type'Class));

   function Create (Creator : not null access procedure
                      (Item : in out Element_Type'Class))
                    return Element_Reference;

   procedure Create (Ref    : Database_Reference;
                     Creator : not null access procedure
                       (Item : in out Element_Type'Class));

   procedure Delete (Item : not null access Element_Type'Class);

   function Element (Ref : Database_Reference) return Element_Type'Class;
   function Reference (Ref : Database_Reference) return Element_Reference;
   function Reference (Item : Element_Type'Class) return Element_Reference;

   --     function Locked_Element
   --  (Ref : Database_Reference)
   --  return Element_Access;
--     procedure Unlock (Item  : Element_Access);

   procedure Update (Ref : Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Element_Type'Class));

   generic
      type User_Data (<>) is private;
   procedure Generic_Update (Ref   : Database_Reference;
                             Data  : User_Data;
                             Updater : not null access
                               procedure (Item : in out Element_Type'Class;
                                          Data : in     User_Data));

   procedure Scan (Process : not null access
                     procedure (Item : Element_Type'Class));

   procedure Scan
     (Match   : not null access
        function (Item : Element_Type'Class)
      return Boolean;
      Process : not null access
        procedure (Item : Element_Type'Class));

   procedure Scan
     (Process : not null access
        procedure (Item : Element_Reference));

   procedure Iterate (Process : not null access
                        procedure (Item : in out Element_Type'Class));

   procedure Iterate (Match : not null access
                      function (Item : Element_Type'Class)
                      return Boolean;
                      Process : not null access
                        procedure (Item : in out Element_Type'Class));

   function Search (Match : not null access
                      function (Item : Element_Type'Class)
                      return Boolean)
                   return Cursor;

   function Search (Identifier : String) return Cursor;
   function Exists (Identifier : String) return Boolean;
   function Get (Identifier : String) return Element_Reference
   with Pre => Exists (Identifier);

   function Last_Index return Database_Reference;

   function Count_Matching
     (Match : not null access
        function (Item : Element_Type'Class) return Boolean)
      return Natural;

   procedure Lock;
   procedure Unlock;

   type Element_List is tagged private;

   function Count (List : Element_List) return Natural;
   function Reference (List : Element_List;
                       Index : Positive)
                       return Element_Reference;
   procedure Add (List : in out Element_List;
                  Item : Element_Reference);

   function Get_Database return Root_Database_Type'Class;

--     function Random_Element return Element_Access;
--     function Random_Reference return Element_Reference;

private

   package Database_Reference_Vectors is
     new Ada.Containers.Vectors (Positive, Database_Reference);

   type Element_List is new Database_Reference_Vectors.Vector
   with null record;

   type Cursor is
      record
         Has_Element : Boolean;
         Reference   : Database_Reference;
      end record;

   Null_Cursor : constant Cursor := (False, 1);

end Memor.Database;
