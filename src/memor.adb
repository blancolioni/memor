package body Memor is

   Locking_Enabled : Boolean := True;

   -------------
   -- Locking --
   -------------

   procedure Locking (Enabled : Boolean) is
   begin
      Locking_Enabled := Enabled;
   end Locking;

   -------------
   -- Locking --
   -------------

   function Locking return Boolean is
   begin
      return Locking_Enabled;
   end Locking;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Item : Root_Record_Type'Class)
      return Database_Reference
   is
   begin
      return Item.Ref;
   end Reference;

   -------------------
   -- Set_Reference --
   -------------------

--     procedure Set_Reference (Item : in out Root_Record_Type;
--                              Value : Database_Reference)
--     is
--     begin
--        Item.Reference := Value;
--     end Set_Reference;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Reference : Database_Reference)
      return String
   is
      Result : constant String := Database_Reference'Image (Reference);
   begin
      return Result (2 .. Result'Last);
   end To_String;

end Memor;
