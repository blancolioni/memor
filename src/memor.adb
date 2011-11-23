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
     (Item : Root_Record_Type)
      return Database_Reference
   is
   begin
      return Item.Reference;
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

end Memor;
