--  with Ada.Numerics.Discrete_Random;

package body Memor.Database is

   --  Debug_Memor : constant Boolean := False;

--     package Reference_Random is
--        new Ada.Numerics.Discrete_Random (Database_Reference);

   --  Gen : Reference_Random.Generator;

   type Element_Access is access all Element_Type'Class;

   type Local_Database_Type is
     new Root_Database_Type with null record;

   overriding
   function Database_Class_Name (Item : Local_Database_Type) return String;

   overriding
   procedure Update (Item    : Local_Database_Type;
                     Ref     : Memor.Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Root_Record_Type'Class));

   Db_Object : Local_Database_Type;

   protected type Db_Entry (Item : access Element_Type'Class) is
      entry Begin_Fetch;
      procedure End_Fetch;
      entry Lock;
      procedure Unlock;
   private
      Fetching_Count : Natural := 0;
      Locked         : Boolean := False;
   end Db_Entry;

   type Db_Entry_Access is access Db_Entry;

   package Db_Vectors is
      new Ada.Containers.Vectors (Database_Reference, Db_Entry_Access);

   protected Db is
      entry Lock;
      procedure Unlock;

      procedure Insert (Item : not null access Element_Type'Class);
      procedure Delete (Item : not null access Element_Type'Class);
      procedure Set (Index : Database_Reference;
                     Item  : Element_Access);

      function Element (Ref : Database_Reference) return Db_Entry_Access;
      function Has_Element (Ref : Database_Reference) return Boolean;
      function Last_Index return Database_Reference'Base;
   private
      V              : Db_Vectors.Vector;
      Db_Lock        : Boolean := False;
      Rec_Lock_Count : Natural := 0;
   end Db;

   --------
   -- Db --
   --------

   protected body Db is

      ------------
      -- Delete --
      ------------

      procedure Delete (Item : not null access Element_Type'Class) is
      begin
         V.Delete (Root_Record_Type (Item.all).Reference);
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Ref : Database_Reference) return Db_Entry_Access is
      begin
         return V.Element (Ref);
      end Element;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (Ref : Database_Reference) return Boolean is
      begin
         return V.Element (Ref) /= null;
      end Has_Element;

      ------------
      -- Insert --
      ------------

      procedure Insert (Item : not null access Element_Type'Class) is
      begin
         V.Append (new Db_Entry (Item));
      end Insert;

      ----------------
      -- Last_Index --
      ----------------

      function Last_Index return Database_Reference'Base is
      begin
         return V.Last_Index;
      end Last_Index;

      ----------
      -- Lock --
      ----------

      entry Lock when not Db_Lock and then Rec_Lock_Count = 0 is
      begin
         Db_Lock := True;
      end Lock;

      ---------
      -- Set --
      ---------

      procedure Set (Index : Database_Reference;
                     Item  : Element_Access)
      is
      begin
         while V.Last_Index < Index loop
            V.Append (null);
         end loop;
         V.Replace_Element (Index, new Db_Entry (Item));
      end Set;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Db_Lock := False;
      end Unlock;

   end Db;

   protected body Db_Entry is

      -----------------
      -- Begin_Fetch --
      -----------------

      entry Begin_Fetch when not Locked is
      begin
         Fetching_Count := Fetching_Count + 1;
      end Begin_Fetch;

      ---------------
      -- End_Fetch --
      ---------------

      procedure End_Fetch is
      begin
         Fetching_Count := Fetching_Count - 1;
      end End_Fetch;

      ----------
      -- Lock --
      ----------

      entry Lock when Fetching_Count = 0 and then not Locked is
      begin
         Locked := True;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         pragma Assert (Fetching_Count = 0);
         pragma Assert (Locked);
         Locked := False;
      end Unlock;

   end Db_Entry;

   ---------
   -- Add --
   ---------

   procedure Add (List : in out Element_List;
                  Item : Element_Reference)
   is
   begin
      List.Append (Item.Reference);
   end Add;

   -----------
   -- Count --
   -----------

   function Count (List : Element_List) return Natural is
   begin
      return List.Last_Index;
   end Count;

   ------------
   -- Create --
   ------------

   function Create (Creator : not null access procedure
                      (Item : in out Element_Type'Class))
                    return Database_Reference
   is
      Item : constant Element_Access := new Element_Type;
   begin
      Db.Insert (Item);
      Memor.Root_Record_Type (Item.all).Reference := Db.Last_Index;
      Creator (Item.all);
      return Item.Reference;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Creator : not null access procedure
                       (Item : in out Element_Type'Class))
   is
      Ref : constant Database_Reference :=
              Create (Creator);
      pragma Unreferenced (Ref);
   begin
      null;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Ref    : Database_Reference;
                     Creator : not null access procedure
                       (Item : in out Element_Type'Class))
   is
      Item : constant Element_Access := new Element_Type;
   begin
      Memor.Root_Record_Type (Item.all).Reference := Ref;
      Db.Set (Ref, Item);
      Creator (Item.all);
   end Create;

   function Create (Creator : not null access procedure
                      (Item : in out Element_Type'Class))
                    return Element_Reference
   is
      Index : constant Database_Reference :=
                Create (Creator);
   begin
      return Reference (Index);
   end Create;

   -------------------------
   -- Database_Class_Name --
   -------------------------

   overriding
   function Database_Class_Name (Item : Local_Database_Type) return String is
      pragma Unreferenced (Item);
   begin
      return Class_Name;
   end Database_Class_Name;

   ------------
   -- Delete --
   ------------

   procedure Delete (Item : not null access Element_Type'Class) is
   begin
      Db.Delete (Item);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Ref : Database_Reference) return Element_Type'Class is
      It : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if Locking then
         It.Begin_Fetch;
         declare
            Result : constant Element_Type'Class := It.Item.all;
         begin
            It.End_Fetch;
            return Result;
         end;
      else
         return It.Item.all;
      end if;
   end Element;

   --------------------
   -- Generic_Update --
   --------------------

   procedure Generic_Update (Ref   : Database_Reference;
                             Data  : User_Data;
                             Updater : not null access
                               procedure (Item : in out Element_Type'Class;
                                          Data : in     User_Data))
   is
      E      : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if Locking then
         E.Lock;
      end if;
      declare
         Item : constant Element_Access := Element_Access (E.Item);
      begin
         Updater (Item.all, Data);
         E.Unlock;
      end;
   end Generic_Update;

   ---------
   -- Get --
   ---------

   function Get (Identifier : String) return Element_Reference is
      Result : constant Cursor := Search (Identifier);
   begin
      if Has_Element (Result) then
         return Reference (Result);
      else
         raise Constraint_Error
           with "no such " & Class_Name & ": " & Identifier;
      end if;
   end Get;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database return Root_Database_Type'Class is
   begin
      return Db_Object;
   end Get_Database;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Item.Has_Element;
   end Has_Element;

   ------------
   -- Insert --
   ------------

--     procedure Insert (Item : Element_Access) is
--     begin
--        Db.Insert (Item);
--        Memor.Root_Record_Type (Item.all).Reference := Db.Last_Index;
--        --  Item.Set_Reference (Db.Last_Index);
--     end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
                        procedure (Item : in out Element_Type'Class))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E      : constant Db_Entry_Access := Db.Element (I);
         begin
            if Locking then
               E.Lock;
            end if;
            declare
               Item : constant Element_Access := Element_Access (E.Item);
            begin
               Process (Item.all);
               E.Unlock;
            end;
         end;
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Match : not null access
                      function (Item : Element_Type'Class)
                      return Boolean;
                      Process : not null access
                        procedure (Item : in out Element_Type'Class))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E      : constant Db_Entry_Access := Db.Element (I);
         begin
            if Locking then
               E.Lock;
            end if;
            declare
               Item : constant Element_Access := Element_Access (E.Item);
            begin
               if Match (Item.all) then
                  Process (Item.all);
               end if;
               E.Unlock;
            end;
         end;
      end loop;
   end Iterate;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index return Database_Reference'Base is
   begin
      return Db.Last_Index;
   end Last_Index;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      if Locking then
         Db.Lock;
      end if;
   end Lock;

   --------------------
   -- Locked_Element --
   --------------------

   --     function Locked_Element
   --  (Ref : Database_Reference)
   --  return Element_Access
   --  is
--        E      : Db_Entry_Access := Db.Element (Ref);
--     begin
--        if Locking then
--           E.Lock;
--        end if;
--        return Element_Access (E.Item);
--     end Locked_Element;

   --------------------
   -- Random_Element --
   --------------------

--     function Random_Element return Element_Access is
--        Ref : constant Database_Reference :=
--                Reference_Random.Random (Gen) mod Last_Index + 1;
--     begin
--        return Locked_Element (Ref);
--     end Random_Element;

   ----------------------
   -- Random_Reference --
   ----------------------

--     function Random_Reference return Element_Reference is
--        Ref : constant Database_Reference :=
--                Reference_Random.Random (Gen) mod Last_Index + 1;
--     begin
--        return Reference (Ref);
--     end Random_Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference (Ref : Database_Reference) return Element_Reference is
      It : constant Db_Entry_Access := Db.Element (Ref);
   begin
      return Element_Reference (It.Item);
   end Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference (Item : Cursor) return Element_Reference is
      It : constant Db_Entry_Access := Db.Element (Item.Reference);
   begin
      return Element_Reference (It.Item);
   end Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference (List : Element_List;
                       Index : Positive)
                       return Element_Reference
   is
   begin
      return Reference (List.Element (Index));
   end Reference;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access
                        procedure (Item : Element_Type'Class))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E : constant Element_Type'Class :=
                  Element (I);
         begin
            Process (E);
         end;
      end loop;
   end Scan;

   ------------
   -- Search --
   ------------

   function Search
     (Match : not null access
                      function (Item : Element_Type'Class)
                      return Boolean)
      return Cursor
   is
   begin
      for I in 1 .. Db.Last_Index loop
         if Db.Has_Element (I) then
            declare
               E : constant Element_Type'Class :=
                     Element (I);
            begin
               if Match (E) then
                  return (True, I);
               end if;
            end;
         end if;
      end loop;
      return Null_Cursor;
   end Search;

   ------------
   -- Search --
   ------------

   function Search (Identifier : String) return Cursor is
   begin
      for I in 1 .. Db.Last_Index loop
         if Db.Has_Element (I) then
            declare
               E : constant Element_Type'Class :=
                     Element (I);
            begin
               if E in Identifier_Record_Type'Class
                 and then Identifier_Record_Type'Class (E).Identifier
                 = Identifier
               then
                  return (True, I);
               end if;
            end;
         end if;
      end loop;
      return Null_Cursor;
   end Search;

   ---------
   -- Set --
   ---------

--     procedure Set (Index : Database_Reference;
--                    Item  : Element_Access)
--     is
--     begin
--        Db.Set (Index, Item);
--        Memor.Root_Record_Type (Item.all).Reference := Index;
--     end Set;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      if Locking then
         Db.Unlock;
      end if;
   end Unlock;

   ------------
   -- Unlock --
   ------------

--     procedure Unlock (Item  : Element_Access) is
--        E      : Db_Entry_Access :=
--                   Db.Element (Root_Record_Type'Class (Item.all).Reference);
--     begin
--        if Locking then
--           E.Unlock;
--        end if;
--     end Unlock;

   ------------
   -- Update --
   ------------

   ------------
   -- Update --
   ------------

   procedure Update (Ref : Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Element_Type'Class))
   is
      E      : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if Locking then
         E.Lock;
      end if;
      declare
         Item : constant Element_Access := Element_Access (E.Item);
      begin
         Updater (Item.all);
         E.Unlock;
      end;
   end Update;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Item    : Local_Database_Type;
                     Ref     : Memor.Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Root_Record_Type'Class))
   is

      pragma Unreferenced (Item);

      procedure Perform_Update
        (Item : in out Element_Type'Class);

      --------------------
      -- Perform_Update --
      --------------------

      procedure Perform_Update
        (Item : in out Element_Type'Class)
      is
      begin
         Updater (Item);
      end Perform_Update;

   begin
      Update (Ref, Perform_Update'Access);
   end Update;

end Memor.Database;