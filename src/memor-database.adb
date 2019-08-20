--  with Ada.Numerics.Discrete_Random;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

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

   overriding procedure Update
     (Item    : Local_Database_Type;
      Ref     : Memor.Database_Reference;
      Updater : not null access
        procedure (Item : not null access Root_Record_Type'Class));

   overriding function Element
     (Db        : Local_Database_Type;
      Reference : Database_Reference)
      return access constant Root_Record_Type'Class;

   Db_Object : aliased Local_Database_Type;

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
      new Ada.Containers.Vectors (Real_Database_Reference, Db_Entry_Access);

   package Db_Free_List is
     new Ada.Containers.Doubly_Linked_Lists (Db_Entry_Access);

   protected Db is
      entry Lock;
      procedure Unlock;

      procedure Create
        (Element : out Element_Access);

      procedure Create_With_Reference
        (Ref : Database_Reference;
         Element : out Element_Access);

      procedure Delete (Reference : Database_Reference);

      function Element (Ref : Database_Reference) return Db_Entry_Access;
      function Has_Element (Ref : Database_Reference) return Boolean;
      function Last_Index return Database_Reference'Base;
      function Deleted_Count return Natural;

   private
      V              : Db_Vectors.Vector;
      Free           : Db_Free_List.List;
      Db_Lock        : Boolean := False;
      Rec_Lock_Count : Natural := 0;
   end Db;

   --------
   -- Db --
   --------

   protected body Db is

      procedure Create
        (Element : out Element_Access)
      is
         New_Entry   : Db_Entry_Access;
      begin
         if Free.Is_Empty then
            Element := new Element_Type;
            New_Entry   := new Db_Entry (Element);
            V.Append (New_Entry);
            Memor.Root_Record_Type (Element.all).Ref := V.Last_Index;
         else
            New_Entry := Free.First_Element;
            Element   := Element_Access (New_Entry.Item);
            V (Element.Ref) := New_Entry;
            Free.Delete_First;
         end if;
      end Create;

      ---------------------------
      -- Create_With_Reference --
      ---------------------------

      procedure Create_With_Reference
        (Ref     : Database_Reference;
         Element : out Element_Access)
      is
         Free_Position : Db_Free_List.Cursor := Db_Free_List.No_Element;
      begin
         for Position in Free.Iterate loop
            if Db_Free_List.Element (Position).Item.Reference = Ref then
               Free_Position := Position;
               exit;
            end if;
         end loop;

         if Db_Free_List.Has_Element (Free_Position) then
            V (Ref) := Db_Free_List.Element (Free_Position);
            Element := Element_Access (V (Ref).Item);
         else
            Element := new Element_Type;
            V (Ref) := new Db_Entry (Element);
            Element.Ref := Ref;
         end if;
      end Create_With_Reference;

      ------------
      -- Delete --
      ------------

      procedure Delete (Reference : Database_Reference) is
         Current : constant Db_Entry_Access := V (Reference);
      begin
         Free.Append (Current);
         V.Replace_Element (Reference, null);
      end Delete;

      -------------------
      -- Deleted_Count --
      -------------------

      function Deleted_Count return Natural is
      begin
         return Natural (Free.Length);
      end Deleted_Count;

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

   ------------------
   -- Active_Count --
   ------------------

   function Active_Count return Natural is
      Result : Natural := 0;
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E : constant access constant Element_Type'Class :=
                  Element (I);
         begin
            if E /= null then
               Result := Result + 1;
            end if;
         end;
      end loop;
      return Result;
   end Active_Count;

   ---------
   -- Add --
   ---------

   procedure Add (List : in out Element_List;
                  Item : Element_Reference)
   is
   begin
      List.Append (Item.Reference);
   end Add;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Ref : in out Updateable_Reference) is
   begin
      Ref.Count.all := Ref.Count.all + 1;
   end Adjust;

   -----------
   -- Count --
   -----------

   function Count (List : Element_List) return Natural is
   begin
      return List.Last_Index;
   end Count;

   --------------------
   -- Count_Matching --
   --------------------

   function Count_Matching
     (Match : not null access
        function (Item : Element_Type'Class)
      return Boolean)
      return Natural
   is
      Result : Natural := 0;
   begin
      for I in 1 .. Last_Index loop
         if Match (Reference (I).all) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Matching;

   ------------
   -- Create --
   ------------

   function Create (Creator : not null access procedure
                      (Item : in out Element_Type'Class))
                    return Database_Reference
   is
      New_Element : Element_Access;
   begin
      Db.Create (New_Element);
      Creator (New_Element.all);
      return New_Element.Reference;
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
      New_Element : Element_Access;
   begin
      Db.Create_With_Reference (Ref, New_Element);
      Creator (New_Element.all);
   end Create;

   ------------
   -- Create --
   ------------

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

   procedure Delete
     (Reference : Database_Reference)
   is
   begin
      Db.Delete (Reference);
   end Delete;

   -------------------
   -- Deleted_Count --
   -------------------

   function Deleted_Count return Natural is
   begin
      return Db.Deleted_Count;
   end Deleted_Count;

   -------------
   -- Element --
   -------------

   function Element
     (Ref : Database_Reference)
      return access constant Element_Type'Class
   is
      It : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if It = null then
         return null;
      else
         if Locking then
            It.Begin_Fetch;
            return Result : constant access constant Element_Type'Class :=
              It.Item
            do
               It.End_Fetch;
            end return;
         else
            return It.Item;
         end if;
      end if;
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Db        : Local_Database_Type;
      Reference : Database_Reference)
      return access constant Root_Record_Type'Class
   is
      pragma Unreferenced (Db);
   begin
      return Element (Reference);
   end Element;

   ------------
   -- Exists --
   ------------

   function Exists (Identifier : String) return Boolean is
      Result : constant Cursor := Search (Identifier);
   begin
      return Has_Element (Result);
   end Exists;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Ref : in out Updateable_Reference) is
   begin
      if Ref.Count /= null then
         Ref.Count.all := Ref.Count.all - 1;
         if Ref.Count.all = 0 then
            declare
               procedure Free is
                 new Ada.Unchecked_Deallocation (Natural, Count_Access);
            begin
               Free (Ref.Count);
               Ref.Count := null;
               if Locking then
                  Db.Element (Ref.Element.Ref).Unlock;
               end if;
            end;
         end if;
      end if;
   end Finalize;

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
         if Locking then
            E.Unlock;
         end if;
         Item.After_Change;
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

   function Get_Database return Memor_Database is
   begin
      return Db_Object'Access;
   end Get_Database;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Item : Cursor) return Boolean is
   begin
      return Item.Has_Element;
   end Has_Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Ref : in out Updateable_Reference) is
   begin
      Ref.Count := new Natural'(1);
   end Initialize;

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
      procedure Update (Item : not null access Element_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Item : not null access Element_Type'Class) is
      begin
         Process (Item.all);
      end Update;

   begin
      Iterate (Update'Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Item : not null access Element_Type'Class))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E      : constant Db_Entry_Access := Db.Element (I);
         begin
            if E /= null then
               if Locking then
                  E.Lock;
               end if;
               declare
                  Item : constant Element_Access := Element_Access (E.Item);
               begin
                  Process (Item);
                  if Locking then
                     E.Unlock;
                  end if;
                  Item.After_Change;
               end;
            end if;
         end;
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Process : not null access
                        procedure (Item : in out Root_Record_Type'Class))
   is
      procedure Update (Item : in out Element_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Item : in out Element_Type'Class) is
      begin
         Process (Item);
      end Update;

   begin
      Iterate (Update'Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (Process : not null access
                        procedure (Item : not null access
                                     Root_Record_Type'Class))
   is
      procedure Update (Item : not null access Element_Type'Class);

      ------------
      -- Update --
      ------------

      procedure Update (Item : not null access Element_Type'Class) is
      begin
         Process (Item);
      end Update;

   begin
      Iterate (Update'Access);
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
            if E /= null then
               if Locking then
                  E.Lock;
               end if;
               declare
                  Item : constant Element_Access := Element_Access (E.Item);
               begin
                  if Match (Item.all) then
                     Process (Item.all);
                  end if;
                  if Locking then
                     E.Unlock;
                  end if;
                  Item.After_Change;
               end;
            end if;
         end;
      end loop;
   end Iterate;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index return Database_Reference is
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
      if It = null then
         return null;
      else
         return Element_Reference (It.Item);
      end if;
   end Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Item : Element_Type'Class)
      return Element_Reference
   is
      Ref : constant Database_Reference := Memor.Reference (Item);
   begin
      return Reference (Ref);
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
            E : constant access constant Element_Type'Class :=
                  Element (I);
         begin
            if E /= null then
               Process (E.all);
            end if;
         end;
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access
        procedure (Item : Element_Reference))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            Ref : constant Element_Reference := Reference (I);
         begin
            if Ref /= null then
               Process (Ref);
            end if;
         end;
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Match   : not null access
        function (Item : Element_Type'Class)
      return Boolean;
      Process : not null access
        procedure (Item : Element_Type'Class))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E : constant access constant Element_Type'Class :=
                  Element (I);
         begin
            if Match (E.all) then
               Process (E.all);
            end if;
         end;
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Match   : not null access
        function (Item : Element_Reference)
      return Boolean;
      Process : not null access
        procedure (Item : Element_Reference))
   is
   begin
      for I in 1 .. Db.Last_Index loop
         declare
            E : constant access constant Element_Type'Class :=
                  Element (I);
         begin
            if Match (Element_Reference (E)) then
               Process (Element_Reference (E));
            end if;
         end;
      end loop;
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access
        procedure (Item : Root_Record_Type'Class))
   is
      procedure Internal (Item : Element_Type'Class);

      --------------
      -- Internal --
      --------------

      procedure Internal (Item : Element_Type'Class) is
      begin
         Process (Item);
      end Internal;

   begin
      Scan (Internal'Access);
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
               E : constant access constant Element_Type'Class :=
                     Element (I);
            begin
               if Match (E.all) then
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
               E : constant access constant Element_Type'Class :=
                     Element (I);
            begin
               if E.all in Identifier_Record_Type'Class then
                  declare
                     Id : constant String :=
                            Identifier_Record_Type'Class (E.all).Identifier;
                  begin
                     if Id = Identifier then
                        return (True, I);
                     end if;
                  end;
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

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Update  : in out Root_Element_Update'Class;
      Element : not null access constant Element_Type'Class)
   is
   begin
      Update.Set_Target (Element.all);
   end Set_Target;

   ----------------
   -- Set_Target --
   ----------------

   procedure Set_Target
     (Update  : in out Root_Element_Update'Class;
      Element : Element_Type'Class)
   is
   begin
      Update.Reference := Element.Reference;
      Update.Database  := Element.Object_Database;
   end Set_Target;

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

   procedure Update (Ref : Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Element_Type'Class))
   is
      procedure Do_Update (Item : not null access Element_Type'Class);

      ---------------
      -- Do_Update --
      ---------------

      procedure Do_Update (Item : not null access Element_Type'Class) is
      begin
         Updater (Item.all);
      end Do_Update;

   begin
      Update (Ref, Do_Update'Access);
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update (Ref     : Database_Reference;
                     Updater : not null access
                       procedure (Item : not null access Element_Type'Class))
   is
      E      : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if Locking then
         E.Lock;
      end if;
      declare
         Item : constant Element_Access := Element_Access (E.Item);
      begin
         Updater (Item);
         if Locking then
            E.Unlock;
         end if;
         E.Item.After_Change;
      end;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update (Ref     : Database_Reference;
                     Updater : not null access
                       procedure (Item : in out Root_Record_Type'Class))
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
         if Locking then
            E.Unlock;
         end if;
         E.Item.After_Change;
      end;
   end Update;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Item    : Local_Database_Type;
                     Ref     : Memor.Database_Reference;
                     Updater : not null access
                       procedure (Item : not null access
                                    Root_Record_Type'Class))
   is
      pragma Unreferenced (Item);
      E      : constant Db_Entry_Access := Db.Element (Ref);
   begin
      if Locking then
         E.Lock;
      end if;

      declare
         Item : constant Element_Access := Element_Access (E.Item);
      begin
         Updater (Item);
         if Locking then
            E.Unlock;
         end if;
         E.Item.After_Change;
      end;

   end Update;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Element_Type'Class)
      return Updateable_Reference
   is
      Element : constant Db_Entry_Access := Db.Element (Item.Ref);
   begin
      if Locking then
         Element.Lock;
      end if;

      return Result : Updateable_Reference (Element.Item) do
         null;
      end return;
   end Update;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Item : Root_Element_Update) is
      procedure Do_Update
        (Element : not null access Root_Record_Type'Class);

      ---------------
      -- Do_Update --
      ---------------

      procedure Do_Update
        (Element : not null access Root_Record_Type'Class)
      is
      begin
         Root_Element_Update'Class (Item).Update_Element
           (Element_Type (Element.all)'Access);
      end Do_Update;

   begin
      Item.Database.Update
        (Item.Reference, Do_Update'Access);
   end Update;

   -----------------
   -- Upper_Bound --
   -----------------

   function Upper_Bound return Natural is
   begin
      return Natural (Last_Index);
   end Upper_Bound;

end Memor.Database;
