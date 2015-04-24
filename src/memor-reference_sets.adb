package body Memor.Reference_Sets is

   procedure Toggle
     (Set : in out Reference_Set;
      Ref : Database_Reference);

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : Database_Reference;
      Right : Reference_Set)
      return Reference_Set
   is
      Result : Reference_Set := Right;
   begin
      Add (Result, Left);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : Reference_Set;
      Right : Database_Reference)
      return Reference_Set
   is
      Result : Reference_Set := Left;
   begin
      Add (Result, Right);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : Reference_Set;
      Right : Reference_Set)
      return Reference_Set
   is
      Result : Reference_Set := Left;
   begin
      Union (Result, Right);
      return Result;
   end "&";

   ---------
   -- Add --
   ---------

   procedure Add
     (Set  : in out Reference_Set;
      Item : Database_Reference)
   is
   begin
      if not Member (Set, Item) then
         Toggle (Set, Item);
         Set.Count := Set.Count + 1;
      end if;
   end Add;

   -----------
   -- Empty --
   -----------

   function Empty return Reference_Set is
   begin
      return Result : Reference_Set do
         Result.Count := 0;
      end return;
   end Empty;

   ------------------
   -- Intersection --
   ------------------

   procedure Intersection
     (Set_1 : in out Reference_Set;
      Set_2 : Reference_Set)
   is
   begin

      if Set_1.V.Last_Index > Set_2.V.Last_Index then
         Set_1.V.Set_Length (Set_2.V.Length);
      end if;

      for Index in 1 .. Set_2.V.Last_Index loop
         if Index <= Set_1.V.Last_Index then
            Set_1.V (Index) := Set_1.V (Index) and Set_2.V (Index);
         else
            exit;
         end if;
      end loop;
   end Intersection;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Set : Reference_Set) return Boolean is
   begin
      return Set.Count = 0;
   end Is_Empty;

   ------------
   -- Member --
   ------------

   function Member (Set : Reference_Set;
                    Item : Database_Reference)
                    return Boolean
   is
      Index : constant Positive := Positive (Item / Set_Block_Size + 1);
      Mask  : constant Set_Block :=
                Set_Block (2 ** Natural (Item mod Set_Block_Size));
   begin
      return Index <= Set.V.Last_Index
        and then (Set.V (Index) and Mask) = Mask;
   end Member;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Set : in out Reference_Set;
      Item : Database_Reference)
   is
   begin
      if Member (Set, Item) then
         Toggle (Set, Item);
         Set.Count := Set.Count - 1;
      end if;
   end Remove;

   ------------
   -- Toggle --
   ------------

   procedure Toggle
     (Set : in out Reference_Set;
      Ref : Database_Reference)
   is
      Index : constant Positive := Positive (Ref / Set_Block_Size + 1);
      Mask  : constant Set_Block :=
                Set_Block (2 ** Natural (Ref mod Set_Block_Size));
   begin
      while Index > Set.V.Last_Index loop
         Set.V.Append (0);
      end loop;

      if (Set.V (Index) and Mask) = Mask then
         Set.V (Index) := Set.V (Index) and not Mask;
      else
         Set.V (Index) := Set.V (Index) or Mask;
      end if;
   end Toggle;

   -----------
   -- Union --
   -----------

   procedure Union
     (Set_1 : in out Reference_Set;
      Set_2 : Reference_Set)
   is
   begin
      for Index in 1 .. Set_2.V.Last_Index loop
         if Index <= Set_1.V.Last_Index then
            Set_1.V (Index) := Set_1.V (Index) or Set_2.V (Index);
         else
            Set_1.V.Append (Set_2.V (Index));
         end if;
      end loop;
   end Union;

end Memor.Reference_Sets;
