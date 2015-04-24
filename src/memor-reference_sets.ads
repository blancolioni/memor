private with Ada.Containers.Vectors;

package Memor.Reference_Sets is

   type Reference_Set is private;

   function Is_Empty (Set : Reference_Set) return Boolean;
   function Empty return Reference_Set;

   function Member (Set : Reference_Set;
                    Item : Database_Reference)
                    return Boolean;

   procedure Add (Set  : in out Reference_Set;
                  Item : Database_Reference);

   procedure Remove (Set : in out Reference_Set;
                     Item : Database_Reference);

   procedure Union (Set_1 : in out Reference_Set;
                    Set_2 : Reference_Set);

   procedure Intersection (Set_1 : in out Reference_Set;
                           Set_2 : Reference_Set);

   function "&" (Left : Database_Reference;
                 Right : Reference_Set)
                 return Reference_Set;

   function "&" (Left : Reference_Set;
                 Right : Database_Reference)
                 return Reference_Set;

   function "&" (Left : Reference_Set;
                 Right : Reference_Set)
                 return Reference_Set;

private

   Set_Block_Size : constant := 64;

   type Set_Block is mod 2 ** 64
     with Size => 64;

   package Set_Vectors is
     new Ada.Containers.Vectors (Positive, Set_Block);

   type Reference_Set is
      record
         Count : Natural := 0;
         V     : Set_Vectors.Vector;
      end record;

end Memor.Reference_Sets;
