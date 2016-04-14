private with Ada.Containers.Vectors;

generic
   type Element_Type is private;
   Default_Value : Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Memor.Element_Vectors is

   type Vector is tagged private;

   procedure Clear (Container : in out Vector);

   function Element
     (Container : Vector;
      Reference : Database_Reference)
      return Element_Type;

   procedure Replace_Element
     (Container : in out Vector;
      Reference : Database_Reference;
      Element   : Element_Type);

   procedure Update_Element
     (Container : in out Vector;
      Reference : Database_Reference;
      Update    : not null access
        procedure (Element : in out Element_Type));

   procedure Iterate
     (Container : Vector;
      Process   : not null access
        procedure (Reference : Memor.Database_Reference;
                   Element   : Element_Type));

private

   package Element_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Real_Database_Reference,
        Element_Type => Element_Type,
        "="          => "=");

   type Vector is new Element_Vectors.Vector with null record;

end Memor.Element_Vectors;
