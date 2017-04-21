private with Ada.Containers.Vectors;

generic
   type Index_Type is new Root_Record_Type with private;
   type Element_Type is private;
   Default_Value : Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Memor.Element_Vectors is

   type Vector is tagged private;

   procedure Clear (Container : in out Vector);

   function Element
     (Container : Vector;
      Index     : Index_Type'Class)
      return Element_Type;

   function Element
     (Container : Vector;
      Index     : not null access constant Index_Type'Class)
      return Element_Type
   is (Container.Element (Index.all));

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type'Class;
      Element   : Element_Type);

   procedure Replace_Element
     (Container : in out Vector;
      Index     : not null access constant Index_Type'Class;
      Element   : Element_Type);

   procedure Update_Element
     (Container : in out Vector;
      Index     : Index_Type'Class;
      Update    : not null access
        procedure (Element : in out Element_Type));

   procedure Update_Element
     (Container : in out Vector;
      Index     : not null access constant Index_Type'Class;
      Update    : not null access
        procedure (Element : in out Element_Type));

   procedure Scan
     (Container    : Vector;
      Process      : not null access
        procedure (Index     : Index_Type'Class;
                   Element   : Element_Type));

   procedure Scan
     (Container    : Vector;
      Skip_Default : Boolean;
      Process      : not null access
        procedure (Index     : Index_Type'Class;
                   Element   : Element_Type));

   procedure Update
     (Container    : in out Vector;
      Process      : not null access
        procedure (Index     : Index_Type'Class;
                   Element   : in out Element_Type));

   procedure Update
     (Container    : in out Vector;
      Skip_Default : Boolean;
      Process      : not null access
        procedure (Index     : Index_Type'Class;
                   Element   : in out Element_Type));

private

   package Element_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Real_Database_Reference,
        Element_Type => Element_Type,
        "="          => "=");

   type Vector is tagged
      record
         V  : Element_Vectors.Vector;
         Db : Memor_Database;
      end record;

end Memor.Element_Vectors;
