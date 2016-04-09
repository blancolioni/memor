package body Memor.Element_Vectors is

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Container : in out Vector) is
   begin
      Element_Vectors.Vector (Container).Clear;
   end Clear;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Container : Vector;
      Reference : Database_Reference)
      return Element_Type
   is
   begin
      if Reference < Container.Last_Index then
         return Element_Vectors.Vector (Container).Element (Reference);
      else
         return Default_Value;
      end if;
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   overriding procedure Replace_Element
     (Container : in out Vector;
      Reference : Database_Reference;
      Element   : Element_Type)
   is
   begin
      while Container.Last_Index < Reference loop
         Container.Append (Default_Value);
      end loop;
      Element_Vectors.Vector (Container).Replace_Element (Reference, Element);
   end Replace_Element;

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Container : in out Vector;
      Reference : Database_Reference;
      Update    : not null access
        procedure (Element : in out Element_Type))
   is
      Value : Element_Type := Container.Element (Reference);
   begin
      Update (Value);
      Container.Replace_Element (Reference, Value);
   end Update_Element;

end Memor.Element_Vectors;
