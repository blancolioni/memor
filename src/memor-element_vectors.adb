package body Memor.Element_Vectors is

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Container.V.Clear;
   end Clear;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type'Class)
      return Element_Type
   is
      Reference : constant Database_Reference := Index.Reference;
   begin
      if Reference <= Container.V.Last_Index then
         return Container.V.Element (Reference);
      else
         return Default_Value;
      end if;
   end Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Vector;
      Process   : not null access
        procedure (Index     : Index_Type'Class;
                   Element   : Element_Type))
   is
   begin
      if Container.Db /= null then
         for Reference in 1 .. Container.V.Last_Index loop
            Process (Index_Type'Class (Container.Db.Element (Reference).all),
                     Container.V.Element (Reference));
         end loop;
      end if;
   end Iterate;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type'Class;
      Element   : Element_Type)
   is
   begin
      pragma Assert (Container.Db = null
                     or else Container.Db = Index.Object_Database);

      while Container.V.Last_Index < Index.Reference loop
         Container.V.Append (Default_Value);
      end loop;
      Container.V.Replace_Element (Index.Reference, Element);
      Container.Db := Index.Object_Database;
   end Replace_Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : not null access constant Index_Type'Class;
      Element   : Element_Type)
   is
   begin
      Container.Replace_Element (Index.all, Element);
   end Replace_Element;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Vector;
      Index     : Index_Type'Class;
      Update    : not null access
        procedure (Element : in out Element_Type))
   is
   begin
      pragma Assert (Container.Db = null
                     or else Container.Db = Index.Object_Database);
      Container.Db := Index.Object_Database;

      while Container.V.Last_Index < Index.Reference loop
         Container.V.Append (Default_Value);
      end loop;
      declare
         Value : Element_Type renames
                   Container.V (Index.Reference);
      begin
         Update (Value);
      end;
   end Update_Element;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Vector;
      Index     : not null access Index_Type'Class;
      Update    : not null access
        procedure (Element : in out Element_Type))
   is
   begin
      Container.Update_Element (Index.all, Update);
   end Update_Element;

end Memor.Element_Vectors;
