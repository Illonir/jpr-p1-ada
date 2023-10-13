-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
   Number_Of_Components   : constant Integer := 5;
   Number_Of_Vessels : constant Integer := 3;
   Number_Of_Fleets  : constant Integer := 2;

   subtype Component_Type is Integer range 1 .. Number_Of_Components;
   subtype Vessel_Type is Integer range 1 .. Number_Of_Vessels;
   subtype Fleet_Type is Integer range 1 .. Number_Of_Fleets;

   Component_Names : constant array (Component_Type) of String (1 .. 32) :=
     ("Engine", "Power generator", "Hull", "Central computer", "Armaments");
   Vessel_Names : constant array (Vessel_Type) of String (1 .. 16) :=
     ("Luxury yacht", "Cruiser", "Freighter");

   package Random_Vessel is new Ada.Numerics.Discrete_Random (Vessel_Type);
   type My_Str is new String (1 .. 256);

   -- Producer produces determined product
   task type Component_Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start_Production (Component : in Component_Type; Production_Time : in Integer);
   end Component_Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Fleet is
      -- Give the Consumer an identity
      entry Start
        (New_Fleet_Number : in Fleet_Type; New_Vessel_Integration_Time : in Integer);
   end Fleet;

   -- In the Buffer, products are assemblied into an assembly
   task type Shipyard is
      -- Accept a product to the storage provided there is a room for it
      entry Take_Component
        (Component : in Component_Type; Component_ID : in Integer; Production_Halted : out Boolean);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver_Ready_Vessel (Vessel : in Vessel_Type; Vessel_ID : out Integer);
   end Shipyard;

   Comoponent_Producers : array (1 .. Number_Of_Components) of Component_Producer;
   Fleets : array (1 .. Number_Of_Fleets) of Fleet;
   Main_Shipyard : Shipyard;

   task body Component_Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production_Time is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      Production_Time_Generator : Random_Production_Time.Generator;   --  generator liczb losowych
      Component_Type_Number : Integer;
      Current_Component_ID      : Integer;
      Production          : Integer;   --???
      Production_Halted              : Boolean := False;

   begin
      accept Start_Production (Component : in Component_Type; Production_Time : in Integer) do
         Random_Production_Time.Reset (Production_Time_Generator);    --  start random number generator
         Current_Component_ID      := 1;
         Component_Type_Number := Component;
         Production          := Production_Time;
      end Start_Production;

      Put_Line ("Started production of " & Component_Names (Component_Type_Number));
      loop
         if Production_Halted then
            Production_Halted := False;
            Put_Line
              ("###Production of component " &
               Component_Names (Component_Type_Number) & " number " &
               Integer'Image (Current_Component_ID) & "halted");
            delay Duration (6);-- one full production cycle
         else
            delay Duration
              (Random_Production_Time.Random (Production_Time_Generator)); --  symuluj produkcj\u00c4\u0099
            Put_Line
              ("Produced component " & Component_Names (Component_Type_Number) &
               " number " & Integer'Image (Current_Component_ID));
            Current_Component_ID := Current_Component_ID + 1;
         end if;

         Main_Shipyard.Take_Component (Component_Type_Number, Current_Component_ID, Production_Halted);
      end loop;
   end Component_Producer;

   task body Fleet is
      subtype Integration_Time_Range is Integer range 4 .. 8;
      package Random_Integration_Time is new Ada.Numerics.Discrete_Random
        (Integration_Time_Range);
      Integration_Time_Generator : Random_Integration_Time.Generator;  --  random number generator (time)
      Vessel_Type_Generator : Random_Vessel.Generator;    --  also (assemblies)
      Fleet_Number     : Fleet_Type;
      Assembly_Number : Integer;
      Integration     : Integer;
      Assembly_Type   : Integer;
      Fleet_Names : constant array
        (1 .. Number_Of_Fleets) of String (1 .. 9) :=
        ("Fleet1", "Fleet2");
   begin
      accept Start
        (New_Fleet_Number : in Fleet_Type; New_Vessel_Integration_Time : in Integer)
      do
         Random_Integration_Time.Reset (Integration_Time_Generator);   --  ustaw generator
         Random_Vessel.Reset (Vessel_Type_Generator);     --  teÅ¼
         Fleet_Number := New_Fleet_Number;
         Integration := New_Vessel_Integration_Time;
      end Start;

      Put_Line ("Started assembly of fleet " & Fleet_Names (Fleet_Number));
      loop
         Assembly_Type := Random_Vessel.Random (Vessel_Type_Generator);

         select
            -- take an assembly for consumption
            Main_Shipyard.Deliver_Ready_Vessel (Vessel, Vessel_ID);
            Put_Line
              (Fleet_Names (Fleet_Number) & ": integrated vessel " &
               Vessel_Names (Vessel) & " number " &
               Integer'Image (Vessel_ID));
            delay Duration
              (Random_Integration_Time.Random (Integration_Time_Generator)); --  simulate consumption
         else
            Put_Line ("$$$Fleet wouldnt wait");
            delay Duration (8);
         end select;

      end loop;
   end Fleet;

   task body Shipyard is
      Storage_Capacity : constant Integer := 15;
      type Storage_type is array (Component_Type) of Integer;
      Storage          : Storage_type := (0, 0, 0, 0, 0);
      Assembly_Content : array (Vessel_Type, Component_Type) of Integer :=
        ((2, 1, 2, 1, 2), (2, 2, 0, 1, 0), (1, 1, 2, 0, 1));
      Max_Assembly_Content : array (Component_Type) of Integer;
      Assembly_Number      : array (Vessel_Type) of Integer := (1, 1, 1);
      In_Storage           : Integer                          := 0;

      procedure Setup_Variables is
      begin
         for W in Component_Type loop
            Max_Assembly_Content (W) := 0;
            for Z in Vessel_Type loop
               if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
                  Max_Assembly_Content (W) := Assembly_Content (Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept (Product : Component_Type) return Boolean is
         Free : Integer;         --  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking : array (Component_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room : Integer;
         MP           : Boolean;                   --  can accept
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         -- There is free room in the storage
         Free := Storage_Capacity - In_Storage;
         MP   := True;
         for W in Component_Type loop
            if Storage (W) < Max_Assembly_Content (W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;                --  storage has products for arbitrary
            --  assembly
         end if;
         if Integer'Max
             (0, Max_Assembly_Content (Product) - Storage (Product)) >
           0
         then
            -- exactly this product lacks
            return True;
         end if;
         Lacking_room := 1;                     --  insert current product
         for W in Component_Type loop
            Lacking (W) :=
              Integer'Max (0, Max_Assembly_Content (W) - Storage (W));
            Lacking_room := Lacking_room + Lacking (W);
         end loop;
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver (Assembly : Vessel_Type) return Boolean is
      begin
         for W in Component_Type loop
            if Storage (W) < Assembly_Content (Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         Put ("Storage [");
         for W in Component_Type loop
            if W = 1 then
               Put (Integer'Image (Storage (W)));
            else
               Put (", " & Integer'Image (Storage (W)));
            end if;
         end loop;
         Put_Line ("]");
      end Storage_Contents;

   begin
      Put_Line ("Buffer started");
      Setup_Variables;
      loop
         accept Take_Component
           (Product : in     Component_Type; Number : in Integer;
            Halted  :    out Boolean)
         do
            if Can_Accept (Product) then
               Put_Line
                 ("Accepted product " & Product_Name (Product) & " number " &
                  Integer'Image (Number));
               Storage (Product) := Storage (Product) + 1;
               In_Storage        := In_Storage + 1;
            else
               Put_Line
                 ("Rejected product " & Product_Name (Product) & " number " &
                  Integer'Image (Number));
               Halted := True;
            end if;
         end Take_Component;

         Storage_Contents;

         accept Deliver_Ready_Vessel (Assembly : in Vessel_Type; Number : out Integer) do
            if Can_Deliver (Assembly) then
               Put_Line
                 ("Delivered assembly " & Assembly_Name (Assembly) &
                  " number " & Integer'Image (Assembly_Number (Assembly)));
               for W in Component_Type loop
                  Storage (W) := Storage (W) - Assembly_Content (Assembly, W);
                  In_Storage  := In_Storage - Assembly_Content (Assembly, W);
               end loop;
               Number                     := Assembly_Number (Assembly);
               Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
            else
               Put_Line
                 ("Lacking products for assembly " & Assembly_Name (Assembly));
               Number := 0;
            end if;
         end Deliver_Ready_Vessel;
         Storage_Contents;
      end loop;
   end Shipyard;

begin
   for I in 1 .. Number_Of_Components loop
      Comoponent_Producers (I).Start_Production (I, 10);
   end loop;
   for J in 1 .. Number_Of_Fleets loop
      Fleets (J).Start (J, 12);
   end loop;
end Simulation;
