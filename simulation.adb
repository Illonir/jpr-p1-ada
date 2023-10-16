-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded; use Ada.Strings;

procedure Simulation is
   Number_Of_Components : constant Integer := 5;
   Number_Of_Vessels    : constant Integer := 3;
   Number_Of_Fleets     : constant Integer := 2;

   subtype Component_Type is Integer range 1 .. Number_Of_Components;
   subtype Vessel_Type is Integer range 1 .. Number_Of_Vessels;
   subtype Fleet_Type is Integer range 1 .. Number_Of_Fleets;

   package Max_20_String is new Ada.Strings.Bounded.Generic_Bounded_Length
     (20);
   use Max_20_String;

   Component_Names : constant array (Component_Type) of Bounded_String :=
     (To_Bounded_String ("Engine"), To_Bounded_String ("Power generator"),
      To_Bounded_String ("Hull"), To_Bounded_String ("Computer"),
      To_Bounded_String ("Armaments"));

   Vessel_Names : constant array (Vessel_Type) of Bounded_String :=
     (To_Bounded_String ("Yacht"), To_Bounded_String ("Cruiser"),
      To_Bounded_String ("Freighter"));

   package Random_Vessel is new Ada.Numerics.Discrete_Random (Vessel_Type);
   type My_Str is new String (1 .. 256);

   -- Producer produces determined product
   task type Component_Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start_Production
        (Component : in Component_Type; Production_Time : in Integer);
   end Component_Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Fleet is
      -- Give the Consumer an identity
      entry Start
        (New_Fleet_Number            : in Fleet_Type;
         New_Vessel_Integration_Time : in Integer);
   end Fleet;

   -- In the Buffer, products are assemblied into an assembly
   task type Shipyard is
      -- Accept a product to the storage provided there is a room for it
      entry Take_Component
        (Component         : in     Component_Type; Component_ID : in Integer;
         Production_Halted :    out Boolean);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver_Ready_Vessel
        (Vessel : in Vessel_Type; Vessel_ID : out Integer);
   end Shipyard;

   Comoponent_Producers : array
     (1 .. Number_Of_Components) of Component_Producer;
   Fleets        : array (1 .. Number_Of_Fleets) of Fleet;
   Main_Shipyard : Shipyard;

   task body Component_Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production_Time is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      Production_Time_Generator : Random_Production_Time
        .Generator;   --  generator liczb losowych
      Component_Type_Number : Integer;
      Current_Component_ID  : Integer;
      Production            : Integer;   --???
      Production_Halted     : Boolean := False;

   begin
      accept Start_Production
        (Component : in Component_Type; Production_Time : in Integer)
      do
         Random_Production_Time.Reset
           (Production_Time_Generator);    --  start random number generator
         Current_Component_ID  := 1;
         Component_Type_Number := Component;
         Production            := Production_Time;
      end Start_Production;

      Put_Line
        ("Started production of " &
         To_String (Component_Names (Component_Type_Number)));
      loop
         if Production_Halted then
            Production_Halted := False;
            Put_Line
              ("###Production of component " &
               To_String (Component_Names (Component_Type_Number)) &
               " number " & Integer'Image (Current_Component_ID) & "halted");
            delay Duration (6);-- one full production cycle
         else
            delay Duration
              (Random_Production_Time.Random
                 (Production_Time_Generator)); --  symuluj produkcj\u00c4\u0099
            Put_Line
              ("Produced component " &
               To_String (Component_Names (Component_Type_Number)) &
               " number " & Integer'Image (Current_Component_ID));
            Current_Component_ID := Current_Component_ID + 1;
         end if;

         Main_Shipyard.Take_Component
           (Component_Type_Number, Current_Component_ID, Production_Halted);
      end loop;
   end Component_Producer;

   task body Fleet is
      subtype Integration_Time_Range is Integer range 4 .. 8;
      package Random_Integration_Time is new Ada.Numerics.Discrete_Random
        (Integration_Time_Range);
      Integration_Time_Generator : Random_Integration_Time
        .Generator;  --  random number generator (time)
      Vessel_Type_Generator : Random_Vessel.Generator;    --  also (assemblies)
      Fleet_Number          : Fleet_Type;
      Requested_Vessel_ID   : Integer;
      Integration           : Integer;
      Requested_Vessel      : Integer;
      Fleet_Names : constant array (1 .. Number_Of_Fleets) of Bounded_String :=
        (To_Bounded_String ("Fleet1"), To_Bounded_String ("Fleet2"));
   begin
      accept Start
        (New_Fleet_Number            : in Fleet_Type;
         New_Vessel_Integration_Time : in Integer)
      do
         Random_Integration_Time.Reset
           (Integration_Time_Generator);   --  ustaw generator
         Random_Vessel.Reset (Vessel_Type_Generator);     --  teÅ¼
         Fleet_Number := New_Fleet_Number;
         Integration  := New_Vessel_Integration_Time;
      end Start;

      Put_Line
        ("Started assembly of fleet " &
         To_String (Fleet_Names (Fleet_Number)));
      loop
         Requested_Vessel := Random_Vessel.Random (Vessel_Type_Generator);

         select
            -- take an assembly for consumption
            Main_Shipyard.Deliver_Ready_Vessel
              (Requested_Vessel, Requested_Vessel_ID);
            Put_Line
              (To_String (Fleet_Names (Fleet_Number)) &
               ": integrated vessel " &
               To_String (Vessel_Names (Requested_Vessel)) & " number " &
               Integer'Image (Requested_Vessel_ID));
            delay Duration
              (Random_Integration_Time.Random
                 (Integration_Time_Generator)); --  simulate consumption
         else
            Put_Line ("$$$Fleet wouldnt wait");
            delay Duration (8);
         end select;

      end loop;
   end Fleet;

   task body Shipyard is
      Component_Capacity : constant Integer := 30;
      type Storage_type is array (Component_Type) of Integer;
      Component_Storage : Storage_type := (0, 0, 0, 0, 0);
      Vessel_Components : array (Vessel_Type, Component_Type) of Integer :=
        ((2, 1, 2, 1, 2), (2, 2, 0, 1, 0), (1, 1, 2, 0, 1));
      Max_Component_Content : array (Component_Type) of Integer;
      Assembled_Vessels     : array (Vessel_Type) of Integer := (1, 1, 1);
      Components_In_Storage : Integer                        := 0;

      procedure Setup_Variables is
      begin
         for C in Component_Type loop
            Max_Component_Content (C) := 0;
            for V in Vessel_Type loop
               if Vessel_Components (V, C) > Max_Component_Content (C) then
                  Max_Component_Content (C) := Vessel_Components (V, C);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept_Component (Component : Component_Type) return Boolean
      is
         Free_Storage_Room : Integer;         --  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking_Min_Components : array (Component_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_Storage_Room : Integer;
         MP                   : Boolean;                   --  can accept
      begin
         if Components_In_Storage >= Component_Capacity then
            return False;
         end if;
         -- There is free room in the storage
         Free_Storage_Room := Component_Capacity - Components_In_Storage;
         MP                := True;
         for C in Component_Type loop
            if Component_Storage (C) < Max_Component_Content (C) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;                --  storage has products for arbitrary
            --  assembly
         end if;
         if Integer'Max
             (0,
              Max_Component_Content (Component) -
              Component_Storage (Component)) >
           0
         then
            -- exactly this product lacks
            return True;
         end if;
         Lacking_Storage_Room :=
           1;                     --  insert current product
         for C in Component_Type loop
            Lacking_Min_Components (C) :=
              Integer'Max
                (0, Max_Component_Content (C) - Component_Storage (C));
            Lacking_Storage_Room :=
              Lacking_Storage_Room + Lacking_Min_Components (C);
         end loop;
         if Free_Storage_Room >= Lacking_Storage_Room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept_Component;

      function Can_Deliver_Vessel (Vessel : Vessel_Type) return Boolean is
      begin
         for C in Component_Type loop
            if Component_Storage (C) < Vessel_Components (Vessel, C) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver_Vessel;

      procedure Show_Storage is
      begin
         Put ("Storage [");
         for C in Component_Type loop
            if C = 1 then
               Put (Integer'Image (Component_Storage (C)));
            else
               Put (", " & Integer'Image (Component_Storage (C)));
            end if;
         end loop;
         Put_Line ("]");
      end Show_Storage;

   begin
      Put_Line ("Buffer started");
      Setup_Variables;
      loop
         accept Take_Component
           (Component         : in Component_Type; Component_ID : in Integer;
            Production_Halted :    out Boolean)
         do
            if Can_Accept_Component (Component) then
               Put_Line
                 ("Accepted component " &
                  To_String (Component_Names (Component)) & " number " &
                  Integer'Image (Component_ID));
               Component_Storage (Component) :=
                 Component_Storage (Component) + 1;
               Components_In_Storage := Components_In_Storage + 1;
            else
               Put_Line
                 ("Rejected component " &
                  To_String (Component_Names (Component)) & " number " &
                  Integer'Image (Component_ID));
               Production_Halted := True;
            end if;
         end Take_Component;

         Show_Storage;

         accept Deliver_Ready_Vessel
           (Vessel : in Vessel_Type; Vessel_ID : out Integer)
         do
            if Can_Deliver_Vessel (Vessel) then
               Put_Line
                 ("Delivered vessel " & To_String (Vessel_Names (Vessel)) &
                  " number " & Integer'Image (Assembled_Vessels (Vessel)));
               for C in Component_Type loop
                  Component_Storage (C) :=
                    Component_Storage (C) - Vessel_Components (Vessel, C);
                  Components_In_Storage :=
                    Components_In_Storage - Vessel_Components (Vessel, C);
               end loop;
               Vessel_ID                  := Assembled_Vessels (Vessel);
               Assembled_Vessels (Vessel) := Assembled_Vessels (Vessel) + 1;
            else
               Put_Line
                 ("Lacking products for assembly " &
                  To_String (Vessel_Names (Vessel)));
               Vessel_ID := 0;
            end if;
         end Deliver_Ready_Vessel;
         Show_Storage;
      end loop;
   end Shipyard;

begin
   for C in 1 .. Number_Of_Components loop
      Comoponent_Producers (C).Start_Production (C, 10);
   end loop;
   for F in 1 .. Number_Of_Fleets loop
      Fleets (F).Start (F, 12);
   end loop;
end Simulation;
