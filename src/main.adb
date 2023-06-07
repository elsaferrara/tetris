with Interfaces;        use Interfaces;
with Tetris_Functional; use Tetris_Functional;
with Pico.Pimoroni.Display;    use Pico.Pimoroni.Display;
with Pico.Pimoroni.Display.Buttons;    use Pico.Pimoroni.Display.Buttons;
with RP.Timer;
with RP.Device;
with RP.Clock;

procedure Main
with SPARK_Mode => On
is

         pragma Warnings (Off, """pico.pimoroni.display.cursor_x"" is set by ""Put"" but not used after the call");
     pragma Warnings (Off, """pico.pimoroni.display.cursor_y"" is set by ""Put"" but not used after the call");
      pragma Warnings (Off, """pico.pimoroni.display.cursor_x"" is set by ""Draw_Board"" but not used after the call");
     pragma Warnings (Off, """pico.pimoroni.display.cursor_y"" is set by ""Draw_Board"" but not used after the call");
  pragma Warnings (Off, """Rnd_64"" is set by ""Clock"" but not used after the call");

   Zoom : constant := 6;
   --  Number of pixel to represent one block in the game

   function Fall_Period (Speed_Up : Boolean; Level : Unsigned_32)
                         return Unsigned_32;
   --  Time to wait between each game step

   Level_Nbr    : Unsigned_32;
   Score        : Unsigned_32;

   Line_Counter : Unsigned_32;
   --  Used to compute the level

   Nbr_Of_Complete_Lines : Unsigned_32;

   procedure Draw_Board (With_Piece : Boolean);
   --  Draw on the OLED screen the board and the falling piece

   --  Simple random generator.
   Rnd : Unsigned_64;

   procedure Random_Piece (Nbr : in out Unsigned_64; P : out Piece);
   --  Generate a new random piece

   Next_Piece : Piece;
   --  Next piece to be inserted in the game

   type Game_State is (Pre_Game, New_Piece, Piece_Fall, Game_Over);

   State : Game_State := Game_State'First;
   Rotation_Count : Natural;
   Next_Fall : Unsigned_64;
   Now       : Unsigned_64;

   -----------------
   -- Fall_Period --
   -----------------

   function Fall_Period (Speed_Up : Boolean; Level : Unsigned_32)
                         return Unsigned_32
   is
      Interval : constant Unsigned_32 :=
        ((11 - Level mod 11) * 15000);
   begin
      return (if Speed_Up then Interval / 3 else Interval);
   end Fall_Period;

   ----------------
   -- Draw_Board --
   ----------------

   procedure Draw_Board (With_Piece : Boolean) is
      X_Start : constant := 5;
      Y_Start : constant := 5;

      procedure Draw_Block (X : Integer_16; Y : Integer_16);
      procedure Draw_Piece (P        : Piece;
                            Offset_X : Integer_16 := 0;
                            Offset_Y : Integer_16 := 0);

      ----------------
      -- Draw_Block --
      ----------------

      procedure Draw_Block (X : Integer_16; Y : Integer_16) is
         X_Pos : constant Integer_16 := Zoom * (X - X_Coord'First);
         Y_Pos : constant Integer_16 := Zoom * (Y - Y_Coord'First);
      begin

         for I in Integer_16 range 1 .. Zoom loop
            for J in Integer_16 range 1 .. Zoom loop
               -- Landscape layout
               Set_Pixel ((Integer (X_Pos + I - 1 + X_Start), Integer (Y_Pos + J - 1 + Y_Start)));

               --  Portrait layout
               --  Set_Pixel ((Integer (Y_Pos + J - 1 + Y_Start), Integer (X_Pos + I - 1 + X_Start)));

            end loop;
         end loop;
      end Draw_Block;

      ----------------
      -- Draw_Piece --
      ----------------

      procedure Draw_Piece (P        : Piece;
                            Offset_X : Integer_16 := 0;
                            Offset_Y : Integer_16 := 0)
      is
      begin
         case P.S is
            when O =>
               Pico.Pimoroni.Display.Set_Color (Red);
               Draw_Block (P.X + Offset_X, P.Y + Offset_Y);
               Draw_Block (P.X + Offset_X, P.Y + 1 + Offset_Y);
               Draw_Block (P.X + 1 + Offset_X, P.Y + Offset_Y);
               Draw_Block (P.X + 1 + Offset_X, P.Y + 1 + Offset_Y);

            when I =>
               Pico.Pimoroni.Display.Set_Color (Blue);
               for Y in I_Delta loop
                  for X in I_Delta loop
                     if Possible_I_Shapes (P.D) (Y, X) then
                        Draw_Block (P.X + X + Offset_X, P.Y + Y + Offset_Y);
                     end if;
                  end loop;
               end loop;

            when Three_Shape =>
               case P.S is
                  when J => Pico.Pimoroni.Display.Set_Color (Orange);
                  when L => Pico.Pimoroni.Display.Set_Color (Cyan);
                  when S => Pico.Pimoroni.Display.Set_Color (Magenta);
                  when T => Pico.Pimoroni.Display.Set_Color (Green);
                  when Z => Pico.Pimoroni.Display.Set_Color (Yellow);
                     when others => null;
                 end case;
               for Y in Three_Delta loop
                  for X in Three_Delta loop
                     if Possible_Three_Shapes
                       (P.S, P.D) (Y, X)
                     then
                        Draw_Block (P.X + X + Offset_X, P.Y + Y + Offset_Y);
                     end if;
                  end loop;
               end loop;
         end case;
      end Draw_Piece;
   begin


      Pico.Pimoroni.Display.Set_Color (White);

      Set_Text_Cursor (120, 75);
      Put ("SPARK ");
      Set_Text_Cursor (116, 90);
      Put ("TETRIS");
      Set_Text_Cursor (118, 105);
      Put ("Demo");

      Set_Text_Cursor (180, 0);
      Put ("Score:");
      Set_Text_Cursor (180, 8);

      Put (Integer_32 (Score));
      Set_Text_Cursor (180, 16);
      Put ("Level:");
      Set_Text_Cursor (180, 24);
      Put (Integer_32 (Level_Nbr + 1));

      -- Landscape layout --

      --  Vertical boarders
      Draw_Vertical_Line (X_Start - 1, Y_Start, Y_Size * Zoom);
      Draw_Vertical_Line (X_Start + X_Size * Zoom, Y_Start, Y_Size * Zoom);

      --  Bottom boarder
      Draw_Horizontal_Line (X_Start, Y_Start + Y_Size * Zoom, X_Size * Zoom);

      -- Portrait layout --

      --  Draw_Horizontal_Line (Y_Start, X_Start - 1, Y_Size * Zoom);
      --  Draw_Horizontal_Line (Y_Start, X_Start + X_Size * Zoom, Y_Size * Zoom);
      --  Draw_Vertical_Line (Y_Start + Y_Size * Zoom, X_Start, X_Size * Zoom);


      --  The board
      for Y in Y_Coord loop
         for X in X_Coord loop
            if Cur_Board (Y)(X) /= Empty then
               case Cur_Board (Y)(X) is
               when I => Pico.Pimoroni.Display.Set_Color (Blue);
                  when O => Pico.Pimoroni.Display.Set_Color (Red);
                  when J => Pico.Pimoroni.Display.Set_Color (Orange);
                  when L => Pico.Pimoroni.Display.Set_Color (Cyan);
                  when S => Pico.Pimoroni.Display.Set_Color (Magenta);
                  when T => Pico.Pimoroni.Display.Set_Color (Green);
                  when Z => Pico.Pimoroni.Display.Set_Color (Yellow);
                     when others => null;
               end case;
               Draw_Block (X, Y);
            end if;
         end loop;
      end loop;

      --  The current piece
      if With_Piece then
         Draw_Piece (Cur_Piece);
      end if;

      --  Preview of the next piece displayed to the right of the board
      Draw_Piece (Next_Piece, Offset_X => 7, Offset_Y => 1);
   end Draw_Board;

   ------------------
   -- Random_Piece --
   ------------------

   procedure Random_Piece (Nbr : in out Unsigned_64; P : out Piece) is
   begin
      Nbr := Nbr * 1103515245 + 12345;
      P := (S => Cell'Val (1 + ((Nbr / 65536) mod 7)),
            D => North,
            X => X_Size / 2,
            Y => Y_Coord'First);
   end Random_Piece;

   ----------------
   -- Reset_Game --
   ----------------

   procedure Reset_Game is
   begin
      Cur_Board := (others => (others => Empty));
      Score := 0;
      Level_Nbr := 0;
      Line_Counter := 0;
      Rotation_Count := 0;
      Next_Fall := 0;
      RP.Timer.Clock (RP.Timer.Time (Rnd));
   end Reset_Game;

   procedure Block_Piece
     with Pre => Cur_State = Piece_Falling and Valid_Configuration,
       Post => Cur_State = Piece_Blocked and Valid_Configuration;
   procedure Block_Piece is
      begin
      Cur_State := Piece_Blocked;
   end Block_Piece;


   Success : Boolean;
   Unused : Boolean;

   Release_Time : RP.Timer.Time;
   Period : constant := 100;

begin

   --  Setup_Game;

   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;
   Pico.Pimoroni.Display.Initialize;
   Reset_Game;
   Random_Piece (Rnd, Next_Piece);

   RP.Timer.Clock (Release_Time);
   --  Game loop
   loop

      pragma Loop_Invariant (if State = Piece_Fall then Cur_State = Piece_Falling and Valid_Configuration);

      --  RP.Device.Timer.Delay_Milliseconds(60);
      Pico.Pimoroni.Display.Update (Clear => True);
      Release_Time := RP.Timer.Get_Deadline (Release_Time, Period);
      RP.Timer.Busy_Wait_Until (Release_Time);
      Pico.Pimoroni.Display.Buttons.Poll_Buttons;
      RP.Timer.Clock (RP.Timer.Time (Now));


      case State is
         when Pre_Game =>
            Draw_Board (False);
            Pico.Pimoroni.Display.Set_Color (White);
            Set_Text_Cursor (20, 6);
            Put ("PRESS");
            Set_Text_Cursor (33, 21);
            Put ("A");
            Set_Text_Cursor (30, 36);
            Put ("TO");
            Set_Text_Cursor (20, 51);
            Put ("START");

            if Just_Pressed (A) then
               State := Game_State'Succ (State);
               Reset_Game;
            end if;

         when New_Piece =>
            --  Add a new piece
            Cur_Piece  := Next_Piece;
            Random_Piece (Rnd, Next_Piece);
            Cur_State := Piece_Falling;
            Rotation_Count := 0;
            Next_Fall := Now;

            --  Stop the game when the piece cannot appear
            if not Valid_Configuration then
               State := Game_Over;
            else
               State := Piece_Fall;


            end if;

            Draw_Board (True);

         when Piece_Fall =>
           if Rotation_Count < 2 then
               if Just_Pressed (A) then
                  Do_Action (Turn_Counter_Clockwise, Success);

                  if Success then
                     Rotation_Count := Rotation_Count + 1;
                  end if;
               elsif Just_Pressed (X) then
                  Do_Action (Turn_Clockwise, Success);

                  if Success then
                     Rotation_Count := Rotation_Count + 1;
                  end if;
               end if;
            end if;

            if Just_Pressed (B) then
               Do_Action (Move_Left, Unused);
            elsif Just_Pressed (Y) then
               Do_Action (Move_Right, Unused);
            end if;

            if Now >= Next_Fall then

               Next_Fall := Next_Fall + Unsigned_64 (Fall_Period
                 (Speed_Up => False,
                  Level    => Level_Nbr));

               Rotation_Count := 0;

               --  Fall and continue unless the piece hits the ground
               Do_Action (Move_Down, Success);

               if not Success then

                  --  Done with that piece
                  Block_Piece;
                  Include_Piece_In_Board;
                  Delete_Complete_Lines (Nbr_Of_Complete_Lines);

                  case Nbr_Of_Complete_Lines is
                     when 1 => Score := Score + 4 * (Level_Nbr + 1);
                     when 2 => Score := Score + 10 * (Level_Nbr + 1);
                     when 3 => Score := Score + 30 * (Level_Nbr + 1);
                     when 4 => Score := Score + 120 * (Level_Nbr + 1);
                     when others => null;
                  end case;

                  Line_Counter := Line_Counter + Nbr_Of_Complete_Lines;

                  if Line_Counter >= 10 and then Level_Nbr < 10 then
                     Level_Nbr    := Level_Nbr + 1;
                     Line_Counter := Line_Counter - 10;
                  end if;

                  State := New_Piece;

               end if;
            end if;

            Draw_Board (True);

         when Game_Over =>
            --  Lost!
            Draw_Board (False);
            Set_Color(White);
            Set_Text_Cursor (20, 20);
            Put ("GAME");
            Set_Text_Cursor (20, 35);
            Put ("OVER");

            if Just_Pressed (B) then
               State := Pre_Game;
            end if;

      end case;
   end loop;
end Main;
