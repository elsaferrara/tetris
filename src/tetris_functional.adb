pragma Style_Checks ("M132");

package body Tetris_Functional with
  SPARK_Mode
is
   ----------------------------
   -- Include_Piece_In_Board --
   ----------------------------

   procedure Include_Piece_In_Board is
   begin
      case Cur_Piece.S is
         when O =>
            Cur_Board (Cur_Piece.Y) (Cur_Piece.X)         := Cur_Piece.S;
            Cur_Board (Cur_Piece.Y + 1) (Cur_Piece.X)     := Cur_Piece.S;
            Cur_Board (Cur_Piece.Y) (Cur_Piece.X + 1)     := Cur_Piece.S;
            Cur_Board (Cur_Piece.Y + 1) (Cur_Piece.X + 1) := Cur_Piece.S;

         when I =>
            --  intermediate assertion needed for proof
            pragma Assert
              (for all YY in I_Delta =>
                 (for all XX in I_Delta =>
                    (if Possible_I_Shapes (Cur_Piece.D) (YY, XX) then
                       Is_Empty (Cur_Board, Cur_Piece.Y + YY, Cur_Piece.X + XX))));

            for Y in I_Delta loop
               for X in I_Delta loop
                  if Possible_I_Shapes (Cur_Piece.D) (Y, X) then
                     Cur_Board (Cur_Piece.Y + Y) (Cur_Piece.X + X) := Cur_Piece.S;
                  end if;
               end loop;
            end loop;

         when Three_Shape =>
            --  intermediate assertion needed for proof
            pragma Assert
              (for all YY in Three_Delta =>
                 (for all XX in Three_Delta =>
                    (if Possible_Three_Shapes (Cur_Piece.S, Cur_Piece.D) (YY, XX)
                     then Is_Empty (Cur_Board,
                                    Cur_Piece.Y + YY,
                                    Cur_Piece.X + XX))));

            for Y in Three_Delta loop
               for X in Three_Delta loop
                  if Possible_Three_Shapes (Cur_Piece.S, Cur_Piece.D) (Y, X)
                  then
                     pragma Assert (Cur_Piece.Y + Y in Y_Coord);
                     pragma Assert (Cur_Piece.X + X in X_Coord);
                     Cur_Board (Cur_Piece.Y + Y) (Cur_Piece.X + X) :=
                       Cur_Piece.S;
                  end if;
               end loop;
            end loop;
      end case;

      --  update current state

      Cur_State := Board_Before_Clean;
   end Include_Piece_In_Board;

   ---------------------------
   -- Delete_Complete_Lines --
   ---------------------------

   procedure Delete_Complete_Lines (Number_Of_Complete_Lines : out Unsigned_32) is
      Empty_Line : constant Line := [others => Empty];

      To_Line : Y_Coord := Y_Coord'Last;
      Has_Complete_Lines : Boolean := False;

   begin
      Number_Of_Complete_Lines := 0;

      --  delete all complete lines, identifying the first complete line from
      --  the bottom (higher value of Y)

      for Del_Line in Y_Coord loop
         if Is_Complete_Line (Cur_Board (Del_Line)) then
            Cur_Board (Del_Line) := Empty_Line;
            Has_Complete_Lines := True;
            Number_Of_Complete_Lines := Number_Of_Complete_Lines + 1;
            To_Line := Del_Line;
            pragma Assert (Cur_Board (Del_Line)(X_Coord'First) = Empty);
         end if;
         pragma Loop_Invariant
           (for all Y in Y_Coord'First .. Del_Line =>
               not Is_Complete_Line (Cur_Board (Y)));
      end loop;

      --  iteratively move non-empty lines to the bottom of the board

      if Has_Complete_Lines then
         for From_Line in reverse Y_Coord'First .. To_Line - 1 loop
            pragma Loop_Invariant (No_Complete_Lines (Cur_Board));
            pragma Loop_Invariant (From_Line < To_Line);

            if not Is_Empty_Line (Cur_Board (From_Line)) then
               Cur_Board (To_Line) := Cur_Board (From_Line);
               pragma Assert (not Is_Complete_Line (Cur_Board (To_Line)));

               Cur_Board (From_Line) := Empty_Line;
               pragma Assert (not Is_Complete_Line (Cur_Board (From_Line)));

               To_Line := To_Line - 1;
               pragma Assert (Cur_Board (From_Line)(X_Coord'First) = Empty);
            end if;
         end loop;
      end if;

      pragma Assert (No_Complete_Lines (Cur_Board));

      --  update current state

      Cur_State := Board_After_Clean;
   end Delete_Complete_Lines;

   ----------
   -- Move --
   ----------

   function Move (P : Piece; A : Action) return Piece is
   begin
      case A is
         when Move_Left   => return (P with delta X => P.X - 1);
         when Move_Right  => return (P with delta X => P.X + 1);
         when Move_Down   => return (P with delta Y => P.Y + 1);
         when Turn_Action => return (P with delta D => Turn_Direction (P.D, A));
      end case;
   end Move;

   ---------------
   -- Do_Action --
   ---------------

   procedure Do_Action (A : Action; Success : out Boolean) is
      Candidate : Piece;
   begin
      if Move_Is_Possible (Cur_Piece, A) then
         Candidate := Move (Cur_Piece, A);

         if No_Overlap (Cur_Board, Candidate) then
            Cur_Piece := Candidate;
            Success := True;
            return;
         end if;
      end if;

      Success := False;
   end Do_Action;

end Tetris_Functional;
