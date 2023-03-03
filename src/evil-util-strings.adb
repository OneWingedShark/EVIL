Pragma Ada_2012;
Pragma Assertion_Policy( Check );

Package Body EVIL.Util.Strings with Pure, SPARK_Mode => On is
   Function Contains( Collection : String; Item : Character ) return Boolean is
      (for some C of Collection => C = Item) with Inline;

   Function Trim_Left( S : String; Ch : Character := Space ) return String is
   Begin
      Return Result : Constant String :=
        (if S'Length not in Positive then S
         elsif S(S'First) /= Ch then S
         else  S(Positive'Succ(S'First)..S'Last)
        );
   End Trim_Left;

--       (if S'Length not in Positive then S
--        elsif S(S'First) /= Ch then S
--        else  S(Positive'Succ(S'First)..S'Last)
--       );

   Function Trim_Right( S : String; Ch : Character := Space ) return String is
     (if S'Length not in Positive then S
      elsif S(S'Last) /= Ch then S
      else  S(S'First..Positive'Pred(S'Last))
     );

   Function Trim( S : String; Ch : Character := Space ) return String is
   Begin
      Return Result : Constant String:= ( Trim_Right( Trim_Left(S, Ch), Ch) );
   End Trim;


   Function Trim_Left( Input, Items : String ) return String is
--        (if Input'Length not in Positive or Items'Length not in Positive then Input
--         else (if Contains(Items, Item => Input(Input'First)) then Input
--               else Trim_Left(Input(Positive'Succ(Input'First)..Input'Last), Items)
--             )
--        );
   Begin
      if Input'Length not in Positive or Items'Length not in Positive then
         Return Input;
      else
         Return (if Contains(Items, Item => Input(Input'First)) then Input
                 else Trim_Left(Input(Positive'Succ(Input'First)..Input'Last), Items)
                );
      end if;
   End Trim_Left;


   Function Trim_Right( Input, Items : String ) return String is
   Begin
      if Input'Length not in Positive or Items'Length not in Positive then
         Return Input;
      else
         Return (if Contains(Items, Item => Input(Input'Last)) then Input
                 else Trim_Right(Input(Input'First..Positive'Pred(Input'Last)), Items)
                );
      end if;
   End Trim_Right;
--       (if Items'Length not in Positive then Input
--        else Trim_Right( Items => Items(Positive'Succ(Items'First)..Items'Last),
--                         Input => Trim_Right(Input, Items(Items'First))
--                      )
--       );

   Function Trim( Input, Items : String ) return String is
      Right : String renames Trim_Right( Input, Items );
   Begin
      Return Trim_Left( Right, Items );
   End Trim;
--       ( Trim_Right( Items => Items,
--                     Input => Trim_Left( Items => Items, Input => Input )
--                   )
--       );


   Package Body Search_and_Replace is

      Function Index( Input : String; Item : Character ) return Natural is
      Begin
         Return Result : Natural := 0 do
            Search:
            For Index in Input'Range loop
               if Input(Index) = Item then
                  Result := Index;
                  Exit Search;
               end if;
            End Loop Search;
         End return;
      End Index;

      Function Index( Input, Item : String ) return Natural is
         Subtype Item_Range is positive range Item'Range;

      Begin
         Return Result : Natural := 0 do
            if Item'Length > Input'Length then
               Return;
            end if;

            Search:
            For Index in Input'First..Input'Last-Natural'Pred(Item'Length) loop
               if Input(Index..Index+Positive'Pred(Item'Length)) = Item then
                  Result:= Index;
                  Exit Search;
               end if;
            End Loop Search;
         End return;
      End Index;

      --
      Function Replace( Object, Pattern, Replacement : String ) return String is
         Location : Natural renames Index( Object, Pattern );
      Begin
         if Location not in Positive then
            return Object;
         else
            declare
               subtype Head is Positive range Object'First..Positive'Pred(Location);
               subtype Tail is Positive range Location+Pattern'Length..Object'Last;
            begin
               Return Object(Head) & Replacement & Object(Tail);
            end;
         end if;
      End Replace;

      Function Replace_All( Object, Pattern, Replacement : String ) return String is
         Function Replace_Items( Obj : String;
                                 Pat : String := Pattern;
                                 Rep : String := Replacement
            ) return String renames Replace_All;
         Location : Natural renames Index( Object, Pattern );
      Begin
         if Location not in Positive then
            return Object;
         else
            declare
               subtype Head is Positive range Object'First..Positive'Pred(Location);
               subtype Tail is Positive range Location+Pattern'Length..Object'Last;
            begin
               Return Object(Head) & Replacement &
                 Replace_Items(Object(Tail), Pattern, Replacement);
            end;
         end if;
      End Replace_All;
   End Search_and_Replace;


End EVIL.Util.Strings;
