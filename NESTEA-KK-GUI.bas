SUB DrawScrolling (NameTableAddress%, Hscroll%, Vscroll%)
    IF MemoryRegisters(0) AND 16 THEN PatternTableAddress% = 4096 ELSE PatternTableAddress% = 0
    IF (MemoryRegisters(1) AND 2) = 0 THEN BGcutoff% = 40 ELSE BGcutoff% = 32
    NameTableAddress% = NameTableAddress% + Mirroring(NameTableAddress% \ 1024)
    IF DebuggerFlag = 0 THEN
        AttributeTableAddress% = NameTableAddress% + 960
        FOR AttributeY% = 0 TO 14 STEP 2
            FOR AttributeX% = 0 TO 14 STEP 2
                AttributeColor% = NameTables(AttributeTableAddress%)
                AttributeTableAddress% = AttributeTableAddress% + 1
                AttributeTable(AttributeX%, AttributeY%) = (AttributeColor% AND 3) * 4
                AttributeTable(AttributeX% + 1, AttributeY%) = AttributeColor% AND 12
                AttributeTable(AttributeX%, AttributeY% + 1) = (AttributeColor% AND 48) \ 4
                AttributeTable(AttributeX% + 1, AttributeY% + 1) = (AttributeColor% AND 192) \ 16
            NEXT AttributeX%
        NEXT AttributeY%
        FOR TileY% = 0 TO 232 STEP 8
            IF TileY% + Vscroll% >= -7 AND TileY% + Vscroll% <= 239 THEN
                FOR TileX% = 0 TO 248 STEP 8
                    IF TileX% + Hscroll% >= -8 AND TileX% + Hscroll% <= 255 THEN
                        PixelColorHigh% = AttributeTable(TileX% \ 16, TileY% \ 16)
                        TileAddress% = PatternTableAddress% + NameTables(NameTableAddress% + TileY% * 4 + TileX% \ 8) * 16
                        FOR TileByte% = 0 TO 15: Tile(TileByte%) = PatternTables(TileAddress% + TileByte%): NEXT
                        FOR PixelX% = 0 TO 7
                            CurrentPixelX% = (TileX% - PixelX% + Hscroll% + 39)
                            IF CurrentPixelX% >= BGcutoff% AND CurrentPixelX% < 288 THEN
                                OUT &H3C4, 2: OUT &H3C5, PowerOfTwo(CurrentPixelX% AND 3)
                                TwoExpPixelX% = PowerOfTwo(PixelX%)
                                FOR PixelY% = 0 TO 7
                                    CurrentPixelY% = TileY% + PixelY% + Vscroll%
                                    IF CurrentPixelY% >= 8 AND CurrentPixelY% < 232 THEN
                                        IF Tile(PixelY%) AND TwoExpPixelX% THEN PixelColor% = 1 ELSE PixelColor% = 0
                                        IF Tile(PixelY% + 8) AND TwoExpPixelX% THEN PixelColor% = PixelColor% + 2
                                        POKE CurrentPixelX% \ 4 + CurrentPixelY% * 80, BackgroundPalette(PixelColorHigh% + PixelColor%) + 1
                                    END IF
                                NEXT PixelY%
                            END IF
                        NEXT PixelX%
                    END IF
                NEXT TileX%
            END IF
        NEXT TileY%
    END IF
END SUB


SUB DrawBitmap (BitmapStyle%, FileName$, ColorsUsed%, BitmapX%, BitmapY%)
    FileHandle% = FREEFILE
    OPEN FileName$ FOR BINARY AS FileHandle%
    IF LOF(FileHandle%) = 0 THEN CLOSE FileHandle%: KILL FileName$: ERROR 254
    GET FileHandle%, 19, BitmapWidth%
    GET FileHandle%, 23, BitmapHeight%
    SEEK FileHandle%, 55
    PictureData$ = INPUT$(1024, FileHandle%)
    FOR PaletteColor% = 0 TO ColorsUsed%
        FourBytes$ = MID$(PictureData$, PaletteColor% * 4 + 1, 4)
        OUT 968, PaletteColor%
        OUT 969, ASC(MID$(FourBytes$, 3, 1)) \ 4
        OUT 969, ASC(MID$(FourBytes$, 2, 1)) \ 4
        OUT 969, ASC(FourBytes$) \ 4
    NEXT PaletteColor%
    GET FileHandle%, 11, BitmapOffset%
    SEEK FileHandle%, BitmapOffset% + 1
    SELECT CASE BitmapStyle%
        CASE 1
            BitmapWidth% = (((BitmapWidth% - 1) OR 7) + 1) \ 2
            FOR Row% = BitmapY% + BitmapHeight% - 1 TO BitmapY% STEP -1
                RowData$ = INPUT$(BitmapWidth%, FileHandle%)
                FOR Column% = 0 TO BitmapWidth% - 1
                    TwoPixels% = ASC(MID$(RowData$, Column% + 1, 1))
                    PSET (Column% * 2 + BitmapX%, Row%), TwoPixels% \ 16
                    PSET (Column% * 2 + BitmapX% + 1, Row%), TwoPixels% AND 15
                NEXT Column%
            NEXT Row%
        CASE 2
            BitmapWidth% = (((BitmapWidth% - 1) OR 7) + 1) \ 2
            FOR Row% = (BitmapHeight% - 1) * 2 TO 0 STEP -2
                RowData$ = INPUT$(BitmapWidth%, FileHandle%)
                FOR Column% = 0 TO (BitmapWidth% - 1) * 2 STEP 2
                    TwoPixels% = ASC(MID$(RowData$, Column% \ 2 + 1, 1))
                    PSET (Column% * 2, Row%), TwoPixels% \ 16
                    PSET (Column% * 2 + 1, Row%), TwoPixels% \ 16
                    PSET (Column% * 2, Row% + 1), TwoPixels% \ 16
                    PSET (Column% * 2 + 1, Row% + 1), TwoPixels% \ 16
                    PSET (Column% * 2 + 2, Row%), TwoPixels% AND 15
                    PSET (Column% * 2 + 3, Row%), TwoPixels% AND 15
                    PSET (Column% * 2 + 2, Row% + 1), TwoPixels% AND 15
                    PSET (Column% * 2 + 3, Row% + 1), TwoPixels% AND 15
                NEXT Column%
            NEXT Row%
    END SELECT
    CLOSE FileHandle%
END SUB


FUNCTION TextBox$ (Column%, Row%, Length%)
    DrawObject 21, Column% - 2, Row% - 2, Column% + Length% * 8 + 1, Row% + 17
    DrawText 1, STRING$(Length%, "°"), Column%, Row%, 10
    CursorPosition% = Column%
    DO
        DO
            PressedButton$ = INKEY$
            IF PressedButton$ <> "" THEN EXIT DO
        LOOP
        SELECT CASE ASC(PressedButton$)
            CASE 8
                IF CursorPosition% > Column% THEN
                    TextString$ = LEFT$(TextString$, LEN(TextString$) - 1)
                    CursorPosition% = CursorPosition% - 8
                    LINE (CursorPosition%, Row%)-(CursorPosition% + 7, Row% + 15), 13, BF
                    DrawText 1, "°", CursorPosition%, Row%, 10
                END IF
            CASE 13: EXIT DO
            CASE 32 TO 126
                IF CursorPosition% < Column% + Length% * 8 THEN
                    TextString$ = TextString$ + PressedButton$
                    DrawText 0, PressedButton$, CursorPosition%, Row%, 0
                    CursorPosition% = CursorPosition% + 8
                END IF
        END SELECT
    LOOP
    TextBox$ = TextString$
END FUNCTION

SUB WaitVblank (Waits%)
    FOR Counter% = 1 TO Waits%
        WAIT &H3DA, 8, 8: WAIT &H3DA, 8
    NEXT Counter%
END SUB

SUB DrawObject (WhichObject%, x1%, y1%, x2%, y2%)
    SELECT CASE WhichObject%
        CASE 0
            LINE (x1%, y1%)-(x2%, y2%), 13, BF
            LINE (x1%, y1%)-(x2% - 1, y1%), 15
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 15
            LINE (x1% + 1, y1% + 1)-(x2% - 2, y1% + 1), 14
            LINE (x1% + 1, y1% + 2)-(x1% + 1, y2% - 2), 14
            LINE (x2%, y1%)-(x2%, y2%), 0
            LINE (x1%, y2%)-(x2% - 1, y2%), 0
            LINE (x2% - 1, y1% + 1)-(x2% - 1, y2% - 1), 12
            LINE (x1% + 1, y2% - 1)-(x2% - 2, y2% - 1), 12
            LINE (x1% + 4, y1% + 22)-(x2% - 4, y1% + 22), 12
            LINE (x1% + 4, y1% + 23)-(x1% + 4, y2% - 22), 12
            LINE (x1% + 5, y1% + 23)-(x2% - 5, y1% + 23), 0
            LINE (x1% + 5, y1% + 24)-(x1% + 5, y2% - 23), 0
            LINE (x2% - 4, y1% + 23)-(x2% - 4, y2% - 22), 14
            LINE (x1% + 5, y2% - 22)-(x2% - 5, y2% - 22), 14
            LINE (x2% - 5, y1% + 24)-(x2% - 5, y2% - 23), 15
            LINE (x1% + 6, y2% - 23)-(x2% - 6, y2% - 23), 15
        CASE 10
            LINE (x1%, y1%)-(x2% - 1, y1%), 15
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 15
            LINE (x2%, y1%)-(x2%, y2%), 0
            LINE (x1%, y2%)-(x2% - 1, y2%), 0
        CASE 11
            LINE (x1%, y1%)-(x2% - 1, y1%), 0
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 0
            LINE (x2%, y1%)-(x2%, y2%), 15
            LINE (x1%, y2%)-(x2% - 1, y2%), 15
        CASE 20
            LINE (x1%, y1%)-(x2% - 1, y1%), 15
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 15
            LINE (x1% + 1, y1% + 1)-(x2% - 2, y1% + 1), 14
            LINE (x1% + 1, y1% + 2)-(x1% + 1, y2% - 2), 14
            LINE (x2%, y1%)-(x2%, y2%), 0
            LINE (x1%, y2%)-(x2% - 1, y2%), 0
            LINE (x2% - 1, y1% + 1)-(x2% - 1, y2% - 1), 12
            LINE (x1% + 1, y2% - 1)-(x2% - 2, y2% - 1), 12
        CASE 21
            LINE (x1%, y1%)-(x2% - 1, y1%), 0
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 0
            LINE (x1% + 1, y1% + 1)-(x2% - 2, y1% + 1), 12
            LINE (x1% + 1, y1% + 2)-(x1% + 1, y2% - 2), 12
            LINE (x2%, y1%)-(x2%, y2%), 15
            LINE (x1%, y2%)-(x2% - 1, y2%), 15
            LINE (x2% - 1, y1% + 1)-(x2% - 1, y2% - 1), 14
            LINE (x1% + 1, y2% - 1)-(x2% - 2, y2% - 1), 14
        CASE 40
            LINE (x1%, y1%)-(x2% - 1, y1%), 15
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 15
            LINE (x1% + 1, y1% + 1)-(x2% - 2, y1% + 1), 15
            LINE (x1% + 1, y1% + 2)-(x1% + 1, y2% - 2), 15
            LINE (x1% + 2, y1% + 2)-(x2% - 3, y1% + 2), 14
            LINE (x1% + 2, y1% + 3)-(x1% + 2, y2% - 3), 14
            LINE (x1% + 3, y1% + 3)-(x2% - 4, y1% + 3), 14
            LINE (x1% + 3, y1% + 4)-(x1% + 3, y2% - 4), 14
            LINE (x2%, y1%)-(x2%, y2%), 0
            LINE (x1%, y2%)-(x2% - 1, y2%), 0
            LINE (x2% - 1, y1% + 1)-(x2% - 1, y2% - 1), 0
            LINE (x1% + 1, y2% - 1)-(x2% - 2, y2% - 1), 0
            LINE (x2% - 2, y1% + 2)-(x2% - 2, y2% - 2), 12
            LINE (x1% + 2, y2% - 2)-(x2% - 3, y2% - 2), 12
            LINE (x2% - 3, y1% + 3)-(x2% - 3, y2% - 3), 12
            LINE (x1% + 3, y2% - 3)-(x2% - 4, y2% - 3), 12
        CASE 41
            LINE (x1%, y1%)-(x2% - 1, y1%), 0
            LINE (x1%, y1% + 1)-(x1%, y2% - 1), 0
            LINE (x1% + 1, y1% + 1)-(x2% - 2, y1% + 1), 0
            LINE (x1% + 1, y1% + 2)-(x1% + 1, y2% - 2), 0
            LINE (x1% + 2, y1% + 2)-(x2% - 3, y1% + 2), 12
            LINE (x1% + 2, y1% + 3)-(x1% + 2, y2% - 3), 12
            LINE (x1% + 3, y1% + 3)-(x2% - 4, y1% + 3), 12
            LINE (x1% + 3, y1% + 4)-(x1% + 3, y2% - 4), 12
            LINE (x2%, y1%)-(x2%, y2%), 15
            LINE (x1%, y2%)-(x2% - 1, y2%), 15
            LINE (x2% - 1, y1% + 1)-(x2% - 1, y2% - 1), 15
            LINE (x1% + 1, y2% - 1)-(x2% - 2, y2% - 1), 15
            LINE (x2% - 2, y1% + 2)-(x2% - 2, y2% - 2), 14
            LINE (x1% + 2, y2% - 2)-(x2% - 3, y2% - 2), 14
            LINE (x2% - 3, y1% + 3)-(x2% - 3, y2% - 3), 14
            LINE (x1% + 3, y2% - 3)-(x2% - 4, y2% - 3), 14
    END SELECT
END SUB

SUB DrawText (TextFont%, TextToType$, TextX%, TextY%, TextColor%)
    SELECT CASE VideoMode
        CASE 12
            IF TextFont% = 0 THEN DEF SEG = VARSEG(Font0): FontOffset% = VARPTR(Font0) ELSE DEF SEG = VARSEG(Font1): FontOffset% = VARPTR(Font1)
            FOR TextCharacter% = 1 TO LEN(TextToType$)
                X% = (TextCharacter% - 1) * 8 + TextX%
                ASCIIcode% = ASC(MID$(TextToType$, TextCharacter%, 1))
                FOR Y% = TextY% TO TextY% + 15
                    TextPixels% = PEEK(FontOffset% + ASCIIcode% * 16& + Y% - TextY%)
                    IF TextPixels% AND 128 THEN PSET (X%, Y%), TextColor%
                    IF TextPixels% AND 64 THEN PSET (X% + 1, Y%), TextColor%
                    IF TextPixels% AND 32 THEN PSET (X% + 2, Y%), TextColor%
                    IF TextPixels% AND 16 THEN PSET (X% + 3, Y%), TextColor%
                    IF TextPixels% AND 8 THEN PSET (X% + 4, Y%), TextColor%
                    IF TextPixels% AND 4 THEN PSET (X% + 5, Y%), TextColor%
                    IF TextPixels% AND 2 THEN PSET (X% + 6, Y%), TextColor%
                    IF TextPixels% AND 1 THEN PSET (X% + 7, Y%), TextColor%
                NEXT Y%
            NEXT TextCharacter%
        CASE 14
            TextX% = TextX% \ 2: TextY% = TextY% \ 2
            IF TextFont% = 0 THEN DEF SEG = VARSEG(Font0): FontOffset% = VARPTR(Font0) ELSE DEF SEG = VARSEG(Font1): FontOffset% = VARPTR(Font1)
            FOR TextCharacter% = 1 TO LEN(TextToType$)
                CharacterX% = ((TextCharacter% - 1) * 8 + TextX%)
                ASCIIcode% = ASC(MID$(TextToType$, TextCharacter%, 1))
                FOR Y% = TextY% * 80 TO (TextY% + 15) * 80 STEP 80
                    IF TextFont% = 0 THEN DEF SEG = VARSEG(Font0) ELSE DEF SEG = VARSEG(Font1)
                    TextPixels% = PEEK(FontOffset% + ASCIIcode% * 16& + (Y% \ 80) - TextY%)
                    IF VideoPage THEN DEF SEG = &HA000 ELSE DEF SEG = &HA4F0
                    FOR X% = CharacterX% TO CharacterX% + 7
                        OUT &H3C4, 2: OUT &H3C5, PowerOfTwo(X% AND 3)
                        IF TextPixels% AND PowerOfTwo(7 - (X% - CharacterX%)) THEN POKE X% \ 4 + Y%, TextColor%
                    NEXT X%
                NEXT Y%
            NEXT TextCharacter%
    END SELECT
END SUB



























































































