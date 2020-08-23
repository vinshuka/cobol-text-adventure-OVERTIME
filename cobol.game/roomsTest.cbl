      ******************************************************************
      * Author: Melissa Christie
      * Date: Aug 16 2020
      * Purpose: Test random room function
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROOMS-TEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-ROOMS-TEST
           ASSIGN TO 'C:\Text-Adventure\cobol.data\rooms-test.dat'
           ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-ROOMS-TEST
           BLOCK CONTAINS 0 RECORDS.
       01 INPUT-REC-ROOMS PIC X(80).
       WORKING-STORAGE SECTION.
      ******************************************************************
      *    LAYOUT FOR THE INPUT-ROOMS FILE
      ******************************************************************
       01 ROOMS-DATA.
           03 I-ROOMNUM        PIC 99.
           03 I-ROOMDESC       PIC X(15).
           03 FILLER           PIC X(63)   VALUE SPACES.
      ******************************************************************
      *    SETUP ROOMS TABLE
      ******************************************************************
       01 ROOMS-TABLE.
         03 ROOMS-FIELDS OCCURS 10 TIMES.
           05 T-ROOMNUM        PIC 99.
           05 T-ROOMDESC       PIC X(14).
           05 T-ROOMITEM       PIC X(13) VALUE SPACES.
           05 T-ROOMITEMNUM    PIC 99.
           05 T-ROOMMON        PIC 99    VALUE 00.
      ******************************************************************
      *    SETUP MAP TABLE
      ******************************************************************
       01 MAP-TABLE.
         03 MAP-FIELDS OCCURS 10 TIMES.
           05 T-MAP-NUM       PIC 99     VALUE ZEROS.
           05 T-MAP-NORTH     PIC 99     VALUE ZEROS.
           05 T-MAP-EAST      PIC 99     VALUE ZEROS.
           05 T-MAP-SOUTH     PIC 99     VALUE ZEROS.
           05 T-MAP-WEST      PIC 99     VALUE ZEROS.
      ******************************************************************
      *    PROGRAM VARIABLES
      ******************************************************************
       01 MISC.
           03 EOF-ROOMS        PIC 9       VALUE 0.
           03 ROOM-COUNT       PIC 99.
           03 MAP-COUNT        PIC 99      VALUE 01.
           03 COUNTER          PIC 99.
           03 CURRENT-ROOM     PIC 99.
           03 CONNECT-COUNT    PIC 99      VALUE 01.
           03 MOD-NUM          PIC 99.
      ******************************************************************
      *    RANDOMIZER VARIABLES
      ******************************************************************
           03 RANDOM-NUM       PIC 99.
           03 RANDOM-SEED      PIC 9(10)   VALUE ZEROS.
           03 RANDOM-NUM-SEED.
               04 SEED-DATE.
                  05  CURRENTYEAR     PIC 9(4).
                  05  CURRENTMONTH    PIC 99.
                  05  CURRENTDAY      PIC 99.
               04 SEED-TIME.
                  05  CURRENTHOUR     PIC 99.
                  05  CURRENTMINUTE   PIC 99.
                  05  CURRENTSSSS     PIC 9(4).
       PROCEDURE DIVISION.
      ******************************************************************
      *    MAIN PROGRAM LOGIC
      ******************************************************************
       000-MAINLINE.
           OPEN INPUT INPUT-ROOMS-TEST.
               PERFORM 1000-LOAD-ROOMS
           UNTIL EOF-ROOMS = 1.
           CLOSE INPUT-ROOMS-TEST.
           PERFORM 2000-RANDOMIZE-MAP.
      *     PERFORM 3000-PRINT-MAP.
           PERFORM 4000-CONNECT-ROOMS.
           PERFORM 5000-PRINT-MAP-DIRECTIONS.
           STOP RUN.
      ******************************************************************
      *    READS THE ROOMS FILE INTO ROOMS TABLE
      ******************************************************************
       1000-LOAD-ROOMS.
           READ INPUT-ROOMS-TEST INTO ROOMS-DATA
               AT END
                   MOVE 1 TO EOF-ROOMS
               NOT AT END
                   ADD 1 TO ROOM-COUNT
                   MOVE I-ROOMNUM   TO T-ROOMNUM(ROOM-COUNT)
                   MOVE I-ROOMDESC  TO T-ROOMDESC(ROOM-COUNT)
           END-READ.
      ******************************************************************
      *    RANDOMIZE MAP-TABLE
      ******************************************************************
       2000-RANDOMIZE-MAP.
           ACCEPT  SEED-TIME FROM TIME.
           MOVE SEED-TIME TO RANDOM-SEED.
           COMPUTE RANDOM-NUM = FUNCTION RANDOM (RANDOM-SEED).
           MOVE T-ROOMNUM(10) TO T-MAP-NUM(10)
           PERFORM UNTIL MAP-COUNT > 9
               COMPUTE RANDOM-NUM = FUNCTION RANDOM * 9 + 1
               IF (T-MAP-NUM(RANDOM-NUM) = 00) THEN
                   MOVE T-ROOMNUM(MAP-COUNT) TO T-MAP-NUM(RANDOM-NUM)
                   ADD 1 TO MAP-COUNT
               ELSE
                   CONTINUE
               END-IF
           END-PERFORM.
      ******************************************************************
      *    PRINT MAP
      ******************************************************************
       3000-PRINT-MAP.
           PERFORM UNTIL COUNTER > 10
               MOVE T-MAP-NUM(COUNTER) TO CURRENT-ROOM
               DISPLAY "MAP:", COUNTER
               DISPLAY "ROOM-NUM:", CURRENT-ROOM
               DISPLAY "DESC:", T-ROOMDESC(CURRENT-ROOM)
               ADD 1 TO COUNTER
           END-PERFORM.
      ******************************************************************
      *    CONNECT-ROOMS
      ******************************************************************
       4000-CONNECT-ROOMS.
           PERFORM UNTIL CONNECT-COUNT > 9
      *    USING MOD FUNCTION TO CHECK IF ROOM HAS EAST OR WEST WALL
               COMPUTE MOD-NUM = FUNCTION MOD (CONNECT-COUNT, 3)
               IF ((CONNECT-COUNT - 3) > 00 AND
                   (CONNECT-COUNT - 3) <= 09) THEN
                    SUBTRACT 3 FROM CONNECT-COUNT GIVING
                    T-MAP-NORTH(CONNECT-COUNT)
               ELSE
                   MOVE 00 TO T-MAP-NORTH(CONNECT-COUNT)
               END-IF
               IF ((CONNECT-COUNT + 1) > 00 AND
                   (CONNECT-COUNT + 1) <= 09 AND
                   MOD-NUM NOT = 00) THEN
                   ADD 1 TO CONNECT-COUNT GIVING
                   T-MAP-EAST(CONNECT-COUNT)
               ELSE
                   MOVE 00 TO T-MAP-EAST(CONNECT-COUNT)
               END-IF
               IF ((CONNECT-COUNT + 3) > 00 AND
                   (CONNECT-COUNT + 3) <= 9) THEN
                   ADD 3 TO CONNECT-COUNT GIVING
                   T-MAP-SOUTH(CONNECT-COUNT)
               ELSE
                   MOVE 00 TO T-MAP-SOUTH(CONNECT-COUNT)
               END-IF
               IF ((CONNECT-COUNT - 1) > 00 AND
                   (CONNECT-COUNT - 1) <= 9 AND
                   MOD-NUM NOT = 01) THEN
                   SUBTRACT 1 FROM CONNECT-COUNT GIVING
                   T-MAP-WEST(CONNECT-COUNT)
               ELSE
                   MOVE 00 TO T-MAP-WEST(CONNECT-COUNT)
               END-IF
               ADD 1 TO CONNECT-COUNT
           END-PERFORM
      *    SET STARTING ROOM NORTH EXIT TO ENTER ON MAP 09
           MOVE 09 TO T-MAP-NORTH(10).
      ******************************************************************
      *    PRINT MAP DIRECTIONS
      ******************************************************************
       5000-PRINT-MAP-DIRECTIONS.
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 10
               MOVE T-MAP-NUM(COUNTER) TO CURRENT-ROOM
               DISPLAY "MAP:", COUNTER
               DISPLAY "ROOM-NUM:", CURRENT-ROOM
               DISPLAY "DESC:", T-ROOMDESC(CURRENT-ROOM)
               DISPLAY "NORTH:", T-MAP-NORTH(COUNTER)
               DISPLAY "EAST:", T-MAP-EAST(COUNTER)
               DISPLAY "SOUTH:", T-MAP-SOUTH(COUNTER)
               DISPLAY "WEST:", T-MAP-WEST(COUNTER)
           END-PERFORM.
       END PROGRAM ROOMS-TEST.
