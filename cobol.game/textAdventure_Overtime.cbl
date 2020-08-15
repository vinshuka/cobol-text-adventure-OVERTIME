      ******************************************************************
      * Author: Melissa Christie
      * Date: Aug 10 2020
      * Purpose: Text-Adventure Game
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERTIME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-ROOMS
           ASSIGN TO 'C:\Text-Adventure\cobol.data\rooms.dat'
           ORGANISATION IS LINE SEQUENTIAL.
           SELECT INPUT-ITEMS
           ASSIGN TO 'C:\Text-Adventure\cobol.data\items.dat'
           ORGANISATION IS LINE SEQUENTIAL.
           SELECT INPUT-MONSTERS
           ASSIGN TO 'C:\Text-Adventure\cobol.data\monsters.dat'
           ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-ROOMS
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC-ROOMS PIC X(80).
       FD INPUT-ITEMS
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC-ITEMS PIC X(80).
       FD INPUT-MONSTERS
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC-MONSTERS PIC X(80).
       WORKING-STORAGE SECTION.
      ******************************************************************
      *    LAYOUT FOR THE INPUT-ROOMS FILE
      ******************************************************************
       01 ROOMS-DATA.
           03 I-ROOMNUM        PIC 99.
           03 I-ROOMDESC       PIC X(15).
           03 I-ROOMNORTH      PIC 99.
           03 I-ROOMEAST       PIC 99.
           03 I-ROOMSOUTH      PIC 99.
           03 I-ROOMWEST       PIC 99.
           03 FILLER           PIC X(55)   VALUE SPACES.
      ******************************************************************
      *    LAYOUT FOR THE INPUT-ITEMS FILE
      ******************************************************************
       01 ITEMS-DATA.
           03 I-ITEMNUM        PIC 99.
           03 I-ITEMDESC       PIC X(13).
           03 I-ITEMWEIGHT     PIC 99.
           03 I-ITEMRESTORE    PIC 99.
           03 I-ITEMDAMAGE     PIC 99.
           03 I-ITEMLOC        PIC 99.
           03 I-ITEMWIN        PIC 99.
           03 FILLER           PIC X(57)   VALUE SPACES.
      ******************************************************************
      *    LAYOUT FOR THE INPUT-MONSTERS FILE
      ******************************************************************
       01 MONSTERS-DATA.
           03 I-MONNUM         PIC 99.
           03 I-MONDESC        PIC X(11).
           03 I-MONTEXT        PIC X(33).
           03 I-MONLOC         PIC 99.
           03 I-MONATTACK      PIC 99.
           03 I-MONHP          PIC 99.
           03 FILLER           PIC X(28)   VALUE SPACES.
      ******************************************************************
      *    SETUP ROOMS TABLE
      ******************************************************************
       01 ROOMS-TABLE.
         03 ROOMS-FIELDS OCCURS 10 TIMES.
           05 T-ROOMNUM        PIC 99.
           05 T-ROOMDESC       PIC X(14).
           05 T-ROOMNORTH      PIC 99.
           05 T-ROOMEAST       PIC 99.
           05 T-ROOMSOUTH      PIC 99.
           05 T-ROOMWEST       PIC 99.
           05 T-ROOMITEM       PIC X(13) VALUE SPACES.
           05 T-ROOMITEMNUM    PIC 99.
           05 T-ROOMMON        PIC 99    VALUE 00.
      ******************************************************************
      *    SETUP ITEMS TABLE
      ******************************************************************
       01 ITEMS-TABLE.
         03 ITEMS-FIELDS OCCURS 10 TIMES.
           05 T-ITEMNUM        PIC 99.
           05 T-ITEMDESC       PIC X(13).
           05 T-ITEMWEIGHT     PIC 99.
           05 T-ITEMRESTORE    PIC 99.
           05 T-ITEMDAMAGE     PIC 99.
           05 T-ITEMLOC        PIC 99.
           05 T-ITEMWIN        PIC 99.
      ******************************************************************
      *    SETUP MONSTERS TABLE
      ******************************************************************
       01 MONSTERS-TABLE.
         03 MONSTERS-FIELDS OCCURS 7 TIMES.
           05 T-MONNUM         PIC 99.
           05 T-MONDESC        PIC X(11).
           05 T-MONTEXT        PIC X(33).
           05 T-MONLOC         PIC 99.
           05 T-MONATTACK      PIC 99.
           05 T-MONHP          PIC 99.
      ******************************************************************
      *    BRIEFCASE TABLE
      ******************************************************************
       01 BRIEFCASE.
         03 BRIEFCASE-FIELDS OCCURS 10 TIMES.
           05 B-ITEMNUM        PIC 99  VALUE ZEROS.
      ******************************************************************
      *    PROGRAM VARIABLES
      ******************************************************************
       01 MISC.
           03 EOF-ROOMS        PIC 9       VALUE 0.
           03 EOF-ITEMS        PIC 9       VALUE 0.
           03 EOF-MONSTERS     PIC 9       VALUE 0.
           03 ROOM-COUNT       PIC 99.
           03 ITEM-COUNT       PIC 99.
           03 MONSTER-COUNT    PIC 99.
           03 PLACE-ITEM       PIC 99      VALUE 01.
           03 PLACE-MON        PIC 99      VALUE 01.
           03 ITEM-NUM         PIC 99.
           03 MON-NUM          PIC 99.
           03 COMMAND-INPUT    PIC X(22)   VALUE SPACES.
           03 COMMAND          PIC X(10)   VALUE SPACES.
           03 COMMAND-PARM     PIC X(12)   VALUE SPACES.
           03 CURRENT-ROOM     PIC 99      VALUE 01.
           03 NEXT-ROOM        PIC 99.
           03 WILL-TO-LIVE     PIC 999     VALUE 100.
           03 BRIEFCASE-WEIGHT PIC 99      VALUE 00.
           03 ITEM-TO-PICKUP   PIC 99.
           03 BRIEFCASE-SLOT   PIC 99      VALUE 01.
           03 B-COUNT          PIC 99.
           03 DROPPED          PIC 9.
           03 B-ITEM           PIC 99.
           03 USED             PIC 9.
           03 PLAYER-ATTACK    PIC 99      VALUE 05.
           03 CURRENT-ITEM     PIC 99.
           03 CURRENT-MON      PIC 99.
           03 MON-HP           PIC 99.
           03 CURRENT-ATTACK   PIC 99.
           03 RANDOM-NUM       PIC 99.
           03 RANDOM-SEED PIC 9(10) VALUE ZEROS.
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
           OPEN INPUT INPUT-ROOMS.
               PERFORM 1000-LOAD-ROOMS
           UNTIL EOF-ROOMS = 1.
           CLOSE INPUT-ROOMS.
           OPEN INPUT INPUT-ITEMS.
               PERFORM 2000-LOAD-ITEMS
           UNTIL EOF-ITEMS = 1.
           CLOSE INPUT-ITEMS.
           OPEN INPUT INPUT-MONSTERS.
               PERFORM 3000-LOAD-MONSTERS
           UNTIL EOF-MONSTERS = 1.
           CLOSE INPUT-MONSTERS.
           PERFORM 2500-PLACE-ITEMS
           PERFORM 3500-PLACE-MONSTERS
      *     ACCEPT  SEED-TIME FROM TIME.
      *     MOVE SEED-TIME TO RANDOM-SEED.
      *     DISPLAY "RANDOM-SEED ", RANDOM-SEED
      *     COMPUTE RANDOM-NUM = FUNCTION RANDOM (RANDOM-SEED).
           DISPLAY "YOU FORGOT YOUR KEYS IN THE OFFICE. YOU MUST"
               DISPLAY "RETRIEVE THEM BEFORE YOUR BOSS FINDS YOU OR"
               DISPLAY "YOU RUN OUT OF YOUR WILL TO LIVE. ELSE"
               DISPLAY "ELSE YOU'LL BE WORKING OVERTIME THIS WEEKEND."
               DISPLAY "ENTER 'HELP' FOR COMMAND LIST"
               DISPLAY "YOU ARE IN ", T-ROOMDESC(CURRENT-ROOM).
           PERFORM UNTIL (COMMAND = 'QUIT' OR WILL-TO-LIVE <= 0)
               DISPLAY "-----------------------------------------------"
               DISPLAY "WILL TO LIVE:", WILL-TO-LIVE
               DISPLAY "BRIEFCASE WEIGHT: ", BRIEFCASE-WEIGHT
               DISPLAY "WHAT WOULD YOU LIKE TO DO?"
               DISPLAY "-----------------------------------------------"
               ACCEPT COMMAND-INPUT
               UNSTRING COMMAND-INPUT
                    DELIMITED BY ALL ' '
                    INTO COMMAND
                         COMMAND-PARM
               PERFORM 4000-PROCESS-COMMAND
           END-PERFORM
           STOP RUN.
      ******************************************************************
      *    READS THE ROOMS FILE INTO ROOMS TABLE
      ******************************************************************
       1000-LOAD-ROOMS.
           READ INPUT-ROOMS INTO ROOMS-DATA
               AT END
                   MOVE 1 TO EOF-ROOMS
               NOT AT END
                   ADD 1 TO ROOM-COUNT
                   MOVE I-ROOMNUM   TO T-ROOMNUM(ROOM-COUNT)
                   MOVE I-ROOMDESC  TO T-ROOMDESC(ROOM-COUNT)
                   MOVE I-ROOMNORTH TO T-ROOMNORTH(ROOM-COUNT)
                   MOVE I-ROOMEAST  TO T-ROOMEAST(ROOM-COUNT)
                   MOVE I-ROOMSOUTH TO T-ROOMSOUTH(ROOM-COUNT)
                   MOVE I-ROOMWEST  TO T-ROOMWEST(ROOM-COUNT)
           END-READ.
      ******************************************************************
      *    READS THE ITEMS FILE INTO ITEMS TABLE
      ******************************************************************
       2000-LOAD-ITEMS.
           READ INPUT-ITEMS INTO ITEMS-DATA
               AT END
                   MOVE 1 TO EOF-ITEMS
               NOT AT END
                   ADD 1 TO ITEM-COUNT
                   MOVE I-ITEMNUM      TO T-ITEMNUM(ITEM-COUNT)
                   MOVE I-ITEMDESC     TO T-ITEMDESC(ITEM-COUNT)
                   MOVE I-ITEMWEIGHT   TO T-ITEMWEIGHT(ITEM-COUNT)
                   MOVE I-ITEMRESTORE  TO T-ITEMRESTORE(ITEM-COUNT)
                   MOVE I-ITEMDAMAGE   TO T-ITEMDAMAGE(ITEM-COUNT)
                   MOVE I-ITEMLOC      TO T-ITEMLOC(ITEM-COUNT)
                   MOVE I-ITEMWIN      TO T-ITEMWIN(ITEM-COUNT)
           END-READ.
      ******************************************************************
      *    PLACES ITEMS INTO ROOMS
      ******************************************************************
       2500-PLACE-ITEMS.
           PERFORM UNTIL PLACE-ITEM = 11
               MOVE T-ITEMLOC(PLACE-ITEM)  TO ITEM-NUM
               MOVE T-ITEMDESC(PLACE-ITEM) TO T-ROOMITEM(ITEM-NUM)
               MOVE T-ITEMNUM(PLACE-ITEM)  TO T-ROOMITEMNUM(ITEM-NUM)
               ADD 1 TO PLACE-ITEM
           END-PERFORM.
      ******************************************************************
      *    READS THE MONSTERS FILE INTO MONSTERS TABLE
      ******************************************************************
       3000-LOAD-MONSTERS.
           READ INPUT-MONSTERS INTO MONSTERS-DATA
               AT END
                   MOVE 1 TO EOF-MONSTERS
               NOT AT END
                   ADD 1 TO MONSTER-COUNT
                   MOVE I-MONNUM    TO T-MONNUM(MONSTER-COUNT)
                   MOVE I-MONDESC   TO T-MONDESC(MONSTER-COUNT)
                   MOVE I-MONTEXT   TO T-MONTEXT(MONSTER-COUNT)
                   MOVE I-MONLOC    TO T-MONLOC(MONSTER-COUNT)
                   MOVE I-MONATTACK TO T-MONATTACK(MONSTER-COUNT)
                   MOVE I-MONHP     TO T-MONHP(MONSTER-COUNT)
           END-READ.
      ******************************************************************
      *    PLACES MONSTERS INTO ROOM
      ******************************************************************
       3500-PLACE-MONSTERS.
           ACCEPT  SEED-TIME FROM TIME.
           MOVE SEED-TIME TO RANDOM-SEED.
           DISPLAY "RANDOM-SEED ", RANDOM-SEED
           COMPUTE RANDOM-NUM = FUNCTION RANDOM (RANDOM-SEED).
           PERFORM UNTIL PLACE-MON = 8
               COMPUTE RANDOM-NUM = FUNCTION RANDOM * 2 + 1
               DISPLAY "RANDOM-NUM ", RANDOM-NUM
               IF (RANDOM-NUM = 2) THEN
                   MOVE T-MONLOC(PLACE-MON) TO MON-NUM
                   MOVE T-MONNUM(PLACE-MON) TO T-ROOMMON(MON-NUM)
               END-IF
               ADD 1 TO PLACE-MON
           END-PERFORM.
      ******************************************************************
      *    PROCESS COMMAND
      ******************************************************************
       4000-PROCESS-COMMAND.
           MOVE 01 TO B-COUNT
           MOVE 0  TO DROPPED
           MOVE 0  TO USED
           IF (COMMAND = "GO") THEN
               PERFORM 4100-PROCESS-GO
           ELSE
               IF (COMMAND = "PICKUP") THEN
                   PERFORM 4200-PROCESS-PICKUP
               ELSE
                   IF (COMMAND = "DROP") THEN
                       PERFORM 4300-PROCESS-DROP
                   ELSE
                       IF (COMMAND = "USE") THEN
                           PERFORM 4400-PROCESS-USE
                       ELSE
                           IF (COMMAND = "LOOK") THEN
                               PERFORM 4500-PROCESS-LOOK
                           ELSE
                               IF (COMMAND = "BRIEFCASE") THEN
                                   PERFORM 4600-PROCESS-BRIEFCASE
                               ELSE
                                   IF (COMMAND = "ATTACK") THEN
                                       PERFORM 4700-PROCESS-ATTACK
                                   ELSE
                                       IF (COMMAND = "HELP") THEN
                                           PERFORM 4800-PROCESS-HELP
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.
      ******************************************************************
      *    4100-PROCESS-GO
      ******************************************************************
       4100-PROCESS-GO.
           IF (COMMAND-PARM = "NORTH") THEN
               MOVE T-ROOMNORTH(CURRENT-ROOM) TO NEXT-ROOM
           ELSE
               IF (COMMAND-PARM = "EAST") THEN
                   MOVE T-ROOMEAST(CURRENT-ROOM) TO NEXT-ROOM
               ELSE
                   IF (COMMAND-PARM = "SOUTH") THEN
                       MOVE T-ROOMSOUTH(CURRENT-ROOM) TO NEXT-ROOM
                   ELSE
                       IF (COMMAND-PARM = "WEST") THEN
                           MOVE T-ROOMWEST(CURRENT-ROOM) TO NEXT-ROOM
                       END-IF
                   END-IF
               END-IF
           END-IF.
           IF (NEXT-ROOM = ZEROS) THEN
               DISPLAY "THERE IS NO ROOM IN THAT DIRECTION"
           ELSE
               PERFORM 4150-ENTER-NEXT-ROOM
           END-IF.
      ******************************************************************
      *    4150-ENTER-NEXT-ROOM
      ******************************************************************
       4150-ENTER-NEXT-ROOM.
           COMPUTE WILL-TO-LIVE = WILL-TO-LIVE - (BRIEFCASE-WEIGHT * .2)
           MOVE NEXT-ROOM TO CURRENT-ROOM
           DISPLAY "YOU ARE IN ", T-ROOMDESC(CURRENT-ROOM)
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               PERFORM 4900-MONSTER-ATTACK
           END-IF.
      ******************************************************************
      *    4200-PROCESS-PICKUP
      ******************************************************************
       4200-PROCESS-PICKUP.
           MOVE T-ROOMITEMNUM(CURRENT-ROOM) TO ITEM-TO-PICKUP
           IF (COMMAND-PARM = T-ITEMDESC(ITEM-TO-PICKUP)) THEN
               PERFORM 4250-PUT-IN-BRIEFCASE
           ELSE
               DISPLAY "THAT ITEM IS NOT HERE"
           END-IF
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               PERFORM 4900-MONSTER-ATTACK
           END-IF.
      ******************************************************************
      *    4250-PUT-IN-BRIEFCASE
      ******************************************************************
       4250-PUT-IN-BRIEFCASE.
           MOVE 01 TO BRIEFCASE-SLOT
           IF (BRIEFCASE-WEIGHT +
                 T-ITEMWEIGHT(ITEM-TO-PICKUP) > 10) THEN
                 DISPLAY "THE BRIEFCASE IS TOO FULL TO PICK THAT UP"
           ELSE
               PERFORM UNTIL B-ITEMNUM(BRIEFCASE-SLOT) = 00
                   ADD 1 TO BRIEFCASE-SLOT
               END-PERFORM
               MOVE ITEM-TO-PICKUP TO B-ITEMNUM(BRIEFCASE-SLOT)
               MOVE SPACES TO T-ROOMITEM(CURRENT-ROOM)
               MOVE 00 TO T-ROOMITEMNUM(CURRENT-ROOM)
               ADD T-ITEMWEIGHT(ITEM-TO-PICKUP) TO BRIEFCASE-WEIGHT
               DISPLAY "YOU PICK UP ", T-ITEMDESC(ITEM-TO-PICKUP)
           END-IF.
      ******************************************************************
      *    4300-PROCESS-DROP
      ******************************************************************
       4300-PROCESS-DROP.
           PERFORM UNTIL B-COUNT >= 11 OR DROPPED = 10
               MOVE B-ITEMNUM(B-COUNT) TO B-ITEM
               IF (COMMAND-PARM = T-ITEMDESC(B-ITEM)) THEN
                   PERFORM 4350-DROP-ITEM
                   MOVE 1 TO DROPPED
               ELSE
                   ADD 1 TO B-COUNT
               END-IF
           END-PERFORM
           IF (DROPPED = 0) THEN
               DISPLAY "YOU DON'T HAVE THAT ITEM"
           END-IF
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               PERFORM 4900-MONSTER-ATTACK
           END-IF.
      ******************************************************************
      *    4350-DROP-ITEM
      ******************************************************************
       4350-DROP-ITEM.
           IF (T-ROOMITEMNUM(CURRENT-ROOM) = 00) THEN
               MOVE 00 TO B-ITEMNUM(B-COUNT)
               MOVE T-ITEMDESC(B-ITEM) TO T-ROOMITEM(CURRENT-ROOM)
               MOVE B-ITEM TO T-ROOMITEMNUM(CURRENT-ROOM)
               SUBTRACT T-ITEMWEIGHT(B-ITEM) FROM BRIEFCASE-WEIGHT
               IF (BRIEFCASE-WEIGHT < 0) THEN
                   MOVE 00 TO BRIEFCASE-WEIGHT
               END-IF
               DISPLAY "YOU DROP ", T-ITEMDESC(B-ITEM)
           ELSE
               DISPLAY "THERE IS NOWHERE TO DROP THAT"
           END-IF.
      ******************************************************************
      *    4400-PROCESS-USE
      ******************************************************************
       4400-PROCESS-USE.
           PERFORM UNTIL B-COUNT >= 11 OR USED = 1
               MOVE B-ITEMNUM(B-COUNT) TO B-ITEM
               IF (COMMAND-PARM = T-ITEMDESC(B-ITEM)) THEN
                   DISPLAY "YOU USE ", T-ITEMDESC(B-ITEM)
                   SUBTRACT T-ITEMWEIGHT(B-ITEM) FROM BRIEFCASE-WEIGHT
                   IF (BRIEFCASE-WEIGHT < 0) THEN
                       MOVE 00 TO BRIEFCASE-WEIGHT
                   END-IF
                   PERFORM 4450-USE-ITEM
                   MOVE 1 TO USED
              ELSE
                  ADD 1 TO B-COUNT
              END-IF
           END-PERFORM
           IF (USED = 0)
               DISPLAY "YOU DON'T HAVE THAT ITEM"
           END-IF
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               PERFORM 4900-MONSTER-ATTACK
           END-IF.
      ******************************************************************
      *    4450-USE-ITEM
      ******************************************************************
       4450-USE-ITEM.
           MOVE 00 TO B-ITEMNUM(B-COUNT)
           IF (T-ITEMRESTORE(B-ITEM) NOT = 00) THEN
               ADD T-ITEMRESTORE(B-ITEM) TO WILL-TO-LIVE
               IF (WILL-TO-LIVE > 100) THEN
                   MOVE 100 TO WILL-TO-LIVE
               END-IF
               DISPLAY "YOU GAIN ", T-ITEMRESTORE(B-ITEM)
               DISPLAY "TO YOUR WILL TO LIVE"
           ELSE
               IF (T-ITEMDAMAGE(B-ITEM) NOT = 00) THEN
                   PERFORM 4455-ATTACK-ITEM
               ELSE
                   IF (T-ITEMWIN(B-ITEM) NOT = 00 AND CURRENT-ROOM = 01)
                       THEN
                       DISPLAY "CONGRATULATIONS YOU MADE IT!"
                       MOVE 000 TO WILL-TO-LIVE
                   END-IF
               END-IF
           END-IF.
      ******************************************************************
      *    4455-ATTACK-ITEM
      ******************************************************************
       4455-ATTACK-ITEM.
           MOVE T-ITEMDAMAGE(B-ITEM) TO PLAYER-ATTACK
           PERFORM 4700-PROCESS-ATTACK
           MOVE 05 TO PLAYER-ATTACK.
      ******************************************************************
      *    4500-PROCESS-LOOK
      ******************************************************************
       4500-PROCESS-LOOK.
           DISPLAY "YOU ARE IN ", T-ROOMDESC(CURRENT-ROOM)
           IF (T-ROOMITEMNUM(CURRENT-ROOM) NOT = 00) THEN
               MOVE T-ROOMITEMNUM(CURRENT-ROOM) TO CURRENT-ITEM
               DISPLAY "THERE IS A(N) ", T-ITEMDESC(CURRENT-ITEM)
           END-IF
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               MOVE T-ROOMMON(CURRENT-ROOM) TO CURRENT-MON
               DISPLAY "YOU SEE ", T-MONDESC(CURRENT-MON)
           END-IF
           IF (T-ROOMMON(CURRENT-ROOM) NOT = 00) THEN
               PERFORM 4900-MONSTER-ATTACK
           END-IF.
      ******************************************************************
      *    4600-PROCESS-BRIEFCASE
      ******************************************************************
       4600-PROCESS-BRIEFCASE.
           PERFORM UNTIL B-COUNT > 10
               MOVE B-ITEMNUM(B-COUNT) TO B-ITEM
               IF (B-ITEM NOT = 00) THEN
                   DISPLAY T-ITEMDESC(B-ITEM)
               ELSE
                   DISPLAY "-------------"
               END-IF
               ADD 1 TO B-COUNT
           END-PERFORM.
      ******************************************************************
      *    4700-PROCESS-ATTACK
      ******************************************************************
       4700-PROCESS-ATTACK.
           MOVE T-ROOMMON(CURRENT-ROOM) TO CURRENT-MON
           IF (CURRENT-MON NOT = 00)
               DISPLAY "YOU SLAP ", T-MONDESC(CURRENT-MON)
               PERFORM 4750-PLAYER-ATTACK
           ELSE
               IF(CURRENT-MON = 00) THEN
                   DISPLAY "YOU SLAP AT THE AIR!"
               END-IF
           END-IF.
      ******************************************************************
      *    4750-PLAYER-ATTACK
      ******************************************************************
       4750-PLAYER-ATTACK.
           MOVE T-MONHP(CURRENT-MON) TO MON-HP
           SUBTRACT PLAYER-ATTACK FROM MON-HP
           IF (MON-HP <= 0) THEN
               MOVE 00 TO T-ROOMMON(CURRENT-ROOM)
               DISPLAY T-MONDESC(CURRENT-MON), "RUNS AWAY CRYING"
           ELSE
               MOVE MON-HP TO T-MONHP(CURRENT-MON)
           END-IF.
      ******************************************************************
      *    4800-PROCESS-HELP
      ******************************************************************
       4800-PROCESS-HELP.
           DISPLAY "USE YOUR KEYS IN THE PARKING LOT TO WIN."
           DISPLAY "COMMANDS:"
           DISPLAY "GO 'DIRECTION': MOVE INDICATED DIRECTION"
           DISPLAY "PICKUP 'ITEM NAME': PICKUP INDICATED ITEM"
           DISPLAY "DROP 'ITEM NAME': DROP INDICATED ITEM"
           DISPLAY "USE 'ITEM NAME': USE INDICATED ITEM"
           DISPLAY "LOOK: LOOK AROUND THE AREA"
           DISPLAY "BRIEFCASE: CHECK HELD ITEMS"
           DISPLAY "ATTACK: BASIC ATTACK"
           DISPLAY "QUIT: QUIT THE GAME".
      ******************************************************************
      *    4900-MONSTER-ATTACK
      ******************************************************************
       4900-MONSTER-ATTACK.
           MOVE T-ROOMMON(CURRENT-ROOM) TO CURRENT-MON
           MOVE T-MONATTACK(CURRENT-MON) TO CURRENT-ATTACK
           COMPUTE WILL-TO-LIVE = WILL-TO-LIVE - CURRENT-ATTACK
           DISPLAY T-MONDESC(CURRENT-MON), "ATTACKS"
           DISPLAY T-MONTEXT(CURRENT-MON).
      ******************************************************************
      ******************************************************************
       END PROGRAM OVERTIME.
