       program-id. PROJ2023.
      ******************************************************************
      *    AUTHOR: CHRISTIAN VIKTOR A. NUQUI                           *
      *    STUDENT ID: 20201160                                        *
      *    SAMPLE COBOL MODULE FOR SOFT806 ASSIGNMENT                  *
      *    DATE: FEB-MAR 2023                                          *
      ******************************************************************

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  WS-PROGRAM-WORK-AREA.
           05  WS-INPUT-PARM.
               10  WS-INPUT-USER-ID              PIC X(08) VALUE SPACES.
               10  WS-INPUT-USER-PW              PIC X(10) VALUE SPACES.
               10  WS-INPUT-USER-FN              PIC X(25) VALUE SPACES.
               10  WS-INPUT-USER-LN              PIC X(25) VALUE SPACES.
           05  WS-STORED-PARM.
               10  WS-STORED-USER-ID             PIC X(08) VALUE SPACES.
               10  WS-STORED-USER-PW             PIC X(10) VALUE SPACES.
               10  WS-STORED-USER-FN             PIC X(25) VALUE SPACES.
               10  WS-STORED-USER-LN             PIC X(25) VALUE SPACES.
           05  WS-CONCAT-NM                      PIC X(50) VALUE SPACES.
           05  WS-SWITCHES.
               10  WS-MODULE-SW                  PIC X(01).
                   88  WS-MODULE-VALID           VALUE 'R' 'L' 'X'
                                                       'r' 'l' 'x'.
                   88  WS-MODULE-REG             VALUE 'R' 'r'.
                   88  WS-MODULE-LOGIN           VALUE 'L' 'l'.
                   88  WS-MODULE-EXIT            VALUE 'X' 'x'.
               10  WS-PROC-SW                    PIC X(01) VALUE SPACES.
                   88  WS-PROC-END               VALUE 'X'.

       linkage section.

       procedure division.
      ****************
       0000-MAINLINE. 
      ****************
           
           PERFORM  0100-INITIALIZE
               THRU 0100-INITIALIZE-X.

           PERFORM  0200-START-PROMPT
               THRU 0200-START-PROMPT-X.

           PERFORM  1000-PROCESS-TRANSACTIONS
               THRU 1000-PROCESS-TRANSACTIONS-X
              UNTIL WS-PROC-END.
                      
      *    goback.
      * STOP RUN WILL BE USED INSTEAD OF GOBACK.
           STOP RUN.

       0000-MAINLINE-X.
           EXIT.

      ******************
       0100-INITIALIZE.
      ******************
           
           MOVE SPACES TO WS-PROGRAM-WORK-AREA.

       0100-INITIALIZE-X.
           EXIT.

      ********************
       0200-START-PROMPT.
      ********************

           MOVE SPACES TO WS-SWITCHES
                          WS-INPUT-PARM.

           DISPLAY 'HELLO! DO YOU WANT TO:'.
           DISPLAY '[L]OGIN | [R]EGISTER | E[X]IT:'.
           ACCEPT WS-MODULE-SW.

       0200-START-PROMPT-X.
           EXIT.

      ****************************
       1000-PROCESS-TRANSACTIONS.
      ****************************

           IF  WS-PROC-END
               GO TO 1000-PROCESS-TRANSACTIONS-X
           END-IF.

      * CHECK IF ENTERED VALUES IS VALID
           IF NOT WS-MODULE-VALID
              DISPLAY 'INCORRECT INPUT. TRY AGAIN.'
              PERFORM  0200-START-PROMPT
                  THRU 0200-START-PROMPT-X
              GO TO 1000-PROCESS-TRANSACTIONS-X
           END-IF.

      * CHECK IF ENTERED VALUE IS LOGIN
           IF  WS-MODULE-LOGIN
               DISPLAY 'ENTER USER ID:'
               ACCEPT WS-INPUT-USER-ID
               IF  WS-INPUT-USER-ID NOT = WS-STORED-USER-ID
                   DISPLAY 'INCORRECT USER ID. RESETTING...'
                   PERFORM  0200-START-PROMPT
                       THRU 0200-START-PROMPT-X
                   GO TO 1000-PROCESS-TRANSACTIONS-X
               else 
                   DISPLAY 'ENTER PASSWORD:'
                   ACCEPT WS-INPUT-USER-PW
                   IF  WS-INPUT-USER-PW NOT = WS-STORED-USER-PW
                       DISPLAY 'INCORRECT PASSWORD. RESETTING...'
                       PERFORM  0200-START-PROMPT
                           THRU 0200-START-PROMPT-X
                       GO TO 1000-PROCESS-TRANSACTIONS-X
                   else 
                       PERFORM  2000-WELCOME-PAGE
                           THRU 2000-WELCOME-PAGE-X
                       GO TO 1000-PROCESS-TRANSACTIONS-X
                   END-IF
               END-IF
           END-IF.

      * CHECK IF ENTERED VALUE IS REGISTRATION
           IF WS-MODULE-REG
              DISPLAY 'ENTER USER NAME (UP TO 8 CHARACTERS ONLY PLS):'
              ACCEPT WS-STORED-USER-ID
              DISPLAY 'ENTER PASSWORD (UP TO 10 CHARACTERS ONLY PLS):'
              ACCEPT WS-STORED-USER-PW
              DISPLAY 'ENTER YOUR FIRST NAME:'
              ACCEPT WS-STORED-USER-FN
              DISPLAY 'ENTER YOUR LAST NAME:'
              ACCEPT WS-STORED-USER-LN

              IF  WS-STORED-USER-ID = spaces 
              OR  WS-STORED-USER-PW = spaces 
              OR  WS-STORED-USER-FN = spaces 
              OR  WS-STORED-USER-LN = spaces
                  DISPLAY 'NO FIELD SHOULD BE LEFT BLANK. RESETTING...'
              else
                  DISPLAY 'REGISTRATION SUCCESSFUL. RETURNING TO MAIN.' 
              END-IF
              PERFORM  0200-START-PROMPT
                  THRU 0200-START-PROMPT-X
              GO TO 1000-PROCESS-TRANSACTIONS-X
           END-IF.

      * CHECK IF ENTERED VALUE IS EXIT
           IF WS-MODULE-EXIT
              DISPLAY 'EXITING PROGRAM. HAVE A GOOD DAY!'
              SET WS-PROC-END TO true 
              GO TO 1000-PROCESS-TRANSACTIONS-X
           END-IF.

       1000-PROCESS-TRANSACTIONS-X.
           EXIT.

      ********************
       2000-WELCOME-PAGE.
      ********************

           STRING WS-STORED-USER-FN delimited by space
                  ' ' DELIMITED BY SIZE
                  WS-STORED-USER-LN delimited by space
             INTO WS-CONCAT-NM
           end-string.
           DISPLAY 'LOGIN ACCEPTED...'.
           DISPLAY 'WELCOME ' WS-CONCAT-NM.
           DISPLAY 'PRESS [X] TO EXIT:'.
           ACCEPT WS-MODULE-SW.

           PERFORM UNTIL WS-PROC-END
              IF  NOT WS-MODULE-EXIT
                  DISPLAY 'INCORRECT INPUT. PRESS [X] TO EXIT.'
                  ACCEPT WS-MODULE-SW
              else
                  SET WS-PROC-END TO TRUE
              end-if
           END-PERFORM.

       2000-WELCOME-PAGE-X.
           EXIT.
           