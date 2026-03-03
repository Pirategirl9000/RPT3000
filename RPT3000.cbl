       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT3000.                                             00030000
                                                                        00040000
      *   Programmers.: Violet French                                   00050000
      *   Date........: 2026.02.19                                      00060000
      *   Github URL..: https://github.com/Pirategirl9000/RPT3000       00070000
      *   Description.: This program produces a sales report based on   00080000
      *   values acquired from the CUSTMAST dataset                     00090000
       ENVIRONMENT DIVISION.                                            00100000
                                                                        00110000
       INPUT-OUTPUT SECTION.                                            00120000
                                                                        00130000
       FILE-CONTROL.                                                    00140000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00150000
           SELECT ORPT3000 ASSIGN TO RPT3000.                           00160000
                                                                        00170000
       DATA DIVISION.                                                   00180000
                                                                        00190000
       FILE SECTION.                                                    00200000
                                                                        00210000
      **************************************************************    00220000
      * INPUT FILE                                                 *    00230000
      **************************************************************    00240000
       FD  CUSTMAST                                                     00250000
           RECORDING MODE IS F                                          00260000
           LABEL RECORDS ARE STANDARD                                   00270000
           RECORD CONTAINS 130 CHARACTERS                               00280000
           BLOCK CONTAINS 130 CHARACTERS.                               00290000
       01  CUSTOMER-MASTER-RECORD.                                      00300000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00310000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00320000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00330000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00340000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00350000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00360000
           05  FILLER                  PIC X(87).                       00370000
                                                                        00380000
      **************************************************************    00390000
      * OUTPUT FILE                                                *    00400000
      **************************************************************    00410000
       FD  ORPT3000                                                     00420000
           RECORDING MODE IS F                                          00430000
           LABEL RECORDS ARE STANDARD                                   00440000
           RECORD CONTAINS 130 CHARACTERS                               00450000
           BLOCK CONTAINS 130 CHARACTERS.                               00460000
       01  PRINT-AREA      PIC X(130).                                  00470000
                                                                        00480000
       WORKING-STORAGE SECTION.                                         00490000
                                                                        00500000
      *------------------------------------------------------------*    00510000
      *                        WORKING FIELDS                      *    00520000
      *============================================================*    00530000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00540000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00550000
      *------------------------------------------------------------*    00560000
                                                                        00570000
      **************************************************************    00580000
      * SWITCH FOR END OF FILE                                     *    00590000
      **************************************************************    00600000
       01  SWITCHES.                                                    00610000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00620000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00630001
                                                                        00631001
      **************************************************************    00632001
      * SWITCH FOR END OF FILE                                     *    00633001
      **************************************************************    00634001
       01  CONTROL-FIELDS.                                              00635001
           05  OLD-BRANCH-NUMBER       PIC 99.                          00636001
                                                                        00638001
      **************************************************************    00640000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00650000
      **************************************************************    00660000
       01  PRINT-FIELDS.                                                00670000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00680000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00690000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00700000
                                                                        00710000
      **************************************************************    00720000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00730000
      **************************************************************    00740000
       01  TOTAL-FIELDS.                                                00750000
           05  BRANCH-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.        00751001
           05  BRANCH-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.        00752001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00760000
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00770000
                                                                        00780000
      **************************************************************    00790000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00800000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00810000
      **************************************************************    00820000
       01  CURRENT-DATE-AND-TIME.                                       00830000
           05  CD-YEAR         PIC 9999.                                00840000
           05  CD-MONTH        PIC 99.                                  00850000
           05  CD-DAY          PIC 99.                                  00860000
           05  CD-HOURS        PIC 99.                                  00870000
           05  CD-MINUTES      PIC 99.                                  00880000
           05  FILLER          PIC X(9).                                00890000
                                                                        00900000
      **************************************************************    00910000
      * STORES FIELDS WITH VALUES CALCULATED PER CUSTOMER         *     00920000
      **************************************************************    00930000
       01  CALCULATED-FIELDS.                                           00940000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            00950000
                                                                        00960000
      *------------------------------------------------------------*    00970000
      *                       OUTPUT FIELDS                        *    00980000
      *============================================================*    00990000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01000000
      *                      THE OUTPUT FILE                       *    01010000
      *------------------------------------------------------------*    01020000
                                                                        01030000
      **************************************************************    01040000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01050000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01060000
      **************************************************************    01070000
       01  HEADING-LINE-1.                                              01080000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01090000
           05  HL1-MONTH       PIC 9(2).                                01100000
           05  FILLER          PIC X(1)    VALUE "/".                   01110000
           05  HL1-DAY         PIC 9(2).                                01120000
           05  FILLER          PIC X(1)    VALUE "/".                   01130000
           05  HL1-YEAR        PIC 9(4).                                01140000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01150000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01160000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01170000
           05  FILLER          PIC X(15)   VALUE SPACE.                 01180000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01190000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01200000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01210000
                                                                        01220000
      **************************************************************    01230000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01240000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01250000
      **************************************************************    01260000
       01  HEADING-LINE-2.                                              01270000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01280000
           05  HL2-HOURS       PIC 9(2).                                01290000
           05  FILLER          PIC X(1)    VALUE ":".                   01300000
           05  HL2-MINUTES     PIC 9(2).                                01310000
           05  FILLER          PIC X(68)   VALUE SPACE.                 01320000
           05  FILLER          PIC X(10)   VALUE "RPT3000".             01330000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01340000
                                                                        01350000
      **************************************************************    01360000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01370000
      **************************************************************    01380000
       01  HEADING-LINE-3.                                              01390000
           05 FILLER               PIC X(130)   VALUE SPACE.            01400000
                                                                        01410000
      **************************************************************    01420000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01430000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01440000
      * THE NEXT HEADER LINE                                       *    01450000
      **************************************************************    01460000
       01  HEADING-LINE-4.                                              01470000
           05  FILLER      PIC X(8)    VALUE "BRANCH  ".                01480001
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01490001
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01500000
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01510000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01520000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01530000
           05  FILLER      PIC X(36)   VALUE SPACE.                     01540001
                                                                        01550000
      **************************************************************    01560000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01570000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01580000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01590000
      **************************************************************    01600000
       01  HEADING-LINE-5.                                              01610000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01620000
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01630000
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01640000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01650000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01660000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01670000
           05  FILLER      PIC X(37)   VALUE SPACE.                     01680000
                                                                        01690000
      **************************************************************    01700000
      * STORES THE SIXTH HEADER LINE INFORMATION                   *    01710000
      * DISPLAYS COLUMN DIVIDERS FOR THE REPORT                    *    01720000
      **************************************************************    01730000
       01  HEADING-LINE-6.                                              01740000
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> BRANCH NUM      01750000
           05  FILLER      PIC X      VALUE SPACE.                      01760000
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> SALES REP       01770000
           05  FILLER      PIC X      VALUE SPACE.                      01780000
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> CUST NUM        01790000
           05  FILLER      PIC X(2)   VALUE SPACE.                      01800000
           05  FILLER      PIC X(20)  VALUE ALL '-'. *> CUST NAME       01810000
           05  FILLER      PIC X(3)   VALUE SPACE.                      01820000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES THIS      01830000
           05  FILLER      PIC X(4)   VALUE SPACE.                      01840000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES LAST      01850000
           05  FILLER      PIC X(4)   VALUE SPACE.                      01860000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> CHANGE AMNT     01870000
           05  FILLER      PIC X(3)   VALUE SPACE.                      01880000
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> CHANGE PERC     01890000
           05  FILLER      PIC X(40)  VALUE SPACE.                      01900000
                                                                        01910000
      **************************************************************    01920000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01930000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    01940000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    01950000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    01960000
      * DIFFERENCE IN PERCENT.                                     *    01970000
      **************************************************************    01980000
       01  CUSTOMER-LINE.                                               01990000
           05  FILLER              PIC X(2)     VALUE SPACE.            02000000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02010000
           05  FILLER              PIC X(4)     VALUE SPACE.            02020000
           05  CL-SALESREP-NUMBER  PIC X(2).                            02030000
           05  FILLER              PIC X(3)     VALUE SPACE.            02040000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02050000
           05  FILLER              PIC X(2)     VALUE SPACE.            02060000
           05  CL-CUSTOMER-NAME    PIC X(20).                           02070000
           05  FILLER              PIC X(3)     VALUE SPACE.            02080000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02090000
           05  FILLER              PIC X(4)     VALUE SPACE.            02100000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02110000
           05  FILLER              PIC X(4)     VALUE SPACE.            02120000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02130000
           05  FILLER              PIC X(3)     VALUE SPACE.            02140000
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          02150000
           05  FILLER              PIC X(40)    VALUE SPACE.            02160000
                                                                        02170000
      **************************************************************    02180000
      * STORES THE FIRST GRAND TOTAL LINE                          *    02190000
      * DISPLAYS COLUMN DIVIDERS FOR THE GRAND TOTALS              *    02200000
      **************************************************************    02210000
       01  GRAND-TOTAL-LINE1.                                           02220000
           05  FILLER              PIC X(40)    VALUE SPACE.            02230000
           05  FILLER              PIC X(13)    VALUE ALL '='.          02240000
           05  FILLER              PIC X        VALUE SPACE.            02250000
           05  FILLER              PIC X(13)    VALUE ALL '='.          02260000
           05  FILLER              PIC X        VALUE SPACE.            02270000
           05  FILLER              PIC X(13)    VALUE ALL '='.          02280000
           05  FILLER              PIC X(3)     VALUE SPACES.           02290000
           05  FILLER              PIC X(6)     VALUE ALL '='.          02300000
           05  FILLER              PIC X(40)    VALUE SPACES.           02310000
                                                                        02310101
      **************************************************************    02311001
      * STORES THE BRANCH TOTAL LINE                               *    02312002
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02313002
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02314002
      * USED FOR OUTPUTTING                                        *    02315002
      **************************************************************    02316001
       01  BRANCH-TOTAL-LINE.                                           02317001
           05  FILLER              PIC X(23)    VALUE SPACE.            02318001
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02318101
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02319001
           05  FILLER              PIC X(3)     VALUE SPACE.            02319101
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02319201
           05  FILLER              PIC X(3)     VALUE SPACE.            02319301
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02319401
           05  FILLER              PIC X(3)     VALUE SPACE.            02319501
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02319601
           05  FILLER              PIC X(47)    VALUE SPACE.            02319701
                                                                        02319803
      **************************************************************    02319903
      * STORES THE FIRST GRAND TOTAL LINE                          *    02320003
      * DISPLAYS COLUMN DIVIDERS FOR THE GRAND TOTALS              *    02320103
      **************************************************************    02320203
       01  GRAND-TOTAL-LINE1.                                           02320303
           05  FILLER              PIC X(40)    VALUE SPACE.            02320403
           05  FILLER              PIC X(13)    VALUE ALL '='.          02320503
           05  FILLER              PIC X        VALUE SPACE.            02320603
           05  FILLER              PIC X(13)    VALUE ALL '='.          02320703
           05  FILLER              PIC X        VALUE SPACE.            02320803
           05  FILLER              PIC X(13)    VALUE ALL '='.          02320903
           05  FILLER              PIC X(3)     VALUE SPACES.           02321003
           05  FILLER              PIC X(6)     VALUE ALL '='.          02321103
           05  FILLER              PIC X(40)    VALUE SPACES.           02321203
                                                                        02321303
      **************************************************************    02322000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02330000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02340000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02350000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02360000
      **************************************************************    02370000
       01  GRAND-TOTAL-LINE2.                                           02380000
           05  FILLER              PIC X(23)    VALUE SPACE.            02390001
           05  FILLER              PIC X(14)    VALUE "GRAND TOTAL".    02391001
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02400000
           05  FILLER              PIC X(1)     VALUE SPACE.            02410000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02420000
           05  FILLER              PIC X        VALUE SPACE.            02430000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02440000
           05  FILLER              PIC X(3)     VALUE SPACE.            02450000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02460000
           05  FILLER              PIC X(43)    VALUE SPACE.            02470001
                                                                        02480000
       PROCEDURE DIVISION.                                              02490000
                                                                        02500000
      **************************************************************    02510000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02520000
      * READING AND WRITING TO AND FROM THEM                       *    02530000
      **************************************************************    02540000
       000-PREPARE-SALES-REPORT.                                        02550000
                                                                        02560000
           OPEN INPUT  CUSTMAST                                         02570000
                OUTPUT ORPT3000.                                        02580000
                                                                        02590000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02600000
           *> THE HEADER LINES                                          02610000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02620000
                                                                        02630000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02640000
           *> THE END OF THE INPUT FILE                                 02650000
           PERFORM 200-PREPARE-SALES-LINES                              02660000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02670000
                                                                        02680000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02690000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02700000
                                                                        02710000
           CLOSE CUSTMAST                                               02720000
                 ORPT3000.                                              02730000
           STOP RUN.                                                    02740000
                                                                        02750000
      **************************************************************    02760000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    02770000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    02780000
      **************************************************************    02790000
       100-FORMAT-REPORT-HEADING.                                       02800000
                                                                        02810000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         02820000
                                                                        02830000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          02840000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         02850000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           02860000
           MOVE CD-MONTH   TO HL1-MONTH.                                02870000
           MOVE CD-DAY     TO HL1-DAY.                                  02880000
           MOVE CD-YEAR    TO HL1-YEAR.                                 02890000
           MOVE CD-HOURS   TO HL2-HOURS.                                02900000
           MOVE CD-MINUTES TO HL2-MINUTES.                              02910000
                                                                        02920000
      **************************************************************    02930000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    02940000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    02950000
      * TERMINATING LINE OF THE FILE                               *    02960000
      **************************************************************    02970000
       200-PREPARE-SALES-LINES.                                         02980000
                                                                        02990000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03000000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03010000
                                                                        03020000
           *> IF THE LINE WE READ WASN'T BLANK THEN                     03030000
           *> WE WILL OUTPUT THAT CUSTOMER'S SALES TO THE OUTPUT        03040000
           *> NOTE: WE DON'T OUTPUT THE LAST LINE BECAUSE IT'S BLANK    03050000
           IF CUSTMAST-EOF-SWITCH = "N"                                 03060000
               IF FIRST-RECORD-SWITCH = "Y"                             03070004
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03080004
                   MOVE "N" TO FIRST-RECORD-SWITCH                      03081004
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03082004
               ELSE                                                     03083004
                   IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER              03084004
                       PERFORM 240-PRINT-BRANCH-LINE                    03085004
                       PERFORM 220-PRINT-CUSTOMER-LINE                  03086004
                       MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER       03087004
                   ELSE                                                 03088004
                       PERFORM 220-PRINT-CUSTOMER-LINE.                 03089004
               ELSE                                                     03089104
                   PERFORM 360-PRINT-BRANCH-LINE.                       03089204
                                                                        03089304
      **************************************************************    03090000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03100000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03110000
      **************************************************************    03120000
       210-READ-CUSTOMER-RECORD.                                        03130000
                                                                        03140000
           READ CUSTMAST                                                03150000
               AT END                                                   03160000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03170000
                                                                        03180000
      **************************************************************    03190000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03200000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03210000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03220000
      **************************************************************    03230000
       220-PRINT-CUSTOMER-LINE.                                         03240000
                                                                        03250000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03260000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03270000
           IF LINE-COUNT >= LINES-ON-PAGE                               03280000
               PERFORM 230-PRINT-HEADING-LINES.                         03290000
                                                                        03300000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         03310000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     03320000
           MOVE CM-BRANCH-NUMBER    TO CL-BRANCH-NUMBER.                03330000
           MOVE CM-SALESREP-NUMBER  TO CL-SALESREP-NUMBER.              03340000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              03350000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                03360000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               03370000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               03380000
                                                                        03390000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    03400000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      03410000
           COMPUTE CHANGE-AMOUNT =                                      03420000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   03430000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      03440000
                                                                        03450000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     03460000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        03470000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        03480000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          03490000
           IF CM-SALES-LAST-YTD = ZERO                                  03500000
               MOVE 999.9 TO CL-CHANGE-PERCENT                          03510000
           ELSE                                                         03520000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      03530000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              03540000
                   ON SIZE ERROR                                        03550000
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 03560000
                                                                        03570000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       03580000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            03590000
           WRITE PRINT-AREA.                                            03600000
           ADD 1 TO LINE-COUNT.                                         03610000
                                                                        03620000
           *> ADD THIS CUSTOMERS SALES TO THE GRAND TOTALS              03630000
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.               03640000
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.               03650000
                                                                        03660000
      **************************************************************    03670000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    03680000
      * FOR EVERY PAGE                                             *    03690000
      **************************************************************    03700000
       230-PRINT-HEADING-LINES.                                         03710000
                                                                        03720000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             03730000
           *> SO WE INCREASE THE PAGE COUNT HERE                        03740000
           ADD 1 TO PAGE-COUNT.                                         03750000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      03760000
                                                                        03770000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 03780000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           03790000
           WRITE PRINT-AREA.                                            03800000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           03810000
           WRITE PRINT-AREA.                                            03820000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           03830000
           WRITE PRINT-AREA.                                            03840000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           03850000
           WRITE PRINT-AREA.                                            03860000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           03870000
           WRITE PRINT-AREA.                                            03880000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           03890000
           WRITE PRINT-AREA.                                            03900000
                                                                        03910000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    03920000
           *> OF A NEW PAGE                                             03930000
           MOVE ZERO TO LINE-COUNT.                                     03940000
                                                                        03950000
      **************************************************************    03951004
      *                                                            *    03952004
      *                                                            *    03953004
      *                                                            *    03954004
      **************************************************************    03955004
       240-PRINT-BRANCH-LINE.                                           03956004
                                                                        03957004
                                                                        03958004
                                                                        03959004
      **************************************************************    03960000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    03970000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    03980000
      * BEEN PRINTED                                               *    03990000
      **************************************************************    04000000
       300-PRINT-GRAND-TOTALS.                                          04010000
                                                                        04020000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                04030000
           *> OUTPUT LINE FOR GRAND TOTALS                              04040000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             04050000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             04060000
                                                                        04070000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             04080000
           COMPUTE CHANGE-AMOUNT =                                      04090000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             04100000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     04110000
                                                                        04120000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             04130000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   04140000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           04150000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          04160000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               04170000
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         04180000
           ELSE                                                         04190000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     04200000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           04210000
                   ON SIZE ERROR                                        04220000
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                04230000
                                                                        04240000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  04250000
           MOVE GRAND-TOTAL-LINE1    TO PRINT-AREA.                     04260000
           WRITE PRINT-AREA.                                            04270000
           MOVE GRAND-TOTAL-LINE2    TO PRINT-AREA.                     04280000
           WRITE PRINT-AREA.                                            04290000
