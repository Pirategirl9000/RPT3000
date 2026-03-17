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
                                                                        00640001
      **************************************************************    00650001
      * SWITCH FOR END OF FILE                                     *    00660001
      **************************************************************    00670001
       01  CONTROL-FIELDS.                                              00680001
           05  OLD-BRANCH-NUMBER       PIC 99.                          00690001
                                                                        00700001
      **************************************************************    00710000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00720000
      **************************************************************    00730000
       01  PRINT-FIELDS.                                                00740000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00750000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00760000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00770000
                                                                        00780000
      **************************************************************    00790000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00800000
      **************************************************************    00810000
       01  TOTAL-FIELDS.                                                00820000
           05  BRANCH-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.        00830001
           05  BRANCH-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.        00840001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00850000
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00860000
                                                                        00870000
      **************************************************************    00880000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00890000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00900000
      **************************************************************    00910000
       01  CURRENT-DATE-AND-TIME.                                       00920000
           05  CD-YEAR         PIC 9999.                                00930000
           05  CD-MONTH        PIC 99.                                  00940000
           05  CD-DAY          PIC 99.                                  00950000
           05  CD-HOURS        PIC 99.                                  00960000
           05  CD-MINUTES      PIC 99.                                  00970000
           05  FILLER          PIC X(9).                                00980000
                                                                        00990000
      **************************************************************    01000000
      * STORES FIELDS WITH VALUES CALCULATED PER CUSTOMER         *     01010000
      **************************************************************    01020000
       01  CALCULATED-FIELDS.                                           01030000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01040000
                                                                        01050000
      *------------------------------------------------------------*    01060000
      *                       OUTPUT FIELDS                        *    01070000
      *============================================================*    01080000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01090000
      *                      THE OUTPUT FILE                       *    01100000
      *------------------------------------------------------------*    01110000
                                                                        01120000
      **************************************************************    01130000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01140000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01150000
      **************************************************************    01160000
       01  HEADING-LINE-1.                                              01170000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01180000
           05  HL1-MONTH       PIC 9(2).                                01190000
           05  FILLER          PIC X(1)    VALUE "/".                   01200000
           05  HL1-DAY         PIC 9(2).                                01210000
           05  FILLER          PIC X(1)    VALUE "/".                   01220000
           05  HL1-YEAR        PIC 9(4).                                01230000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01240000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01250000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01260000
           05  FILLER          PIC X(15)   VALUE SPACE.                 01270000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01280000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01290000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01300000
                                                                        01310000
      **************************************************************    01320000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01330000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01340000
      **************************************************************    01350000
       01  HEADING-LINE-2.                                              01360000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01370000
           05  HL2-HOURS       PIC 9(2).                                01380000
           05  FILLER          PIC X(1)    VALUE ":".                   01390000
           05  HL2-MINUTES     PIC 9(2).                                01400000
           05  FILLER          PIC X(68)   VALUE SPACE.                 01410000
           05  FILLER          PIC X(10)   VALUE "RPT3000".             01420000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01430000
                                                                        01440000
      **************************************************************    01450000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01460000
      **************************************************************    01470000
       01  HEADING-LINE-3.                                              01480000
           05 FILLER               PIC X(130)   VALUE SPACE.            01490000
                                                                        01500000
      **************************************************************    01510000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01520000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01530000
      * THE NEXT HEADER LINE                                       *    01540000
      **************************************************************    01550000
       01  HEADING-LINE-4.                                              01560000
           05  FILLER      PIC X(8)    VALUE "BRANCH  ".                01570001
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01580001
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01590000
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01600000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01610000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01620000
           05  FILLER      PIC X(36)   VALUE SPACE.                     01630001
                                                                        01640000
      **************************************************************    01650000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01660000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01670000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01680000
      **************************************************************    01690000
       01  HEADING-LINE-5.                                              01700000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01710000
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01720000
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01730000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01740000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01750000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01760000
           05  FILLER      PIC X(37)   VALUE SPACE.                     01770000
                                                                        01780000
      **************************************************************    01790000
      * STORES THE SIXTH HEADER LINE INFORMATION                   *    01800000
      * DISPLAYS COLUMN DIVIDERS FOR THE REPORT                    *    01810000
      **************************************************************    01820000
       01  HEADING-LINE-6.                                              01830000
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> BRANCH NUM      01840000
           05  FILLER      PIC X      VALUE SPACE.                      01850000
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> SALES REP       01860000
           05  FILLER      PIC X      VALUE SPACE.                      01870000
           05  FILLER      PIC X(5)   VALUE ALL '-'. *> CUST NUM        01880000
           05  FILLER      PIC X(2)   VALUE SPACE.                      01890000
           05  FILLER      PIC X(20)  VALUE ALL '-'. *> CUST NAME       01900000
           05  FILLER      PIC X(3)   VALUE SPACE.                      01910000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES THIS      01920000
           05  FILLER      PIC X(4)   VALUE SPACE.                      01930000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> SALES LAST      01940000
           05  FILLER      PIC X(4)   VALUE SPACE.                      01950000
           05  FILLER      PIC X(10)  VALUE ALL '-'. *> CHANGE AMNT     01960000
           05  FILLER      PIC X(3)   VALUE SPACE.                      01970000
           05  FILLER      PIC X(6)   VALUE ALL '-'. *> CHANGE PERC     01980000
           05  FILLER      PIC X(40)  VALUE SPACE.                      01990000
                                                                        02000000
      **************************************************************    02010000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    02020000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02030000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02040000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02050000
      * DIFFERENCE IN PERCENT.                                     *    02060000
      **************************************************************    02070000
       01  CUSTOMER-LINE.                                               02080000
           05  FILLER              PIC X(2)     VALUE SPACE.            02090000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02100000
           05  FILLER              PIC X(4)     VALUE SPACE.            02110000
           05  CL-SALESREP-NUMBER  PIC X(2).                            02120000
           05  FILLER              PIC X(3)     VALUE SPACE.            02130000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02140000
           05  FILLER              PIC X(2)     VALUE SPACE.            02150000
           05  CL-CUSTOMER-NAME    PIC X(20).                           02160000
           05  FILLER              PIC X(3)     VALUE SPACE.            02170000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02180000
           05  FILLER              PIC X(4)     VALUE SPACE.            02190000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02200000
           05  FILLER              PIC X(4)     VALUE SPACE.            02210000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02220000
           05  FILLER              PIC X(3)     VALUE SPACE.            02230000
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          02240000
           05  FILLER              PIC X(40)    VALUE SPACE.            02250000
                                                                        02260001
      **************************************************************    02270001
      * STORES THE BRANCH TOTAL LINE                               *    02280002
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02290002
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02300002
      * USED FOR OUTPUTTING                                        *    02310002
      **************************************************************    02320001
       01  BRANCH-TOTAL-LINE.                                           02330001
           05  FILLER              PIC X(23)    VALUE SPACE.            02340001
           05  FILLER              PIC X(14)    VALUE "BRANCH TOTAL".   02350001
           05  BTL-SALES-THIS-YTD  PIC ZZZ,ZZ9.99-.                     02360001
           05  FILLER              PIC X(3)     VALUE SPACE.            02370001
           05  BTL-SALES-LAST-YTD  PIC ZZZ,ZZ9.99-.                     02380001
           05  FILLER              PIC X(3)     VALUE SPACE.            02390001
           05  BTL-CHANGE-AMOUNT   PIC ZZZ,ZZ9.99-.                     02400001
           05  FILLER              PIC X(3)     VALUE SPACE.            02410001
           05  BTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02420007
           05  FILLER              PIC X(47)    VALUE SPACE.            02430001
                                                                        02440003
      **************************************************************    02450003
      * STORES THE FIRST GRAND TOTAL LINE                          *    02460003
      * DISPLAYS COLUMN DIVIDERS FOR THE GRAND TOTALS              *    02470003
      **************************************************************    02480003
       01  GRAND-TOTAL-LINE1.                                           02490003
           05  FILLER              PIC X(40)    VALUE SPACE.            02500003
           05  FILLER              PIC X(13)    VALUE ALL '='.          02510003
           05  FILLER              PIC X        VALUE SPACE.            02520003
           05  FILLER              PIC X(13)    VALUE ALL '='.          02530003
           05  FILLER              PIC X        VALUE SPACE.            02540003
           05  FILLER              PIC X(13)    VALUE ALL '='.          02550003
           05  FILLER              PIC X(3)     VALUE SPACES.           02560003
           05  FILLER              PIC X(6)     VALUE ALL '='.          02570003
           05  FILLER              PIC X(40)    VALUE SPACES.           02580003
                                                                        02590003
      **************************************************************    02600000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02610000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02620000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02630000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02640000
      **************************************************************    02650000
       01  GRAND-TOTAL-LINE2.                                           02660000
           05  FILLER              PIC X(23)    VALUE SPACE.            02670001
           05  FILLER              PIC X(14)    VALUE "GRAND TOTAL".    02680001
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   02690000
           05  FILLER              PIC X(1)     VALUE SPACE.            02700000
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   02710000
           05  FILLER              PIC X        VALUE SPACE.            02720000
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   02730000
           05  FILLER              PIC X(3)     VALUE SPACE.            02740000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02750000
           05  FILLER              PIC X(43)    VALUE SPACE.            02760001
                                                                        02770000
       PROCEDURE DIVISION.                                              02780000
                                                                        02790000
      **************************************************************    02800000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02810000
      * READING AND WRITING TO AND FROM THEM                       *    02820000
      **************************************************************    02830000
       000-PREPARE-SALES-REPORT.                                        02840000
                                                                        02850000
           OPEN INPUT  CUSTMAST                                         02860000
                OUTPUT ORPT3000.                                        02870000
                                                                        02880000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02890000
           *> THE HEADER LINES                                          02900000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02910000
                                                                        02920000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02930000
           *> THE END OF THE INPUT FILE                                 02940000
           PERFORM 200-PREPARE-SALES-LINES                              02950000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02960000
                                                                        02970000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02980000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02990000
                                                                        03000000
           CLOSE CUSTMAST                                               03010000
                 ORPT3000.                                              03020000
           STOP RUN.                                                    03030000
                                                                        03040000
      **************************************************************    03050000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03060000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03070000
      **************************************************************    03080000
       100-FORMAT-REPORT-HEADING.                                       03090000
                                                                        03100000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03110000
                                                                        03120000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03130000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03140000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03150000
           MOVE CD-MONTH   TO HL1-MONTH.                                03160000
           MOVE CD-DAY     TO HL1-DAY.                                  03170000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03180000
           MOVE CD-HOURS   TO HL2-HOURS.                                03190000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03200000
                                                                        03210000
      **************************************************************    03220000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03230000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03240000
      * TERMINATING LINE OF THE FILE                               *    03250000
      **************************************************************    03260000
       200-PREPARE-SALES-LINES.                                         03270000
                                                                        03280000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03290000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03300000
                                                                        03310000
           *> IF THE LINE WE READ WASN'T BLANK THEN                     03320000
           *> WE WILL OUTPUT THAT CUSTOMER'S SALES TO THE OUTPUT        03330000
           *> NOTE: WE DON'T OUTPUT THE LAST LINE BECAUSE IT'S BLANK    03340000
           IF CUSTMAST-EOF-SWITCH = "N"                                 03350000
               IF FIRST-RECORD-SWITCH = "Y"                             03360004
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03370004
                   MOVE "N" TO FIRST-RECORD-SWITCH                      03380004
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03390004
               ELSE                                                     03400004
                   IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER              03410004
                       PERFORM 240-PRINT-BRANCH-LINE                    03420004
                       PERFORM 220-PRINT-CUSTOMER-LINE                  03430004
                       MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER       03440004
                   ELSE                                                 03450004
                       PERFORM 220-PRINT-CUSTOMER-LINE                  03460009
           ELSE                                                         03470007
               PERFORM 240-PRINT-BRANCH-LINE.                           03480007
                                                                        03490004
      **************************************************************    03500000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03510000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03520000
      **************************************************************    03530000
       210-READ-CUSTOMER-RECORD.                                        03540000
                                                                        03550000
           READ CUSTMAST                                                03560000
               AT END                                                   03570000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03580000
                                                                        03590000
      **************************************************************    03600000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03610000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03620000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03630000
      **************************************************************    03640000
       220-PRINT-CUSTOMER-LINE.                                         03650000
                                                                        03660000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03670000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03680000
           IF LINE-COUNT >= LINES-ON-PAGE                               03690000
               PERFORM 230-PRINT-HEADING-LINES.                         03700000
                                                                        03710000
           *> IF THIS IS THE FIRST RECORD OR THE FIRST RECORD OF THIS   03720006
           *> BRANCH THEN WE MOVE THE BRANCH NUMBER TO BE PRINTED       03730006
           *> OTHERWISE WE MOVE SPACES TO THE BRANCH NUMBER ITEM        03740006
           IF FIRST-RECORD-SWITCH = "Y"                                 03750005
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER                03760005
           ELSE                                                         03770005
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                  03780005
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03790005
               ELSE                                                     03800005
                   MOVE SPACES TO CL-BRANCH-NUMBER.                     03810005
                                                                        03820005
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         03830000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     03840000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              03850000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                03860000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               03870000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               03880000
                                                                        03890000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    03900000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      03910000
           COMPUTE CHANGE-AMOUNT =                                      03920000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   03930000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      03940000
                                                                        03950000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     03960000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        03970000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        03980000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          03990000
           IF CM-SALES-LAST-YTD = ZERO                                  04000000
               MOVE 999.9 TO CL-CHANGE-PERCENT                          04010000
           ELSE                                                         04020000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04030000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04040000
                   ON SIZE ERROR                                        04050000
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 04060000
                                                                        04070000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04080000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04090000
           PERFORM 225-WRITE-REPORT-LINE.                               04100008
                                                                        04110005
           *> ADD THIS CUSTOMERS SALES TO THE BRANCH TOTALS             04120005
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.              04130005
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-THIS-YTD,              04140005
                                                                        04150000
           *> ADD THIS CUSTOMERS SALES TO THE GRAND TOTALS              04160000
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.               04170000
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.               04180000
                                                                        04190000
      **************************************************************    04200008
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04210008
      * FOR EVERY PAGE                                             *    04220008
      **************************************************************    04230008
       225-WRITE-REPORT-LINE.                                           04240009
           WRITE PRINT-AREA.                                            04250009
           ADD 1 TO LINE-COUNT.                                         04260009
                                                                        04270008
      **************************************************************    04280000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04290000
      * FOR EVERY PAGE                                             *    04300000
      **************************************************************    04310000
       230-PRINT-HEADING-LINES.                                         04320000
                                                                        04330000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04340000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04350000
           ADD 1 TO PAGE-COUNT.                                         04360000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04370000
                                                                        04380000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04390000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04400000
           WRITE PRINT-AREA.                                            04410000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04420000
           WRITE PRINT-AREA.                                            04430000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04440000
           WRITE PRINT-AREA.                                            04450000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04460000
           WRITE PRINT-AREA.                                            04470000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04480000
           WRITE PRINT-AREA.                                            04490000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04500000
           WRITE PRINT-AREA.                                            04510000
                                                                        04520000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04530000
           *> OF A NEW PAGE                                             04540000
           MOVE ZERO TO LINE-COUNT.                                     04550000
                                                                        04560000
      **************************************************************    04570004
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04580008
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04590008
      **************************************************************    04600004
       240-PRINT-BRANCH-LINE.                                           04610004
                                                                        04620008
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04630008
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04640008
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04650008
                                                                        04660004
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04670008
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       04680008
           COMPUTE CHANGE-AMOUNT =                                      04690008
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04700008
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04710008
                                                                        04720008
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                04730008
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        04740008
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04750008
               MOVE 999.9 TO BTL-CHANGE-AMOUNT                          04760008
           ELSE                                                         04770008
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04780008
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04790008
                   ON SIZE ERROR                                        04800008
                       MOVE 999.9 TO BTL-CHANGE-PERCENT                 04810009
                                                                        04820008
           *> PRINT BRANCH LINE                                         04830008
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        04840008
           PERFORM 225-WRITE-REPORT-LINE.                               04850008
                                                                        04860008
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 04870008
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           04880008
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           04890008
                                                                        04900008
           *> ZERO OUT THE BRANCH TOTALS                                04910008
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          04920008
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          04930008
                                                                        04940008
      **************************************************************    04950000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    04960000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    04970000
      * BEEN PRINTED                                               *    04980000
      **************************************************************    04990000
       300-PRINT-GRAND-TOTALS.                                          05000000
                                                                        05010000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                05020000
           *> OUTPUT LINE FOR GRAND TOTALS                              05030000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             05040000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             05050000
                                                                        05060000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             05070000
           COMPUTE CHANGE-AMOUNT =                                      05080000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             05090000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     05100000
                                                                        05110000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             05120000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   05130000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           05140000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          05150000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               05160000
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         05170000
           ELSE                                                         05180000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     05190000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           05200000
                   ON SIZE ERROR                                        05210000
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                05220000
                                                                        05230000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  05240000
           MOVE GRAND-TOTAL-LINE1    TO PRINT-AREA.                     05250000
           PERFORM 225-WRITE-REPORT-LINE.                               05260008
           MOVE GRAND-TOTAL-LINE2    TO PRINT-AREA.                     05270000
           PERFORM 225-WRITE-REPORT-LINE.                               05280008
