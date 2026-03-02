=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
000002        IDENTIFICATION DIVISION.
000003        PROGRAM-ID. NYFPOWFF.
000004        AUTHOR. BHARATH CHEVIREDDY.
000005        DATE-WRITTEN. 03/2026.
000006        ENVIRONMENT DIVISION.
000007        CONFIGURATION SECTION.
000008        INPUT-OUTPUT SECTION.
000009        FILE-CONTROL.
000010            SELECT REPORT-FILE ASSIGN TO RPTOUT.
000011        DATA DIVISION.
000012        FILE SECTION.
000013        FD  REPORT-FILE
000014        RECORDING MODE IS F
000015        BLOCK CONTAINS 0 RECORDS.
000016        01  REPORT-REC        PIC X(400).
000017********************************************************************
000018*                                                                  *
000019*A    ABSTRACT..                                                   *
000020*  FILEPASS IS NEEDED FOR VUL18 ACTIVE 22 POLICIES. THIS FILEPASS  *
000021*  IS REQUESTED TO DETERMINE ALL VUL18 POLICIES WITH ACTIVE       *
000022*  STATUS 22 ALONG WITH POLICY DETAILS, OWNER INFORMATION,        *
000023*  PLAN CODES AND ISSUE AGE.                                      *
000024*  FILEPASS REPORT.                                                *
000025*                                                                  *
000026*J    JCL..                                                        *
000027*                                                                  *
000028* //NYFPOWFF EXEC PGM=NYFPOWFF                                     *
000029* //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
000030* //SYSOUT   DD SYSOUT=*                                           *
000031* //RPTOUT   DD DSN=T54.T9511F0.NYFPOWFF.OUTPUT.DATA,             *
000032* //            DISP=(,CATLG,CATLG),                               *
000033* //            UNIT=USER,                                         *
000034* //            SPACE=(CYL,(50,30),RLSE),                          *
000035* //            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)                 *
000036* //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                   *
000037* //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                  *
000038* //SYSIPT   DD DUMMY                                              *
000039* //*                                                               *
000040*                                                                  *
000041*P    ENTRY PARAMETERS..                                           *
000042*     NONE.                                                        *
000043*                                                                  *
000044*E    ERRORS DETECTED BY THIS ELEMENT..                            *
000045*     I/O ERROR ON FILES                                           *
000046*                                                                  *
000047*C    ELEMENTS INVOKED BY THIS ELEMENT..                           *
000048*                                                                  *
000049*     CKVSAMIO ---- VSAM I/O INTERFACE                             *
000050*     CKABEND  ---- FORCE A PROGRAM INTERUPT                       *
000051*     CKETRLST ---- TRAILER LIST ELEMENT                           *
000052*     CKETRGET ---- TRAILER GET ELEMENT                            *
000053*     CKSDT1IO ---- AUX SEGMENT TABLE INTERFACE                    *
000054*     CKSETADR ---- SET ADDRESS                                    *
000055*     CKCOBCRD ---- PRINT ELEMENT                                  *
000056*     CKDCEXIN ---- DATE CONVERSION                                *
000057*     CKDCINEX ---- DATE CONVERSION                                *
000058*     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
000059*                                                                  *
000060*U    USER CONSTANTS AND TABLES REFERENCED..                       *
000061*     NONE                                                         *
000062*                                                                  *
000063********************************************************************
000064        EJECT
000065        WORKING-STORAGE SECTION.
000066        01  FILLER PIC X(32)
000067             VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
000068********************************************************************
000069*    DATA AREAS
000070********************************************************************
000071        COPY CKRECMAX.
000072        EJECT
000073********************************************************************
000074*    READ ONLY CONSTANTS
000075********************************************************************
000076        01  READ-ONLY-WORK-AREA.
000077             05 HWORD              COMP PIC S9(04) VALUE +8.
000078             05 WS-DUMMY           PIC X VALUE SPACE.
000079             05 BINARY1            COMP PIC S9(04) VALUE +1.
000080             05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
000081             05 MSG01-IO-ERROR     PIC X(19)
000082                                  VALUE 'I/O ERROR ON FILE -'.
000083* SWITCHES AREA
000084             05 END-OF-FILE-INDICATOR PIC X(1).
000085                88 END-OF-FILE VALUE 'Y'.
000086                88 CONTINUE-PROCESSING VALUE 'Y'.
000087             05 VUL18-IND           PIC X(1).
000088                88 VUL18-PRODUCT     VALUE 'Y'.
000089                88 VUL18-NOT-PRODUCT VALUE 'N'.
000090             05 ACTIVE-22-IND       PIC X(1).
000091                88 ACTIVE-22-FOUND   VALUE 'Y'.
000092                88 ACTIVE-22-NOT-FOUND VALUE 'N'.
000093* I-O READ ONLY DATA
000094             05 WS-IO-CODE                           PIC X(1).
000095                88 INFORCE-IO-COMPLETED              VALUE '0'.
000096                88 INFORCE-IO-EOF                    VALUE '6'.
000097                88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
000098                                                   '7' THRU '9'.
000099* INFORCE READ ONLY DATA
000100             05 INFORCE-FILE-LENGTH                  COMP SYNC 
000101                PIC S9(4) VALUE +12.
000102             05 INF-RECORD-KEY.
000103                10 INFORCE-KEY-FILE-CODE             PIC X.
000104                10 INFORCE-KEY-USER-ID               PIC X.
000105                10 INFORCE-KEY-POL-NUM               PIC X(10).
000106             05 INFORCE-BASIC-LENGTH                 COMP SYNC 
000107                PIC S9(4).
000108             05 INFORCE-RECSIZE                      COMP PIC S9(8) 
000109                VALUE +65000.
000110             05 FILLER REDEFINES INFORCE-RECSIZE.
000111                10 FILLER                            PIC X(2).
000112                10 INFORCE-PRMAX                     COMP PIC 9(4).
000113             05 INFORCE-MAX-SEGS                     COMP PIC S9(4)
000114                VALUE +4000.
000115* HISTORY READ ONLY DATA
000116             05 SDT-H-TABLE-NAME                     PIC X(08) 
000117                VALUE 'CKESDTBH'.
000118             05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) 
000119                VALUE +20.
000120             05 INFORCE-VSAMX-INFO.
000121                10 FILLER                            PIC X(7) 
000122                VALUE 'FVDUNLD'.
000123                10 FILLER                            PIC X 
000124                VALUE LOW-VALUE.
000125                10 FILLER                            PIC X 
000126                VALUE ' '.
000127                10 FILLER                            PIC X(08) 
000128                VALUE LOW-VALUES.
000129             05 HISTORY-VSAMX-INFO.
000130                10 FILLER           PIC X(7)   
000131                VALUE 'FVDULHD'.
000132                10 FILLER           PIC X      
000133                VALUE LOW-VALUE.
000134                10 FILLER           PIC X      
000135                VALUE ' '.
000136                10 FILLER           PIC X(11)  
000137                VALUE LOW-VALUES.
000138        EJECT
000139********************************************************************
000140*                V A R I A B L E   D A T A   A R E A S             *
000141********************************************************************
000142        01 VARIABLE-WORK-AREA.
000143             05 RECORD-LENGTH        PIC S9(8) COMP.
000144             05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
000145             05 WS-ERROR-MSG         PIC X(50).
000146             05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
000147             05 WS-SEG-ID            PIC X(02).
000148             05 WS-STATUS            PIC X(02).
000149             05 WS-ISSUE-STATE       PIC X(02).
000150             05 WS-OWNER-STATE       PIC X(02).
000151             05 WS-PLAN-BASE         PIC X(02).
000152             05 WS-PLAN-TYPE         PIC X(01).
000153             05 WS-SEG-SEQ           COMP-3 PIC S9(5)
000154                VALUE ZERO.
000155             05 WS-ACF-IO-BYTE       PIC X.
000156             05 WS-RETURN-CODE       PIC X.
000157             05 WS-SEG-WORK-AREA     PIC X(25000) 
000158                VALUE SPACE.
000159* DATE AREA
000160             05 WS-BIRTH-DATE.
000161                10 WS-BRTH-MM         PIC X(02).
000162                10 WS-BRTH-DD         PIC X(02).
000163                10 WS-BRTH-YYYY       PIC X(04).
000164             05 WS-ISSU-DATE.
000165                10 WS-ISSU-MM         PIC X(02) VALUE SPACE.
000166                10 WS-ISSU-DD         PIC X(02) VALUE SPACE.
000167                10 WS-ISSU-YYYY       PIC X(04) VALUE SPACE.
000168             05 WS-CURR-DATE.
000169                10 WS-CURR-YEAR       PIC 9(02).
000170                10 WS-CURR-MO         PIC 9(02).
000171                10 WS-CURR-DAY        PIC 9(02).
000172             05 WS-CURR-CONV-DATE.
000173                10 WS-CURR-CONV-MM    PIC 9(02).
000174                10 WS-CURR-CONV-DD    PIC 9(02).
000175                10 WS-CURR-CONV-CC    PIC 9(02).
000176                10 WS-CURR-CONV-YY    PIC 9(02).
000177             05 WS-INT-CURR-DATE     COMP-3.
000178                10 WS-INT-YEAR        PIC S9(03).
000179                10 WS-INT-DAY         PIC S9(03).
000180             05 WS-INT-ISSU-DATE     COMP-3.
000181                10 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
000182                10 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
000183             05 WS-CKDCARTH-CONTANTS.
000184                10 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
000185                10 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
000186                10 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
000187                10 WS-DCARTH-DIFFERENCE PIC X 
000188                VALUE '2'.
000189             05 WS-POLICY-READ-CNT   PIC 9(09) VALUE ZERO.
000190             05 WS-REC-WRITTEN-CNTR  PIC 9(09) VALUE ZERO.
000191        EJECT
000192********************************************************************
000193* INFORCE RECORD CONTROL SECTION
000194********************************************************************
000195
000196        01 INFORCE-FILE-AREA.
000197             05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
000198             05 INFORCE-FILE-KEY.
000199                10 INFORCE-REC-ID    PIC X(01)
000200                VALUE SPACE.
000201                10 INFORCE-USER-ID   PIC X(1)
000202                VALUE SPACE.
000203                10 INFORCE-POL-NUMBER PIC X(10)
000204                VALUE SPACE.
000205             05 INFORCE-IO-STAT      PIC X(01).
000206             05 FILLER               PIC X(64985).
000207        EJECT
000208        01 INFORCE-FILE-DCB.
000209        COPY CKDCBMAX.
000210        EJECT
000211        01 INFORCE-FILE-AUXDCB      PIC X(25000).
000212        01 HISTORY-FILE-AUXDCB      PIC X(25000).
000213        EJECT
000214********************************************************************
000215* HISTORY RECORD CONTROL SECTION
000216********************************************************************
000217
000218        COPY CKNRECRC.
000219             05 FILLER                   PIC X(13980).
000220        EJECT
000221        01 HISTORY-FILE-DCB.
000222        COPY CKDCBLRG.
000223        COPY CKUBGPRM.
000224        EJECT
000225********************************************************************
000226* OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
000227********************************************************************
000228
000229        01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
000230        EJECT
000231********************************************************************
000232* REPORT RECORD - VUL18 ACTIVE 22 DETAILS
000233********************************************************************
000234
000235        01 RP-RECORD.
000236             05 RP-POLICY             PIC X(10).
000237             05 FILLER                PIC X(01) VALUE X'05'.
000238             05 RP-POL-STATE          PIC X(02).
000239             05 FILLER                PIC X(01) VALUE X'05'.
000240             05 RP-STATUS             PIC X(02).
000241             05 FILLER                PIC X(01) VALUE X'05'.
000242             05 RP-STATUS-DESC        PIC X(30).
000243             05 FILLER                PIC X(01) VALUE X'05'.
000244             05 RP-ISSUE-DATE         PIC X(10).
000245             05 FILLER                PIC X(01) VALUE X'05'.
000246             05 RP-PRODUCT-NAME       PIC X(51).
000247             05 FILLER                PIC X(01) VALUE X'05'.
000248             05 RP-OWNER-NAME         PIC X(81).
000249             05 FILLER                PIC X(01) VALUE X'05'.
000250             05 RP-OWNER-ADDRESS      PIC X(100).
000251             05 FILLER                PIC X(01) VALUE X'05'.
000252             05 RP-PLAN-CODE          PIC X(11).
000253             05 FILLER                PIC X(01) VALUE X'05'.
000254             05 RP-ISSUE-STATE        PIC X(02).
000255             05 FILLER                PIC X(01) VALUE X'05'.
000256             05 RP-ISSUE-AGE          PIC ZZ9.
000257             05 FILLER                PIC X(01) VALUE X'05'.
000258        EJECT
000259********************************************************************
000260*                    ESSENTIAL SEGMENTS ONLY                        *
000261********************************************************************
000262
000263        COPY CKFRECCV.
000264        EJECT
000265        COPY CKFRECAU.
000266        EJECT
000267        COPY CKFRECUB.
000268        EJECT
000269* AUXSEG1 RECORD AREA
000270        COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
000271                      BY AUX-INF-DCB.
000272        EJECT
000273        COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
000274                      INFORCE-AUX-SDT.
000275        EJECT
000276********************************************************************
000277*                     BATCH  I/O  RECORD                          *
000278********************************************************************
000279
000280        COPY CKBCHCDS REPLACING
000281             BCHCODES-CALLING-CODES BY BCHCODES.
000282        01 FILLER PIC X(32)
000283             VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
000284        EJECT
000285 LINKAGE SECTION.
000286        EJECT
000287        PROCEDURE DIVISION.
000288********************************************************************
000289*                        MAINLINE LOGIC                           *
000290********************************************************************
000291
000292        0000-CONTROL-PROCESS.
000293             PERFORM 1000-INITIALIZATION
000294                 THRU 1099-INITIALIZATION-EXIT.
000295             PERFORM 1100-OPEN-FILES
000296                 THRU 1199-OPEN-FILES-EXIT.
000297             SET CONTINUE-PROCESSING TO TRUE.
000298             MOVE SPACE TO END-OF-FILE-INDICATOR.
000299             PERFORM 2000-MAIN-PROCESS
000300                 THRU 2000-MAIN-PROCESS-EXIT
000301                 UNTIL END-OF-FILE.
000302             PERFORM EOJ9000-CLOSE-FILES
000303                 THRU EOJ9999-EXIT.
000304             GOBACK.
000305        EJECT
000306********************************************************************
000307*                         INITIALIZATION                          *
000308********************************************************************
000309
000310        1000-INITIALIZATION.
000311             CALL 'CKSETADR' USING BINARY1
000312                                UBAUHCB-AUX-HIST-DCB-ADDR
000313                                AUX-HIST-DCB.
000314             MOVE LOW-VALUES TO AUX-HIST-DCB.
000315             CALL 'CKSETADR' USING BINARY1
000316                                AUXSEGDT-PTR OF AUX-HIST-DCB
000317                                HISTORY-AUX-SDT.
000318             INITIALIZE END-OF-FILE-INDICATOR.
000319             MOVE ZERO TO WS-IO-CODE.
000320             MOVE LOW-VALUES TO INFORCE-FILE-DCB.
000321             INITIALIZE INFORCE-FILE-AREA.
000322             INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
000323* GET CURRENT DATE
000324             ACCEPT WS-CURR-DATE FROM DATE.
000325             MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
000326             MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
000327             MOVE 20          TO WS-CURR-CONV-CC.
000328             MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
000329             CALL 'CKDCEXIN'
000330                 USING WS-CURR-CONV-DATE
000331                       WS-INT-CURR-DATE.
000332        1099-INITIALIZATION-EXIT.
000333             EXIT.
000334        EJECT
000335********************************************************************
000336*                         OPEN ALL FILES                          *
000337********************************************************************
000338
000339        1100-OPEN-FILES.
000340* OPEN OUTPUT REPORT FILE
000341             OPEN OUTPUT REPORT-FILE.
000342             MOVE '6' TO WS-IO-CODE.
000343             CALL 'CKVSAMIO'
000344                  USING INFORCE-VSAM
000345                        WS-IO-CODE
000346                        INFORCE-FILE-AREA
000347                        INFORCE-FILE-LENGTH
000348                        INF-RECORD-KEY
000349                        INFORCE-VSAMX-INFO.
000350             IF WS-IO-CODE NOT EQUAL '0'
000351                 DISPLAY 'OPEN INFORCE FAILED'
000352                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000353                 GO TO EOJ9900-ABEND
000354             END-IF.
000355             MOVE '6' TO WS-IO-CODE.
000356             CALL 'CKSDT1IO'
000357                  USING WS-IO-CODE
000358                        INFORCE-AUX-SDT.
000359             IF WS-IO-CODE NOT EQUAL '0'
000360                 DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
000361                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000362                 GO TO EOJ9900-ABEND
000363             END-IF.
000364             MOVE '4' TO WS-IO-CODE.
000365             CALL 'CKETRLST'
000366                  USING WS-IO-CODE
000367                        WS-DUMMY
000368                        INFORCE-FILE-DCB
000369                        WS-DUMMY
000370                        WS-DUMMY
000371             IF WS-IO-CODE NOT EQUAL '0'
000372                 DISPLAY 'ERROR IN CKETRLST'
000373                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000374                 GO TO EOJ9900-ABEND
000375             END-IF.
000376        1199-OPEN-FILES-EXIT.
000377             EXIT.
000378        EJECT
000379********************************************************************
000380*                        MAIN PROCESS                             *
000381********************************************************************
000382
000383        2000-MAIN-PROCESS.
000384             PERFORM 2100-READ-NEXT-POLICY
000385                 THRU 2199-READ-NEXT-POLICY-EXIT.
000386             IF NOT END-OF-FILE
000387                 PERFORM 2200-PROCESS-POLICY
000388                     THRU 2299-PROCESS-POLICY-EXIT
000389             END-IF.
000390        2000-MAIN-PROCESS-EXIT.
000391             EXIT.
000392        EJECT
000393********************************************************************
000394*                    READ NEXT POLICY                             *
000395********************************************************************
000396
000397        2100-READ-NEXT-POLICY.
000398             MOVE '8' TO WS-IO-CODE.
000399             CALL 'CKVSAMIO'
000400                  USING INFORCE-VSAM
000401                        WS-IO-CODE
000402                        INFORCE-FILE-AREA
000403                        INFORCE-FILE-LENGTH
000404                        INF-RECORD-KEY
000405                        INFORCE-VSAMX-INFO.
000406             IF WS-IO-CODE = '6'
000407                 SET END-OF-FILE TO TRUE
000408             ELSE
000409                 IF WS-IO-CODE NOT = '0'
000410                     DISPLAY 'READ INFORCE FAILED'
000411                     DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000412                     GO TO EOJ9900-ABEND
000413                 END-IF
000414             END-IF.
000415        2199-READ-NEXT-POLICY-EXIT.
000416             EXIT.
000417        EJECT
000418********************************************************************
000419*                     PROCESS POLICY                              *
000420********************************************************************
000421
000422 2200-PROCESS-POLICY.
000423             MOVE 'N' TO VUL18-IND
000424             MOVE 'N' TO ACTIVE-22-IND
000425* CHECK FOR VUL18 PRODUCT
000426             IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
000427                 SET VUL18-PRODUCT TO TRUE
000428             END-IF
000429* CHECK FOR ACTIVE STATUS 22
000430             IF VUL18-PRODUCT
000431                 IF CURR-STAT OF CV-SEGMENT = '22'
000432                     SET ACTIVE-22-FOUND TO TRUE
000433                 END-IF
000434             END-IF
000435* IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
000436             IF VUL18-PRODUCT AND ACTIVE-22-FOUND
000437                 PERFORM 2300-BUILD-REPORT-RECORD
000438                     THRU 2399-BUILD-REPORT-RECORD-EXIT
000439                 PERFORM 2400-WRITE-REPORT-RECORD
000440                     THRU 2499-WRITE-REPORT-RECORD-EXIT
000441             END-IF
000442        2299-PROCESS-POLICY-EXIT.
000443             EXIT.
000444        EJECT
000445********************************************************************
000446*                  BUILD REPORT RECORD                            *
000447********************************************************************
000448
000449        2300-BUILD-REPORT-RECORD.
000450* MOVE POLICY NUMBER
000451             MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
000452* MOVE POLICY STATE
000453             MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
000454* MOVE STATUS
000455             MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
000456* MOVE STATUS DESCRIPTION
000457             EVALUATE CURR-STAT OF CV-SEGMENT
000458                 WHEN '22'
000459                     MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
000460                 WHEN OTHER
000461                     MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
000462             END-EVALUATE
000463* MOVE ISSUE DATE
000464             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
000465             MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
000466* MOVE PRODUCT NAME
000467             MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
000468* MOVE OWNER NAME
000469             MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
000470* MOVE OWNER ADDRESS
000471             STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
000472                    ' ' DELIMITED BY SIZE
000473                    ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
000474                    ' ' DELIMITED BY SIZE
000475                    CITY OF AU-SEGMENT DELIMITED BY SPACE
000476                    ' ' DELIMITED BY SIZE
000477                    STATE OF AU-SEGMENT DELIMITED BY SPACE
000478                    ' ' DELIMITED BY SIZE
000479                    ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
000480                    INTO RP-OWNER-ADDRESS
000481* MOVE PLAN CODE
000482             MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
000483* MOVE ISSUE STATE
000484             MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
000485* CALCULATE AND MOVE ISSUE AGE
000486             PERFORM 2500-CALCULATE-ISSUE-AGE
000487                 THRU 2599-CALCULATE-ISSUE-AGE-EXIT
000488             MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
000489        2399-BUILD-REPORT-RECORD-EXIT.
000490             EXIT.
000491        EJECT
000492********************************************************************
000493*                 WRITE REPORT RECORD                            *
000494********************************************************************
000495
000496        2400-WRITE-REPORT-RECORD.
000497             WRITE REPORT-REC FROM RP-RECORD
000498             IF NOT WRITE-OK
000499                 DISPLAY 'WRITE ERROR ON REPORT FILE'
000500                 GO TO EOJ9900-ABEND
000501             END-IF
000502             ADD 1 TO WS-REC-WRITTEN-CNTR
000503        2499-WRITE-REPORT-RECORD-EXIT.
000504             EXIT.
000505        EJECT
000506********************************************************************
000507*               CALCULATE ISSUE AGE                               *
000508********************************************************************
000509
000510        2500-CALCULATE-ISSUE-AGE.
000511* CONVERT ISSUE DATE TO INTERNAL FORMAT
000512             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
000513             CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
000514* CALCULATE AGE DIFFERENCE
000515             CALL 'CKDCARTH' USING WS-INT-CURR-DATE
000516                           WS-INT-ISSU-DATE
000517                           WS-DCARTH-DIFFERENCE
000518                           WS-ISSUE-AGE
000519        2599-CALCULATE-ISSUE-AGE-EXIT.
000520             EXIT.
000521        EJECT
000522********************************************************************
000523*                        CLOSE FILES                              *
000524********************************************************************
000525
000526 EOJ9000-CLOSE-FILES.
000527* CLOSE REPORT FILE
000528             CLOSE REPORT-FILE
000529* CLOSE INFORCE FILE
000530             MOVE '5' TO WS-IO-CODE.
000531             CALL 'CKVSAMIO'
000532                  USING INFORCE-VSAM
000533                        WS-IO-CODE
000534* DISPLAY COUNTERS
000535             DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
000536             DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
000537             GO TO EOJ9999-EXIT.
000538        EOJ9900-ABEND.
000539             DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
000540        EOJ9999-EXIT.
000541             EXIT.
