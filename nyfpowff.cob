=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. NYFPOWFF.
000003 AUTHOR. BHARATH CHEVIREDDY.
000004 DATE-WRITTEN. 03/2026.
000005 ENVIRONMENT DIVISION.
000006 CONFIGURATION SECTION.
000007 INPUT-OUTPUT SECTION.
000008 FILE-CONTROL.
000009     SELECT REPORT-FILE ASSIGN TO RPTOUT.
000010 DATA DIVISION.
000011 FILE SECTION.
000012 FD  REPORT-FILE
000013     RECORDING MODE IS F
000014     BLOCK CONTAINS 0 RECORDS.
000015 01  REPORT-REC        PIC X(400).
000016********************************************************************
000017*                                                                  *
000018*A    ABSTRACT..                                                   *
000019*  FILEPASS IS NEEDED FOR VUL18 ACTIVE 22 POLICIES. THIS FILEPASS  *
000020*  IS REQUESTED TO DETERMINE ALL VUL18 POLICIES WITH ACTIVE       *
000021*  STATUS 22 ALONG WITH POLICY DETAILS, OWNER INFORMATION,        *
000022*  PLAN CODES AND ISSUE AGE.                                      *
000023*  FILEPASS REPORT.                                                *
000024*                                                                  *
000025*J    JCL..                                                        *
000026*                                                                  *
000027* //NYFPOWFF EXEC PGM=NYFPOWFF                                     *
000028* //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
000029* //SYSOUT   DD SYSOUT=*                                           *
000030* //RPTOUT   DD DSN=T54.T9511F0.NYFPOWFF.OUTPUT.DATA,             *
000031* //            DISP=(,CATLG,CATLG),                               *
000032* //            UNIT=USER,                                         *
000033* //            SPACE=(CYL,(50,30),RLSE),                          *
000034* //            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)                 *
000035* //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                   *
000036* //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                  *
000037* //SYSIPT   DD DUMMY                                              *
000038* //*                                                               *
000039*                                                                  *
000040*P    ENTRY PARAMETERS..                                           *
000041*     NONE.                                                        *
000042*                                                                  *
000043*E    ERRORS DETECTED BY THIS ELEMENT..                            *
000044*     I/O ERROR ON FILES                                           *
000045*                                                                  *
000046*C    ELEMENTS INVOKED BY THIS ELEMENT..                           *
000047*                                                                  *
000048*     CKVSAMIO ---- VSAM I/O INTERFACE                             *
000049*     CKABEND  ---- FORCE A PROGRAM INTERUPT                       *
000050*     CKETRLST ---- TRAILER LIST ELEMENT                           *
000051*     CKETRGET ---- TRAILER GET ELEMENT                            *
000052*     CKSDT1IO ---- AUX SEGMENT TABLE INTERFACE                    *
000053*     CKSETADR ---- SET ADDRESS                                    *
000054*     CKCOBCRD ---- PRINT ELEMENT                                  *
000055*     CKDCEXIN ---- DATE CONVERSION                                *
000056*     CKDCINEX ---- DATE CONVERSION                                *
000057*     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
000058*                                                                  *
000059*U    USER CONSTANTS AND TABLES REFERENCED..                       *
000060*     NONE                                                         *
000061*                                                                  *
000062********************************************************************
000063 EJECT
000064 WORKING-STORAGE SECTION.
000065 01  FILLER PIC X(32)
000066     VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
000067********************************************************************
000068*    DATA AREAS
000069********************************************************************
000070 COPY CKRECMAX.
000071 EJECT
000072********************************************************************
000073*    READ ONLY CONSTANTS
000074********************************************************************
000075 01  READ-ONLY-WORK-AREA.
000076     05 HWORD              COMP PIC S9(04) VALUE +7.
000077     05 WS-DUMMY           PIC X VALUE SPACE.
000078     05 BINARY1            COMP PIC S9(04) VALUE +1.
000079     05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
000080     05 MSG01-IO-ERROR     PIC X(19)
000081                           VALUE 'I/O ERROR ON FILE -'.
000082* SWITCHES AREA
000083     05 END-OF-FILE-INDICATOR PIC X(1).
000084        88 END-OF-FILE VALUE 'Y'.
000085        88 CONTINUE-PROCESSING VALUE 'Y'.
000086     05 VUL18-IND           PIC X(1).
000087        88 VUL18-PRODUCT     VALUE 'Y'.
000088        88 VUL18-NOT-PRODUCT VALUE 'N'.
000089     05 ACTIVE-22-IND       PIC X(1).
000090        88 ACTIVE-22-FOUND   VALUE 'Y'.
000091        88 ACTIVE-22-NOT-FOUND VALUE 'N'.
000092* I-O READ ONLY DATA
000093 05 WS-IO-CODE                           PIC X(1).
000094     88 INFORCE-IO-COMPLETED              VALUE '0'.
000095     88 INFORCE-IO-EOF                    VALUE '6'.
000096     88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
000097                                                 '7' THRU '9'.
000098* INFORCE READ ONLY DATA
000099 05 INFORCE-FILE-LENGTH                  COMP SYNC PIC S9(4) VALUE +12.
000100 05 INF-RECORD-KEY.
000101     10 INFORCE-KEY-FILE-CODE             PIC X.
000102     10 INFORCE-KEY-USER-ID               PIC X.
000103     10 INFORCE-KEY-POL-NUM               PIC X(10).
000104 05 INFORCE-BASIC-LENGTH                 COMP SYNC PIC S9(4).
000105 05 INFORCE-RECSIZE                      COMP PIC S9(8) VALUE +65000.
000106 05 FILLER REDEFINES INFORCE-RECSIZE.
000107     10 FILLER                            PIC X(2).
000108     10 INFORCE-PRMAX                     COMP PIC 9(4).
000109 05 INFORCE-MAX-SEGS                     COMP PIC S9(4) VALUE +4000.
000110* HISTORY READ ONLY DATA
000111 05 SDT-H-TABLE-NAME                     PIC X(08) VALUE 'CKESDTBH'.
000112 05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) VALUE +20.
000113 05 INFORCE-VSAMX-INFO.
000114     10 FILLER                            PIC X(7) VALUE 'FVDUNLD'.
000115     10 FILLER                            PIC X VALUE LOW-VALUE.
000116     10 FILLER                            PIC X VALUE ' '.
000117     10 FILLER                            PIC X(08) VALUE LOW-VALUES.
000118 05 HISTORY-VSAMX-INFO.
000119     10 FILLER           PIC X(7)   VALUE 'FVDULHD'.
000120     10 FILLER           PIC X      VALUE LOW-VALUE.
000121     10 FILLER           PIC X      VALUE ' '.
000122     10 FILLER           PIC X(11)  VALUE LOW-VALUES.
000123 EJECT
000124********************************************************************
000125*                V A R I A B L E   D A T A   A R E A S             *
000126********************************************************************
000127 01 VARIABLE-WORK-AREA.
000128     05 RECORD-LENGTH        PIC S9(8) COMP.
000129     05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
000130     05 WS-ERROR-MSG         PIC X(50).
000131     05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
000132     05 WS-SEG-ID            PIC X(02).
000133     05 WS-STATUS            PIC X(02).
000134     05 WS-ISSUE-STATE       PIC X(02).
000135     05 WS-OWNER-STATE       PIC X(02).
000136     05 WS-PLAN-BASE         PIC X(02).
000137     05 WS-PLAN-TYPE         PIC X(01).
000138     05 WS-SEG-SEQ           COMP-3 PIC S9(5).
000139     05 WS-ACF-IO-BYTE       PIC X.
000140     05 WS-RETURN-CODE       PIC X.
000141     05 WS-SEG-WORK-AREA     PIC X(25000) VALUE SPACE.
000142* DATE AREA
000143     05 WS-BIRTH-DATE.
000144         10 WS-BRTH-MM         PIC X(02).
000145         10 WS-BRTH-DD         PIC X(02).
000146         10 WS-BRTH-YYYY       PIC X(04).
000147     05 WS-ISSU-DATE.
000148         10 WS-ISSU-MM         PIC X(02) VALUE SPACE.
000149         10 WS-ISSU-DD         PIC X(02) VALUE SPACE.
000150         10 WS-ISSU-YYYY       PIC X(04) VALUE SPACE.
000151     05 WS-CURR-DATE.
000152         10 WS-CURR-YEAR       PIC 9(02).
000153         10 WS-CURR-MO         PIC 9(02).
000154         10 WS-CURR-DAY        PIC 9(02).
000155     05 WS-CURR-CONV-DATE.
000156         10 WS-CURR-CONV-MM    PIC 9(02).
000157         10 WS-CURR-CONV-DD    PIC 9(02).
000158         10 WS-CURR-CONV-CC    PIC 9(02).
000159         10 WS-CURR-CONV-YY    PIC 9(02).
000160     05 WS-INT-CURR-DATE     COMP-3.
000161         10 WS-INT-YEAR        PIC S9(03).
000162         10 WS-INT-DAY         PIC S9(03).
000163     05 WS-INT-ISSU-DATE     COMP-3.
000164         10 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
000165         10 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
000166     05 WS-CKDCARTH-CONTANTS.
000167         10 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
000168         10 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
000169         10 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
000170         10 WS-DCARTH-DIFFERENCE PIC X VALUE '2'.
000171     05 WS-POLICY-READ-CNT   PIC 9(09) VALUE ZERO.
000172     05 WS-REC-WRITTEN-CNTR  PIC 9(09) VALUE ZERO.
000173 EJECT
000174********************************************************************
000175* INFORCE RECORD CONTROL SECTION
000176********************************************************************
000177
000178 01 INFORCE-FILE-AREA.
000179     05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
000180     05 INFORCE-FILE-KEY.
000181         10 INFORCE-REC-ID    PIC X(01).
000182         10 INFORCE-USER-ID   PIC X(1).
000183         10 INFORCE-POL-NUMBER PIC X(10).
000184     05 INFORCE-IO-STAT      PIC X(01).
000185     05 FILLER               PIC X(64985).
000186 EJECT
000187 01 INFORCE-FILE-DCB.
000188     COPY CKDCBMAX.
000189 EJECT
000190 01 INFORCE-FILE-AUXDCB      PIC X(25000).
000191 01 HISTORY-FILE-AUXDCB      PIC X(25000).
000192 EJECT
000193********************************************************************
000194* HISTORY RECORD CONTROL SECTION
000195********************************************************************
000196
000197 COPY CKNRECRC.
000198 05 FILLER                   PIC X(13980).
000199 EJECT
000200 01 HISTORY-FILE-DCB.
000201     COPY CKDCBLRG.
000202     COPY CKUBGPRM.
000203 EJECT
000204********************************************************************
000205* OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
000206********************************************************************
000207
000208 01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
000209 EJECT
000210********************************************************************
000211* REPORT RECORD - VUL18 ACTIVE 22 DETAILS
000212********************************************************************
000213
000214 01 RP-RECORD.
000215     05 RP-POLICY             PIC X(10).
000216     05 FILLER                PIC X(01) VALUE X'05'.
000217     05 RP-POL-STATE          PIC X(02).
000218     05 FILLER                PIC X(01) VALUE X'05'.
000219     05 RP-STATUS             PIC X(02).
000220     05 FILLER                PIC X(01) VALUE X'05'.
000221     05 RP-STATUS-DESC        PIC X(30).
000222     05 FILLER                PIC X(01) VALUE X'05'.
000223     05 RP-ISSUE-DATE         PIC X(10).
000224     05 FILLER                PIC X(01) VALUE X'05'.
000225     05 RP-PRODUCT-NAME       PIC X(51).
000226     05 FILLER                PIC X(01) VALUE X'05'.
000227     05 RP-OWNER-NAME         PIC X(81).
000228     05 FILLER                PIC X(01) VALUE X'05'.
000229     05 RP-OWNER-ADDRESS      PIC X(100).
000230     05 FILLER                PIC X(01) VALUE X'05'.
000231     05 RP-PLAN-CODE          PIC X(11).
000232     05 FILLER                PIC X(01) VALUE X'05'.
000233     05 RP-ISSUE-STATE        PIC X(02).
000234     05 FILLER                PIC X(01) VALUE X'05'.
000235     05 RP-ISSUE-AGE          PIC ZZ9.
000236     05 FILLER                PIC X(01) VALUE X'05'.
000237 EJECT
000238********************************************************************
000239*                    ESSENTIAL SEGMENTS ONLY                        *
000240********************************************************************
000241
000242 COPY CKFRECCV.
000243 EJECT
000244 COPY CKFRECAU.
000245 EJECT
000246 COPY CKFRECUB.
000247 EJECT
000248* AUXSEG1 RECORD AREA
000249 COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
000250                     BY AUX-INF-DCB.
000251 EJECT
000252 COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
000253                     INFORCE-AUX-SDT.
000254 EJECT
000255********************************************************************
000256*                     BATCH  I/O  RECORD                          *
000257********************************************************************
000258
000259 COPY CKBCHCDS REPLACING
000260      BCHCODES-CALLING-CODES BY BCHCODES.
000261 01 FILLER PIC X(32)
000262    VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
000263 EJECT
000264 LINKAGE SECTION.
000265 EJECT
000266 PROCEDURE DIVISION.
000267********************************************************************
000268*                        MAINLINE LOGIC                           *
000269********************************************************************
000270
000271 0000-CONTROL-PROCESS.
000272     PERFORM 1000-INITIALIZATION
000273         THRU 1099-INITIALIZATION-EXIT.
000274     PERFORM 1100-OPEN-FILES
000275         THRU 1199-OPEN-FILES-EXIT.
000276     SET CONTINUE-PROCESSING TO TRUE.
000277     MOVE SPACE TO END-OF-FILE-INDICATOR.
000278     PERFORM 2000-MAIN-PROCESS
000279         THRU 2000-MAIN-PROCESS-EXIT
000280         UNTIL END-OF-FILE.
000281     PERFORM EOJ9000-CLOSE-FILES
000282         THRU EOJ9999-EXIT.
000283     GOBACK.
000284 EJECT
000285********************************************************************
000286*                         INITIALIZATION                          *
000287********************************************************************
000288
000289 1000-INITIALIZATION.
000290     CALL 'CKSETADR' USING BINARY1
000291                            UBAUHCB-AUX-HIST-DCB-ADDR
000292                            AUX-HIST-DCB.
000293     MOVE LOW-VALUES TO AUX-HIST-DCB.
000294     CALL 'CKSETADR' USING BINARY1
000295                            AUXSEGDT-PTR OF AUX-HIST-DCB
000296                            HISTORY-AUX-SDT.
000297     INITIALIZE END-OF-FILE-INDICATOR.
000298     MOVE ZERO TO WS-IO-CODE.
000299     MOVE LOW-VALUES TO INFORCE-FILE-DCB.
000300     INITIALIZE INFORCE-FILE-AREA.
000301     INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
000302* GET CURRENT DATE
000303     ACCEPT WS-CURR-DATE FROM DATE.
000304     MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
000305     MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
000306     MOVE 20          TO WS-CURR-CONV-CC.
000307     MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
000308     CALL 'CKDCEXIN'
000309         USING WS-CURR-CONV-DATE
000310               WS-INT-CURR-DATE.
000311 1099-INITIALIZATION-EXIT.
000312     EXIT.
000313 EJECT
000314********************************************************************
000315*                         OPEN ALL FILES                          *
000316********************************************************************
000317
000318 1100-OPEN-FILES.
000319* OPEN OUTPUT REPORT FILE
000320     OPEN OUTPUT REPORT-FILE.
000321     MOVE '6' TO WS-IO-CODE.
000322     CALL 'CKVSAMIO'
000323          USING INFORCE-VSAM
000324                WS-IO-CODE
000325                INFORCE-FILE-AREA
000326                INFORCE-FILE-LENGTH
000327                INF-RECORD-KEY
000328                INFORCE-VSAMX-INFO.
000329     IF WS-IO-CODE NOT EQUAL '0'
000330         DISPLAY 'OPEN INFORCE FAILED'
000331         DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000332         GO TO EOJ9900-ABEND
000333     END-IF.
000334     MOVE '6' TO WS-IO-CODE.
000335     CALL 'CKSDT1IO'
000336          USING WS-IO-CODE
000337                INFORCE-AUX-SDT.
000338     IF WS-IO-CODE NOT EQUAL '0'
000339         DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
000340         DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000341         GO TO EOJ9900-ABEND
000342     END-IF.
000343     MOVE '4' TO WS-IO-CODE.
000344     CALL 'CKETRLST'
000345          USING WS-IO-CODE
000346                WS-DUMMY
000347                INFORCE-FILE-DCB
000348                WS-DUMMY
000349                WS-DUMMY.
000350 1199-OPEN-FILES-EXIT.
000351     EXIT.
000352 EJECT
000353********************************************************************
000354*                        MAIN PROCESS                             *
000355********************************************************************
000356
000357 2000-MAIN-PROCESS.
000358     PERFORM 2100-READ-NEXT-POLICY
000359         THRU 2199-READ-NEXT-POLICY-EXIT.
000360     IF NOT END-OF-FILE
000361         PERFORM 2200-PROCESS-POLICY
000362             THRU 2299-PROCESS-POLICY-EXIT
000363     END-IF.
000364 2000-MAIN-PROCESS-EXIT.
000365     EXIT.
000366 EJECT
000367********************************************************************
000368*                    READ NEXT POLICY                             *
000369********************************************************************
000370
000371 2100-READ-NEXT-POLICY.
000372     MOVE '8' TO WS-IO-CODE.
000373     CALL 'CKVSAMIO'
000374          USING INFORCE-VSAM
000375                WS-IO-CODE
000376                INFORCE-FILE-AREA
000377                INFORCE-FILE-LENGTH
000378                INF-RECORD-KEY
000379                INFORCE-VSAMX-INFO.
000380     IF WS-IO-CODE = '6'
000381         SET END-OF-FILE TO TRUE
000382     ELSE
000383         IF WS-IO-CODE NOT = '0'
000384             DISPLAY 'READ INFORCE FAILED'
000385             DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000386             GO TO EOJ9900-ABEND
000387         END-IF
000388     END-IF.
000389 2199-READ-NEXT-POLICY-EXIT.
000390     EXIT.
000391 EJECT
000392********************************************************************
000393*                     PROCESS POLICY                              *
000394********************************************************************
000395
000396 2200-PROCESS-POLICY.
000397     MOVE 'N' TO VUL18-IND
000398     MOVE 'N' TO ACTIVE-22-IND
000399* CHECK FOR VUL18 PRODUCT
000400     IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
000401         SET VUL18-PRODUCT TO TRUE
000402     END-IF
000403* CHECK FOR ACTIVE STATUS 22
000404     IF VUL18-PRODUCT
000405         IF CURR-STAT OF CV-SEGMENT = '22'
000406             SET ACTIVE-22-FOUND TO TRUE
000407         END-IF
000408     END-IF
000409* IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
000410     IF VUL18-PRODUCT AND ACTIVE-22-FOUND
000411         PERFORM 2300-BUILD-REPORT-RECORD
000412             THRU 2399-BUILD-REPORT-RECORD-EXIT
000413         PERFORM 2400-WRITE-REPORT-RECORD
000414             THRU 2499-WRITE-REPORT-RECORD-EXIT
000415     END-IF
000416 2299-PROCESS-POLICY-EXIT.
000417     EXIT.
000418 EJECT
000419********************************************************************
000420*                  BUILD REPORT RECORD                            *
000421********************************************************************
000422
000423 2300-BUILD-REPORT-RECORD.
000424* MOVE POLICY NUMBER
000425     MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
000426* MOVE POLICY STATE
000427     MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
000428* MOVE STATUS
000429     MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
000430* MOVE STATUS DESCRIPTION
000431     EVALUATE CURR-STAT OF CV-SEGMENT
000432         WHEN '22'
000433             MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
000434         WHEN OTHER
000435             MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
000436     END-EVALUATE
000437* MOVE ISSUE DATE
000438     MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
000439     MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
000440* MOVE PRODUCT NAME
000441     MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
000442* MOVE OWNER NAME
000443     MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
000444* MOVE OWNER ADDRESS
000445     STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
000446            ' ' DELIMITED BY SIZE
000447            ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
000448            ' ' DELIMITED BY SIZE
000449            CITY OF AU-SEGMENT DELIMITED BY SPACE
000450            ' ' DELIMITED BY SIZE
000451            STATE OF AU-SEGMENT DELIMITED BY SPACE
000452            ' ' DELIMITED BY SIZE
000453            ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
000454            INTO RP-OWNER-ADDRESS
000455* MOVE PLAN CODE
000456     MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
000457* MOVE ISSUE STATE
000458     MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
000459* CALCULATE AND MOVE ISSUE AGE
000460     PERFORM 2500-CALCULATE-ISSUE-AGE
000461         THRU 2599-CALCULATE-ISSUE-AGE-EXIT
000462     MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
000463 2399-BUILD-REPORT-RECORD-EXIT.
000464     EXIT.
000465 EJECT
000466********************************************************************
000467*                 WRITE REPORT RECORD                            *
000468********************************************************************
000469
000470 2400-WRITE-REPORT-RECORD.
000471     WRITE REPORT-REC FROM RP-RECORD
000472     IF NOT WRITE-OK
000473         DISPLAY 'WRITE ERROR ON REPORT FILE'
000474         GO TO EOJ9900-ABEND
000475     END-IF
000476     ADD 1 TO WS-REC-WRITTEN-CNTR
000477 2499-WRITE-REPORT-RECORD-EXIT.
000478     EXIT.
000479 EJECT
000480********************************************************************
000481*               CALCULATE ISSUE AGE                               *
000482********************************************************************
000483
000484 2500-CALCULATE-ISSUE-AGE.
000485* CONVERT ISSUE DATE TO INTERNAL FORMAT
000486     MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
000487     CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
000488* CALCULATE AGE DIFFERENCE
000489     CALL 'CKDCARTH' USING WS-INT-CURR-DATE
000490                           WS-INT-ISSU-DATE
000491                           WS-DCARTH-DIFFERENCE
000492                           WS-ISSUE-AGE
000493 2599-CALCULATE-ISSUE-AGE-EXIT.
000494     EXIT.
000495 EJECT
000496********************************************************************
000497*                        CLOSE FILES                              *
000498********************************************************************
000499
000500 EOJ9000-CLOSE-FILES.
000501* CLOSE REPORT FILE
000502     CLOSE REPORT-FILE
000503* CLOSE INFORCE FILE
000504     MOVE '5' TO WS-IO-CODE.
000505     CALL 'CKVSAMIO'
000506          USING INFORCE-VSAM
000507                WS-IO-CODE
000508* DISPLAY COUNTERS
000509     DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
000510     DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
000511     GO TO EOJ9999-EXIT.
000512 EOJ9900-ABEND.
000513     DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
000514 EOJ9999-EXIT.
000515     EXIT.