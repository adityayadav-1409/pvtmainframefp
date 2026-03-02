=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
000001       IDENTIFICATION DIVISION.
000002       PROGRAM-ID. NYFPOWFF.
000003       AUTHOR. BHARATH CHEVIREDDY.
000004       DATE-WRITTEN. 03/2026.
000005       ENVIRONMENT DIVISION.
000006       CONFIGURATION SECTION.
000007       INPUT-OUTPUT SECTION.
000008       FILE-CONTROL.
000009           SELECT REPORT-FILE ASSIGN TO RPTOUT.
000010       DATA DIVISION.
000011       FILE SECTION.
000012       FD  REPORT-FILE
000013       RECORDING MODE IS F
000014       BLOCK CONTAINS 0 RECORDS.
000015       01  REPORT-REC        PIC X(400).
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
000064       EJECT
000065       WORKING-STORAGE SECTION.
000066       01  FILLER PIC X(32)
000067            VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
000067********************************************************************
000068*    DATA AREAS
000069********************************************************************
000070       COPY CKRECMAX.
000071       EJECT
000072********************************************************************
000073*    READ ONLY CONSTANTS
000074********************************************************************
000075       01  READ-ONLY-WORK-AREA.
000076            05 HWORD              COMP PIC S9(04) VALUE +8.
000077            05 WS-DUMMY           PIC X VALUE SPACE.
000078            05 BINARY1            COMP PIC S9(04) VALUE +1.
000079            05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
000080            05 MSG01-IO-ERROR     PIC X(19)
000081                                 VALUE 'I/O ERROR ON FILE -'.
000082* SWITCHES AREA
000083            05 END-OF-FILE-INDICATOR PIC X(1).
000084               88 END-OF-FILE VALUE 'Y'.
000085               88 CONTINUE-PROCESSING VALUE 'Y'.
000086            05 VUL18-IND           PIC X(1).
000087               88 VUL18-PRODUCT     VALUE 'Y'.
000088               88 VUL18-NOT-PRODUCT VALUE 'N'.
000089            05 ACTIVE-22-IND       PIC X(1).
000090               88 ACTIVE-22-FOUND   VALUE 'Y'.
000091               88 ACTIVE-22-NOT-FOUND VALUE 'N'.
000092* I-O READ ONLY DATA
000093            05 WS-IO-CODE                           PIC X(1).
000094               88 INFORCE-IO-COMPLETED              VALUE '0'.
000095               88 INFORCE-IO-EOF                    VALUE '6'.
000096               88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
000097                                                  '7' THRU '9'.
000098* INFORCE READ ONLY DATA
000099            05 INFORCE-FILE-LENGTH                  COMP SYNC 
000100               PIC S9(4) VALUE +12.
000101            05 INF-RECORD-KEY.
000102               10 INFORCE-KEY-FILE-CODE             PIC X.
000103               10 INFORCE-KEY-USER-ID               PIC X.
000104               10 INFORCE-KEY-POL-NUM               PIC X(10).
000105            05 INFORCE-BASIC-LENGTH                 COMP SYNC 
000106               PIC S9(4).
000107            05 INFORCE-RECSIZE                      COMP PIC S9(8) 
000108               VALUE +65000.
000109            05 FILLER REDEFINES INFORCE-RECSIZE.
000110               10 FILLER                            PIC X(2).
000111               10 INFORCE-PRMAX                     COMP PIC 9(4).
000112            05 INFORCE-MAX-SEGS                     COMP PIC S9(4) VALUE +4000.
000113* HISTORY READ ONLY DATA
000114            05 SDT-H-TABLE-NAME                     PIC X(08) 
000115               VALUE 'CKESDTBH'.
000116            05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) 
000117               VALUE +20.
000118            05 INFORCE-VSAMX-INFO.
000119               10 FILLER                            PIC X(7) 
000120               VALUE 'FVDUNLD'.
000121               10 FILLER                            PIC X 
000122               VALUE LOW-VALUE.
000123               10 FILLER                            PIC X 
000124               VALUE ' '.
000125               10 FILLER                            PIC X(08) 
000126               VALUE LOW-VALUES.
000127            05 HISTORY-VSAMX-INFO.
000128               10 FILLER           PIC X(7)   
000129               VALUE 'FVDULHD'.
000130               10 FILLER           PIC X      
000131               VALUE LOW-VALUE.
000132               10 FILLER           PIC X      
000133               VALUE ' '.
000134               10 FILLER           PIC X(11)  
000135               VALUE LOW-VALUES.
000136       EJECT
000136********************************************************************
000137*                V A R I A B L E   D A T A   A R E A S             *
000138********************************************************************
000139       01 VARIABLE-WORK-AREA.
000140            05 RECORD-LENGTH        PIC S9(8) COMP.
000141            05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
000142            05 WS-ERROR-MSG         PIC X(50).
000143            05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
000144            05 WS-SEG-ID            PIC X(02).
000145            05 WS-STATUS            PIC X(02).
000146            05 WS-ISSUE-STATE       PIC X(02).
000147            05 WS-OWNER-STATE       PIC X(02).
000148            05 WS-PLAN-BASE         PIC X(02).
000149            05 WS-PLAN-TYPE         PIC X(01).
000150            05 WS-SEG-SEQ           COMP-3 PIC S9(5)
000151               VALUE ZERO.
000152            05 WS-ACF-IO-BYTE       PIC X.
000153            05 WS-RETURN-CODE       PIC X.
000154            05 WS-SEG-WORK-AREA     PIC X(25000) 
000155               VALUE SPACE.
000156* DATE AREA
000157            05 WS-BIRTH-DATE.
000158               10 WS-BRTH-MM         PIC X(02).
000159               10 WS-BRTH-DD         PIC X(02).
000160               10 WS-BRTH-YYYY       PIC X(04).
000161            05 WS-ISSU-DATE.
000162               10 WS-ISSU-MM         PIC X(02) VALUE SPACE.
000163               10 WS-ISSU-DD         PIC X(02) VALUE SPACE.
000164               10 WS-ISSU-YYYY       PIC X(04) VALUE SPACE.
000165            05 WS-CURR-DATE.
000166               10 WS-CURR-YEAR       PIC 9(02).
000167               10 WS-CURR-MO         PIC 9(02).
000168               10 WS-CURR-DAY        PIC 9(02).
000169            05 WS-CURR-CONV-DATE.
000170               10 WS-CURR-CONV-MM    PIC 9(02).
000171               10 WS-CURR-CONV-DD    PIC 9(02).
000172               10 WS-CURR-CONV-CC    PIC 9(02).
000173               10 WS-CURR-CONV-YY    PIC 9(02).
000174            05 WS-INT-CURR-DATE     COMP-3.
000175               10 WS-INT-YEAR        PIC S9(03).
000176               10 WS-INT-DAY         PIC S9(03).
000177            05 WS-INT-ISSU-DATE     COMP-3.
000178               10 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
000179               10 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
000180            05 WS-CKDCARTH-CONTANTS.
000181               10 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
000182               10 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
000183               10 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
000184               10 WS-DCARTH-DIFFERENCE PIC X 
000185               VALUE '2'.
000186            05 WS-POLICY-READ-CNT   PIC 9(09) VALUE ZERO.
000187            05 WS-REC-WRITTEN-CNTR  PIC 9(09) VALUE ZERO.
000188       EJECT
000188********************************************************************
000189* INFORCE RECORD CONTROL SECTION
000190********************************************************************
000191
000192       01 INFORCE-FILE-AREA.
000193            05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
000194            05 INFORCE-FILE-KEY.
000195               10 INFORCE-REC-ID    PIC X(01)
000196               VALUE SPACE.
000197               10 INFORCE-USER-ID   PIC X(1)
000198               VALUE SPACE.
000199               10 INFORCE-POL-NUMBER PIC X(10)
000200               VALUE SPACE.
000201            05 INFORCE-IO-STAT      PIC X(01).
000202            05 FILLER               PIC X(64985).
000203       EJECT
000204       01 INFORCE-FILE-DCB.
000205       COPY CKDCBMAX.
000206       EJECT
000207       01 INFORCE-FILE-AUXDCB      PIC X(25000).
000208       01 HISTORY-FILE-AUXDCB      PIC X(25000).
000209       EJECT
000210********************************************************************
000211* HISTORY RECORD CONTROL SECTION
000212********************************************************************
000213
000214       COPY CKNRECRC.
000215            05 FILLER                   PIC X(13980).
000216       EJECT
000217       01 HISTORY-FILE-DCB.
000218       COPY CKDCBLRG.
000219       COPY CKUBGPRM.
000220       EJECT
000221********************************************************************
000222* OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
000223********************************************************************
000224
000225       01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
000226       EJECT
000227********************************************************************
000228* REPORT RECORD - VUL18 ACTIVE 22 DETAILS
000229********************************************************************
000230
000231       01 RP-RECORD.
000232            05 RP-POLICY             PIC X(10).
000233            05 FILLER                PIC X(01) VALUE X'05'.
000234            05 RP-POL-STATE          PIC X(02).
000235            05 FILLER                PIC X(01) VALUE X'05'.
000236            05 RP-STATUS             PIC X(02).
000237            05 FILLER                PIC X(01) VALUE X'05'.
000238            05 RP-STATUS-DESC        PIC X(30).
000239            05 FILLER                PIC X(01) VALUE X'05'.
000240            05 RP-ISSUE-DATE         PIC X(10).
000241            05 FILLER                PIC X(01) VALUE X'05'.
000242            05 RP-PRODUCT-NAME       PIC X(51).
000243            05 FILLER                PIC X(01) VALUE X'05'.
000244            05 RP-OWNER-NAME         PIC X(81).
000245            05 FILLER                PIC X(01) VALUE X'05'.
000246            05 RP-OWNER-ADDRESS      PIC X(100).
000247            05 FILLER                PIC X(01) VALUE X'05'.
000248            05 RP-PLAN-CODE          PIC X(11).
000249            05 FILLER                PIC X(01) VALUE X'05'.
000250            05 RP-ISSUE-STATE        PIC X(02).
000251            05 FILLER                PIC X(01) VALUE X'05'.
000252            05 RP-ISSUE-AGE          PIC ZZ9.
000253            05 FILLER                PIC X(01) VALUE X'05'.
000254       EJECT
000255********************************************************************
000256*                    ESSENTIAL SEGMENTS ONLY                        *
000257********************************************************************
000258
000259       COPY CKFRECCV.
000260       EJECT
000261       COPY CKFRECAU.
000262       EJECT
000263       COPY CKFRECUB.
000264       EJECT
000265* AUXSEG1 RECORD AREA
000266       COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
000267                     BY AUX-INF-DCB.
000268       EJECT
000269       COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
000270                     INFORCE-AUX-SDT.
000271       EJECT
000272********************************************************************
000273*                     BATCH  I/O  RECORD                          *
000274********************************************************************
000275
000276       COPY CKBCHCDS REPLACING
000277            BCHCODES-CALLING-CODES BY BCHCODES.
000278       01 FILLER PIC X(32)
000279            VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
000280       EJECT
000281 LINKAGE SECTION.
000282       EJECT
000283       PROCEDURE DIVISION.
000284********************************************************************
000285*                        MAINLINE LOGIC                           *
000286********************************************************************
000287
000288       0000-CONTROL-PROCESS.
000289            PERFORM 1000-INITIALIZATION
000290                THRU 1099-INITIALIZATION-EXIT.
000291            PERFORM 1100-OPEN-FILES
000292                THRU 1199-OPEN-FILES-EXIT.
000293            SET CONTINUE-PROCESSING TO TRUE.
000294            MOVE SPACE TO END-OF-FILE-INDICATOR.
000295            PERFORM 2000-MAIN-PROCESS
000296                THRU 2000-MAIN-PROCESS-EXIT
000297                UNTIL END-OF-FILE.
000298            PERFORM EOJ9000-CLOSE-FILES
000299                THRU EOJ9999-EXIT.
000300            GOBACK.
000301       EJECT
000302********************************************************************
000303*                         INITIALIZATION                          *
000304********************************************************************
000305
000306       1000-INITIALIZATION.
000307            CALL 'CKSETADR' USING BINARY1
000308                               UBAUHCB-AUX-HIST-DCB-ADDR
000309                               AUX-HIST-DCB.
000310            MOVE LOW-VALUES TO AUX-HIST-DCB.
000311            CALL 'CKSETADR' USING BINARY1
000312                               AUXSEGDT-PTR OF AUX-HIST-DCB
000313                               HISTORY-AUX-SDT.
000314            INITIALIZE END-OF-FILE-INDICATOR.
000315            MOVE ZERO TO WS-IO-CODE.
000316            MOVE LOW-VALUES TO INFORCE-FILE-DCB.
000317            INITIALIZE INFORCE-FILE-AREA.
000318            INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
000319* GET CURRENT DATE
000320            ACCEPT WS-CURR-DATE FROM DATE.
000321            MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
000322            MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
000323            MOVE 20          TO WS-CURR-CONV-CC.
000324            MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
000325            CALL 'CKDCEXIN'
000326                USING WS-CURR-CONV-DATE
000327                      WS-INT-CURR-DATE.
000328       1099-INITIALIZATION-EXIT.
000329            EXIT.
000330       EJECT
000331********************************************************************
000332*                         OPEN ALL FILES                          *
000333********************************************************************
000334
000335       1100-OPEN-FILES.
000336* OPEN OUTPUT REPORT FILE
000337            OPEN OUTPUT REPORT-FILE.
000338            MOVE '6' TO WS-IO-CODE.
000339            CALL 'CKVSAMIO'
000340                 USING INFORCE-VSAM
000341                       WS-IO-CODE
000342                       INFORCE-FILE-AREA
000343                       INFORCE-FILE-LENGTH
000344                       INF-RECORD-KEY
000345                       INFORCE-VSAMX-INFO.
000346            IF WS-IO-CODE NOT EQUAL '0'
000347                DISPLAY 'OPEN INFORCE FAILED'
000348                DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000349                GO TO EOJ9900-ABEND
000350            END-IF.
000351            MOVE '6' TO WS-IO-CODE.
000352            CALL 'CKSDT1IO'
000353                 USING WS-IO-CODE
000354                       INFORCE-AUX-SDT.
000355            IF WS-IO-CODE NOT EQUAL '0'
000356                DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
000357                DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000358                GO TO EOJ9900-ABEND
000359            END-IF.
000360            MOVE '4' TO WS-IO-CODE.
000361            CALL 'CKETRLST'
000362                 USING WS-IO-CODE
000363                       WS-DUMMY
000364                       INFORCE-FILE-DCB
000365                       WS-DUMMY
000366                       WS-DUMMY
000367            IF WS-IO-CODE NOT EQUAL '0'
000368                DISPLAY 'ERROR IN CKETRLST'
000369                DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000370                GO TO EOJ9900-ABEND
000371            END-IF.
000372       1199-OPEN-FILES-EXIT.
000373            EXIT.
000374       EJECT
000375********************************************************************
000376*                        MAIN PROCESS                             *
000377********************************************************************
000378
000379       2000-MAIN-PROCESS.
000380            PERFORM 2100-READ-NEXT-POLICY
000381                THRU 2199-READ-NEXT-POLICY-EXIT.
000382            IF NOT END-OF-FILE
000383                PERFORM 2200-PROCESS-POLICY
000384                    THRU 2299-PROCESS-POLICY-EXIT
000385            END-IF.
000386       2000-MAIN-PROCESS-EXIT.
000387            EXIT.
000388       EJECT
000389********************************************************************
000390*                    READ NEXT POLICY                             *
000391********************************************************************
000392
000393       2100-READ-NEXT-POLICY.
000394            MOVE '8' TO WS-IO-CODE.
000395            CALL 'CKVSAMIO'
000396                 USING INFORCE-VSAM
000397                       WS-IO-CODE
000398                       INFORCE-FILE-AREA
000399                       INFORCE-FILE-LENGTH
000400                       INF-RECORD-KEY
000401                       INFORCE-VSAMX-INFO.
000402            IF WS-IO-CODE = '6'
000403                SET END-OF-FILE TO TRUE
000404            ELSE
000405                IF WS-IO-CODE NOT = '0'
000406                    DISPLAY 'READ INFORCE FAILED'
000407                    DISPLAY 'WS-IO-CODE=' WS-IO-CODE
000408                    GO TO EOJ9900-ABEND
000409                END-IF
000410            END-IF.
000411       2199-READ-NEXT-POLICY-EXIT.
000412            EXIT.
000413       EJECT
000414********************************************************************
000415*                     PROCESS POLICY                              *
000416********************************************************************
000417
000418 2200-PROCESS-POLICY.
000419            MOVE 'N' TO VUL18-IND
000420            MOVE 'N' TO ACTIVE-22-IND
000421* CHECK FOR VUL18 PRODUCT
000422            IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
000423                SET VUL18-PRODUCT TO TRUE
000424            END-IF
000425* CHECK FOR ACTIVE STATUS 22
000426            IF VUL18-PRODUCT
000427                IF CURR-STAT OF CV-SEGMENT = '22'
000428                    SET ACTIVE-22-FOUND TO TRUE
000429                END-IF
000430            END-IF
000431* IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
000432            IF VUL18-PRODUCT AND ACTIVE-22-FOUND
000433                PERFORM 2300-BUILD-REPORT-RECORD
000434                    THRU 2399-BUILD-REPORT-RECORD-EXIT
000435                PERFORM 2400-WRITE-REPORT-RECORD
000436                    THRU 2499-WRITE-REPORT-RECORD-EXIT
000437            END-IF
000438       2299-PROCESS-POLICY-EXIT.
000439            EXIT.
000440       EJECT
000441********************************************************************
000442*                  BUILD REPORT RECORD                            *
000443********************************************************************
000444
000445       2300-BUILD-REPORT-RECORD.
000446* MOVE POLICY NUMBER
000447            MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
000448* MOVE POLICY STATE
000449            MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
000450* MOVE STATUS
000451            MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
000452* MOVE STATUS DESCRIPTION
000453            EVALUATE CURR-STAT OF CV-SEGMENT
000454                WHEN '22'
000455                    MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
000456                WHEN OTHER
000457                    MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
000458            END-EVALUATE
000459* MOVE ISSUE DATE
000460            MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
000461            MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
000462* MOVE PRODUCT NAME
000463            MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
000464* MOVE OWNER NAME
000465            MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
000466* MOVE OWNER ADDRESS
000467            STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
000468                   ' ' DELIMITED BY SIZE
000469                   ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
000470                   ' ' DELIMITED BY SIZE
000471                   CITY OF AU-SEGMENT DELIMITED BY SPACE
000472                   ' ' DELIMITED BY SIZE
000473                   STATE OF AU-SEGMENT DELIMITED BY SPACE
000474                   ' ' DELIMITED BY SIZE
000475                   ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
000476                   INTO RP-OWNER-ADDRESS
000477* MOVE PLAN CODE
000478            MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
000479* MOVE ISSUE STATE
000480            MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
000481* CALCULATE AND MOVE ISSUE AGE
000482            PERFORM 2500-CALCULATE-ISSUE-AGE
000483                THRU 2599-CALCULATE-ISSUE-AGE-EXIT
000484            MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
000485       2399-BUILD-REPORT-RECORD-EXIT.
000486            EXIT.
000487       EJECT
000488********************************************************************
000489*                 WRITE REPORT RECORD                            *
000490********************************************************************
000491
000492       2400-WRITE-REPORT-RECORD.
000493            WRITE REPORT-REC FROM RP-RECORD
000494            IF NOT WRITE-OK
000495                DISPLAY 'WRITE ERROR ON REPORT FILE'
000496                GO TO EOJ9900-ABEND
000497            END-IF
000498            ADD 1 TO WS-REC-WRITTEN-CNTR
000499       2499-WRITE-REPORT-RECORD-EXIT.
000500            EXIT.
000501       EJECT
000502********************************************************************
000503*               CALCULATE ISSUE AGE                               *
000504********************************************************************
000505
000506       2500-CALCULATE-ISSUE-AGE.
000507* CONVERT ISSUE DATE TO INTERNAL FORMAT
000508            MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
000509            CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
000510* CALCULATE AGE DIFFERENCE
000511            CALL 'CKDCARTH' USING WS-INT-CURR-DATE
000512                          WS-INT-ISSU-DATE
000513                          WS-DCARTH-DIFFERENCE
000514                          WS-ISSUE-AGE
000515       2599-CALCULATE-ISSUE-AGE-EXIT.
000516            EXIT.
000517       EJECT
000518********************************************************************
000519*                        CLOSE FILES                              *
000520********************************************************************
000521
000522 EOJ9000-CLOSE-FILES.
000523* CLOSE REPORT FILE
000524            CLOSE REPORT-FILE
000525* CLOSE INFORCE FILE
000526            MOVE '5' TO WS-IO-CODE.
000527            CALL 'CKVSAMIO'
000528                 USING INFORCE-VSAM
000529                       WS-IO-CODE
000530* DISPLAY COUNTERS
000531            DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
000532            DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
000533            GO TO EOJ9999-EXIT.
000534       EOJ9900-ABEND.
000535            DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
000536       EOJ9999-EXIT.
000537            EXIT.
