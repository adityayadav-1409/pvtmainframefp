 =COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               IDENTIFICATION DIVISION.
               PROGRAM-ID. NYFPOWFF.
               AUTHOR. BHARATH CHEVIREDDY.
               DATE-WRITTEN. 03/2026.
               ENVIRONMENT DIVISION.
               CONFIGURATION SECTION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT REPORT-FILE ASSIGN TO RPTOUT.
        DATA DIVISION.
        FILE SECTION.
        FD  REPORT-FILE
        RECORDING MODE IS F
        BLOCK CONTAINS 0 RECORDS.
        01  REPORT-REC        PIC X(400).
        ********************************************************************
        *                                                                  *
        *A    ABSTRACT..                                                   *
        *  FILEPASS IS NEEDED FOR VUL18 ACTIVE 22 POLICIES. THIS FILEPASS  *
        *  IS REQUESTED TO DETERMINE ALL VUL18 POLICIES WITH ACTIVE       *
        *  STATUS 22 ALONG WITH POLICY DETAILS, OWNER INFORMATION,        *
        *  PLAN CODES AND ISSUE AGE.                                      *
        *  FILEPASS REPORT.                                                *
        *                                                                  *
        *J    JCL..                                                        *
        *                                                                  *
        * //NYFPOWFF EXEC PGM=NYFPOWFF                                     *
        * //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
        * //SYSOUT   DD SYSOUT=*                                           *
        * //RPTOUT   DD DSN=T54.T9511F0.NYFPOWFF.OUTPUT.DATA,             *
        * //            DISP=(,CATLG,CATLG),                               *
        * //            UNIT=USER,                                         *
        * //            SPACE=(CYL,(50,30),RLSE),                          *
        * //            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)                 *
        * //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                   *
        * //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                  *
        * //SYSIPT   DD DUMMY                                              *
        * //*                                                               *
        *                                                                  *
        *P    ENTRY PARAMETERS..                                           *
        *     NONE.                                                        *
        *                                                                  *
        *E    ERRORS DETECTED BY THIS ELEMENT..                            *
        *     I/O ERROR ON FILES                                           *
        *                                                                  *
        *C    ELEMENTS INVOKED BY THIS ELEMENT..                           *
        *                                                                  *
        *     CKVSAMIO ---- VSAM I/O INTERFACE                             *
        *     CKABEND  ---- FORCE A PROGRAM INTERUPT                       *
        *     CKETRLST ---- TRAILER LIST ELEMENT                           *
        *     CKETRGET ---- TRAILER GET ELEMENT                            *
        *     CKSDT1IO ---- AUX SEGMENT TABLE INTERFACE                    *
        *     CKSETADR ---- SET ADDRESS                                    *
        *     CKCOBCRD ---- PRINT ELEMENT                                  *
        *     CKDCEXIN ---- DATE CONVERSION                                *
        *     CKDCINEX ---- DATE CONVERSION                                *
        *     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
        *                                                                  *
        *U    USER CONSTANTS AND TABLES REFERENCED..                       *
        *     NONE                                                         *
        *                                                                  *
        ********************************************************************
        EJECT
        WORKING-STORAGE SECTION.
        01  FILLER PIC X(32)
             VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
        ********************************************************************
        *    DATA AREAS
        ********************************************************************
        COPY CKRECMAX.
        EJECT
        ********************************************************************
        *    READ ONLY CONSTANTS
        ********************************************************************
        01  READ-ONLY-WORK-AREA.
             05 HWORD              COMP PIC S9(04) VALUE +8.
             05 WS-DUMMY           PIC X VALUE SPACE.
             05 BINARY1            COMP PIC S9(04) VALUE +1.
             05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
             05 MSG01-IO-ERROR     PIC X(19)
                                  VALUE 'I/O ERROR ON FILE -'.
        * SWITCHES AREA
             05 END-OF-FILE-INDICATOR PIC X(1).
                88 END-OF-FILE VALUE 'Y'.
                88 CONTINUE-PROCESSING VALUE 'Y'.
             05 VUL18-IND           PIC X(1).
                88 VUL18-PRODUCT     VALUE 'Y'.
                88 VUL18-NOT-PRODUCT VALUE 'N'.
             05 ACTIVE-22-IND       PIC X(1).
                88 ACTIVE-22-FOUND   VALUE 'Y'.
                88 ACTIVE-22-NOT-FOUND VALUE 'N'.
        * I-O READ ONLY DATA
             05 WS-IO-CODE                           PIC X(1).
                88 INFORCE-IO-COMPLETED              VALUE '0'.
                88 INFORCE-IO-EOF                    VALUE '6'.
                88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
                                                   '7' THRU '9'.
        * INFORCE READ ONLY DATA
             05 INFORCE-FILE-LENGTH                  COMP SYNC 
                PIC S9(4) VALUE +12.
             05 INF-RECORD-KEY.
                10 INFORCE-KEY-FILE-CODE             PIC X.
                10 INFORCE-KEY-USER-ID               PIC X.
                10 INFORCE-KEY-POL-NUM               PIC X(10).
             05 INFORCE-BASIC-LENGTH                 COMP SYNC 
                PIC S9(4).
             05 INFORCE-RECSIZE                      COMP PIC S9(8) 
                VALUE +65000.
             05 FILLER REDEFINES INFORCE-RECSIZE.
                10 FILLER                            PIC X(2).
                10 INFORCE-PRMAX                     COMP PIC 9(4).
             05 INFORCE-MAX-SEGS                     COMP PIC S9(4)
                VALUE +4000.
        * HISTORY READ ONLY DATA
             05 SDT-H-TABLE-NAME                     PIC X(08) 
                VALUE 'CKESDTBH'.
             05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) 
                VALUE +20.
             05 INFORCE-VSAMX-INFO.
                10 FILLER                            PIC X(7) 
                VALUE 'FVDUNLD'.
                10 FILLER                            PIC X 
                VALUE LOW-VALUE.
                10 FILLER                            PIC X 
                VALUE ' '.
                10 FILLER                            PIC X(08) 
                VALUE LOW-VALUES.
             05 HISTORY-VSAMX-INFO.
                10 FILLER           PIC X(7)   
                VALUE 'FVDULHD'.
                10 FILLER           PIC X      
                VALUE LOW-VALUE.
                10 FILLER           PIC X      
                VALUE ' '.
                10 FILLER           PIC X(11)  
                VALUE LOW-VALUES.
        EJECT
        ********************************************************************
        *                V A R I A B L E   D A T A   A R E A S             *
        ********************************************************************
        01 VARIABLE-WORK-AREA.
             05 RECORD-LENGTH        PIC S9(8) COMP.
             05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
             05 WS-ERROR-MSG         PIC X(50).
             05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
             05 WS-SEG-ID            PIC X(02).
             05 WS-STATUS            PIC X(02).
             05 WS-ISSUE-STATE       PIC X(02).
             05 WS-OWNER-STATE       PIC X(02).
             05 WS-PLAN-BASE         PIC X(02).
             05 WS-PLAN-TYPE         PIC X(01).
             05 WS-SEG-SEQ           COMP-3 PIC S9(5)
                VALUE ZERO.
             05 WS-ACF-IO-BYTE       PIC X.
             05 WS-RETURN-CODE       PIC X.
             05 WS-SEG-WORK-AREA     PIC X(25000) 
                VALUE SPACE.
        * DATE AREA
             05 WS-BIRTH-DATE.
                10 WS-BRTH-MM         PIC X(02).
                10 WS-BRTH-DD         PIC X(02).
                10 WS-BRTH-YYYY       PIC X(04).
             05 WS-ISSU-DATE.
                10 WS-ISSU-MM         PIC X(02) VALUE SPACE.
                10 WS-ISSU-DD         PIC X(02) VALUE SPACE.
                10 WS-ISSU-YYYY       PIC X(04) VALUE SPACE.
             05 WS-CURR-DATE.
                10 WS-CURR-YEAR       PIC 9(02).
                10 WS-CURR-MO         PIC 9(02).
                10 WS-CURR-DAY        PIC 9(02).
             05 WS-CURR-CONV-DATE.
                10 WS-CURR-CONV-MM    PIC 9(02).
                10 WS-CURR-CONV-DD    PIC 9(02).
                10 WS-CURR-CONV-CC    PIC 9(02).
                10 WS-CURR-CONV-YY    PIC 9(02).
             05 WS-INT-CURR-DATE     COMP-3.
                10 WS-INT-YEAR        PIC S9(03).
                10 WS-INT-DAY         PIC S9(03).
             05 WS-INT-ISSU-DATE     COMP-3.
                10 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
                10 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
             05 WS-CKDCARTH-CONTANTS.
                10 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
                10 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
                10 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
                10 WS-DCARTH-DIFFERENCE PIC X 
                VALUE '2'.
             05 WS-POLICY-READ-CNT   PIC 9(09) VALUE ZERO.
             05 WS-REC-WRITTEN-CNTR  PIC 9(09) VALUE ZERO.
        EJECT
        ********************************************************************
        * INFORCE RECORD CONTROL SECTION
        ********************************************************************
        
        01 INFORCE-FILE-AREA.
             05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
             05 INFORCE-FILE-KEY.
                10 INFORCE-REC-ID    PIC X(01)
                VALUE SPACE.
                10 INFORCE-USER-ID   PIC X(1)
                VALUE SPACE.
                10 INFORCE-POL-NUMBER PIC X(10)
                VALUE SPACE.
             05 INFORCE-IO-STAT      PIC X(01).
             05 FILLER               PIC X(64985).
        EJECT
        01 INFORCE-FILE-DCB.
        COPY CKDCBMAX.
        EJECT
        01 INFORCE-FILE-AUXDCB      PIC X(25000).
        01 HISTORY-FILE-AUXDCB      PIC X(25000).
        EJECT
        ********************************************************************
        * HISTORY RECORD CONTROL SECTION
        ********************************************************************
        
        COPY CKNRECRC.
             05 FILLER                   PIC X(13980).
        EJECT
        01 HISTORY-FILE-DCB.
        COPY CKDCBLRG.
        COPY CKUBGPRM.
        EJECT
        ********************************************************************
        * OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
        ********************************************************************
        
        01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
        EJECT
        ********************************************************************
        * REPORT RECORD - VUL18 ACTIVE 22 DETAILS
        ********************************************************************
        
        01 RP-RECORD.
             05 RP-POLICY             PIC X(10).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-POL-STATE          PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-STATUS             PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-STATUS-DESC        PIC X(30).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-DATE         PIC X(10).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-PRODUCT-NAME       PIC X(51).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-OWNER-NAME         PIC X(81).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-OWNER-ADDRESS      PIC X(100).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-PLAN-CODE          PIC X(11).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-STATE        PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-AGE          PIC ZZ9.
             05 FILLER                PIC X(01) VALUE X'05'.
        EJECT
        ********************************************************************
        *                    ESSENTIAL SEGMENTS ONLY                        *
        ********************************************************************
        
        COPY CKFRECCV.
        EJECT
        COPY CKFRECAU.
        EJECT
        COPY CKFRECUB.
        EJECT
        * AUXSEG1 RECORD AREA
        COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
                      BY AUX-INF-DCB.
        EJECT
        COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
                      INFORCE-AUX-SDT.
        EJECT
        ********************************************************************
        *                     BATCH  I/O  RECORD                          *
        ********************************************************************
        
        COPY CKBCHCDS REPLACING
             BCHCODES-CALLING-CODES BY BCHCODES.
        01 FILLER PIC X(32)
             VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
        EJECT
         LINKAGE SECTION.
        EJECT
        PROCEDURE DIVISION.
        ********************************************************************
        *                        MAINLINE LOGIC                           *
        ********************************************************************
        
        0000-CONTROL-PROCESS.
             PERFORM 1000-INITIALIZATION
                 THRU 1099-INITIALIZATION-EXIT.
             PERFORM 1100-OPEN-FILES
                 THRU 1199-OPEN-FILES-EXIT.
             SET CONTINUE-PROCESSING TO TRUE.
             MOVE SPACE TO END-OF-FILE-INDICATOR.
             PERFORM 2000-MAIN-PROCESS
                 THRU 2000-MAIN-PROCESS-EXIT
                 UNTIL END-OF-FILE.
             PERFORM EOJ9000-CLOSE-FILES
                 THRU EOJ9999-EXIT.
             GOBACK.
        EJECT
        ********************************************************************
        *                         INITIALIZATION                          *
        ********************************************************************
        
        1000-INITIALIZATION.
             CALL 'CKSETADR' USING BINARY1
                                UBAUHCB-AUX-HIST-DCB-ADDR
                                AUX-HIST-DCB.
             MOVE LOW-VALUES TO AUX-HIST-DCB.
             CALL 'CKSETADR' USING BINARY1
                                AUXSEGDT-PTR OF AUX-HIST-DCB
                                HISTORY-AUX-SDT.
             INITIALIZE END-OF-FILE-INDICATOR.
             MOVE ZERO TO WS-IO-CODE.
             MOVE LOW-VALUES TO INFORCE-FILE-DCB.
             INITIALIZE INFORCE-FILE-AREA.
             INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
        * GET CURRENT DATE
             ACCEPT WS-CURR-DATE FROM DATE.
             MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
             MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
             MOVE 20          TO WS-CURR-CONV-CC.
             MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
             CALL 'CKDCEXIN'
                 USING WS-CURR-CONV-DATE
                       WS-INT-CURR-DATE.
        1099-INITIALIZATION-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                         OPEN ALL FILES                          *
        ********************************************************************
        
        1100-OPEN-FILES.
        * OPEN OUTPUT REPORT FILE
             OPEN OUTPUT REPORT-FILE.
             MOVE '6' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
                        INFORCE-FILE-AREA
                        INFORCE-FILE-LENGTH
                        INF-RECORD-KEY
                        INFORCE-VSAMX-INFO.
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'OPEN INFORCE FAILED'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
             MOVE '6' TO WS-IO-CODE.
             CALL 'CKSDT1IO'
                  USING WS-IO-CODE
                        INFORCE-AUX-SDT.
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
             MOVE '4' TO WS-IO-CODE.
             CALL 'CKETRLST'
                  USING WS-IO-CODE
                        WS-DUMMY
                        INFORCE-FILE-DCB
                        WS-DUMMY
                        WS-DUMMY
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'ERROR IN CKETRLST'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
        1199-OPEN-FILES-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                        MAIN PROCESS                             *
        ********************************************************************
        
        2000-MAIN-PROCESS.
             PERFORM 2100-READ-NEXT-POLICY
                 THRU 2199-READ-NEXT-POLICY-EXIT.
             IF NOT END-OF-FILE
                 PERFORM 2200-PROCESS-POLICY
                     THRU 2299-PROCESS-POLICY-EXIT
             END-IF.
        2000-MAIN-PROCESS-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                    READ NEXT POLICY                             *
        ********************************************************************
        
        2100-READ-NEXT-POLICY.
             MOVE '8' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
                        INFORCE-FILE-AREA
                        INFORCE-FILE-LENGTH
                        INF-RECORD-KEY
                        INFORCE-VSAMX-INFO.
             IF WS-IO-CODE = '6'
                 SET END-OF-FILE TO TRUE
             ELSE
                 IF WS-IO-CODE NOT = '0'
                     DISPLAY 'READ INFORCE FAILED'
                     DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                     GO TO EOJ9900-ABEND
                 END-IF
             END-IF.
        2199-READ-NEXT-POLICY-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                     PROCESS POLICY                              *
        ********************************************************************
        
         2200-PROCESS-POLICY.
             MOVE 'N' TO VUL18-IND
             MOVE 'N' TO ACTIVE-22-IND
        * CHECK FOR VUL18 PRODUCT
             IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
                 SET VUL18-PRODUCT TO TRUE
             END-IF
        * CHECK FOR ACTIVE STATUS 22
             IF VUL18-PRODUCT
                 IF CURR-STAT OF CV-SEGMENT = '22'
                     SET ACTIVE-22-FOUND TO TRUE
                 END-IF
             END-IF
        * IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
             IF VUL18-PRODUCT AND ACTIVE-22-FOUND
                 PERFORM 2300-BUILD-REPORT-RECORD
                     THRU 2399-BUILD-REPORT-RECORD-EXIT
                 PERFORM 2400-WRITE-REPORT-RECORD
                     THRU 2499-WRITE-REPORT-RECORD-EXIT
             END-IF
        2299-PROCESS-POLICY-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                  BUILD REPORT RECORD                            *
        ********************************************************************
        
        2300-BUILD-REPORT-RECORD.
        * MOVE POLICY NUMBER
             MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
        * MOVE POLICY STATE
             MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
        * MOVE STATUS
             MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
        * MOVE STATUS DESCRIPTION
             EVALUATE CURR-STAT OF CV-SEGMENT
                 WHEN '22'
                     MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
                 WHEN OTHER
                     MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
             END-EVALUATE
        * MOVE ISSUE DATE
             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
             MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
        * MOVE PRODUCT NAME
             MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
        * MOVE OWNER NAME
             MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
        * MOVE OWNER ADDRESS
             STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    CITY OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    STATE OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
                    INTO RP-OWNER-ADDRESS
        * MOVE PLAN CODE
             MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
        * MOVE ISSUE STATE
             MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
        * CALCULATE AND MOVE ISSUE AGE
             PERFORM 2500-CALCULATE-ISSUE-AGE
                 THRU 2599-CALCULATE-ISSUE-AGE-EXIT
             MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
        2399-BUILD-REPORT-RECORD-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                 WRITE REPORT RECORD                            *
        ********************************************************************
        
        2400-WRITE-REPORT-RECORD.
             WRITE REPORT-REC FROM RP-RECORD
             IF NOT WRITE-OK
                 DISPLAY 'WRITE ERROR ON REPORT FILE'
                 GO TO EOJ9900-ABEND
             END-IF
             ADD 1 TO WS-REC-WRITTEN-CNTR
        2499-WRITE-REPORT-RECORD-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *               CALCULATE ISSUE AGE                               *
        ********************************************************************
        
        2500-CALCULATE-ISSUE-AGE.
        * CONVERT ISSUE DATE TO INTERNAL FORMAT
             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
             CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
        * CALCULATE AGE DIFFERENCE
             CALL 'CKDCARTH' USING WS-INT-CURR-DATE
                           WS-INT-ISSU-DATE
                           WS-DCARTH-DIFFERENCE
                           WS-ISSUE-AGE
        2599-CALCULATE-ISSUE-AGE-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                        CLOSE FILES                              *
        ********************************************************************
        
         EOJ9000-CLOSE-FILES.
        * CLOSE REPORT FILE
             CLOSE REPORT-FILE
        * CLOSE INFORCE FILE
             MOVE '5' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
        * DISPLAY COUNTERS
             DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
             DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
             GO TO EOJ9999-EXIT.
        EOJ9900-ABEND.
             DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
        EOJ9999-EXIT.
             EXIT.=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               IDENTIFICATION DIVISION.
               PROGRAM-ID. NYFPOWFF.
               AUTHOR. BHARATH CHEVIREDDY.
               DATE-WRITTEN. 03/2026.
               ENVIRONMENT DIVISION.
               CONFIGURATION SECTION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT REPORT-FILE ASSIGN TO RPTOUT.
        DATA DIVISION.
        FILE SECTION.
        FD  REPORT-FILE
        RECORDING MODE IS F
        BLOCK CONTAINS 0 RECORDS.
        01  REPORT-REC        PIC X(400).
        ********************************************************************
        *                                                                  *
        *A    ABSTRACT..                                                   *
        *  FILEPASS IS NEEDED FOR VUL18 ACTIVE 22 POLICIES. THIS FILEPASS  *
        *  IS REQUESTED TO DETERMINE ALL VUL18 POLICIES WITH ACTIVE       *
        *  STATUS 22 ALONG WITH POLICY DETAILS, OWNER INFORMATION,        *
        *  PLAN CODES AND ISSUE AGE.                                      *
        *  FILEPASS REPORT.                                                *
        *                                                                  *
        *J    JCL..                                                        *
        *                                                                  *
        * //NYFPOWFF EXEC PGM=NYFPOWFF                                     *
        * //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
        * //SYSOUT   DD SYSOUT=*                                           *
        * //RPTOUT   DD DSN=T54.T9511F0.NYFPOWFF.OUTPUT.DATA,             *
        * //            DISP=(,CATLG,CATLG),                               *
        * //            UNIT=USER,                                         *
        * //            SPACE=(CYL,(50,30),RLSE),                          *
        * //            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)                 *
        * //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                   *
        * //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                  *
        * //SYSIPT   DD DUMMY                                              *
        * //*                                                               *
        *                                                                  *
        *P    ENTRY PARAMETERS..                                           *
        *     NONE.                                                        *
        *                                                                  *
        *E    ERRORS DETECTED BY THIS ELEMENT..                            *
        *     I/O ERROR ON FILES                                           *
        *                                                                  *
        *C    ELEMENTS INVOKED BY THIS ELEMENT..                           *
        *                                                                  *
        *     CKVSAMIO ---- VSAM I/O INTERFACE                             *
        *     CKABEND  ---- FORCE A PROGRAM INTERUPT                       *
        *     CKETRLST ---- TRAILER LIST ELEMENT                           *
        *     CKETRGET ---- TRAILER GET ELEMENT                            *
        *     CKSDT1IO ---- AUX SEGMENT TABLE INTERFACE                    *
        *     CKSETADR ---- SET ADDRESS                                    *
        *     CKCOBCRD ---- PRINT ELEMENT                                  *
        *     CKDCEXIN ---- DATE CONVERSION                                *
        *     CKDCINEX ---- DATE CONVERSION                                *
        *     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
        *                                                                  *
        *U    USER CONSTANTS AND TABLES REFERENCED..                       *
        *     NONE                                                         *
        *                                                                  *
        ********************************************************************
        EJECT
        WORKING-STORAGE SECTION.
        01  FILLER PIC X(32)
             VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
        ********************************************************************
        *    DATA AREAS
        ********************************************************************
        COPY CKRECMAX.
        EJECT
        ********************************************************************
        *    READ ONLY CONSTANTS
        ********************************************************************
        01  READ-ONLY-WORK-AREA.
             05 HWORD              COMP PIC S9(04) VALUE +8.
             05 WS-DUMMY           PIC X VALUE SPACE.
             05 BINARY1            COMP PIC S9(04) VALUE +1.
             05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
             05 MSG01-IO-ERROR     PIC X(19)
                                  VALUE 'I/O ERROR ON FILE -'.
        * SWITCHES AREA
             05 END-OF-FILE-INDICATOR PIC X(1).
                88 END-OF-FILE VALUE 'Y'.
                88 CONTINUE-PROCESSING VALUE 'Y'.
             05 VUL18-IND           PIC X(1).
                88 VUL18-PRODUCT     VALUE 'Y'.
                88 VUL18-NOT-PRODUCT VALUE 'N'.
             05 ACTIVE-22-IND       PIC X(1).
                88 ACTIVE-22-FOUND   VALUE 'Y'.
                88 ACTIVE-22-NOT-FOUND VALUE 'N'.
        * I-O READ ONLY DATA
             05 WS-IO-CODE                           PIC X(1).
                88 INFORCE-IO-COMPLETED              VALUE '0'.
                88 INFORCE-IO-EOF                    VALUE '6'.
                88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
                                                   '7' THRU '9'.
        * INFORCE READ ONLY DATA
             05 INFORCE-FILE-LENGTH                  COMP SYNC 
                PIC S9(4) VALUE +12.
             05 INF-RECORD-KEY.
                10 INFORCE-KEY-FILE-CODE             PIC X.
                10 INFORCE-KEY-USER-ID               PIC X.
                10 INFORCE-KEY-POL-NUM               PIC X(10).
             05 INFORCE-BASIC-LENGTH                 COMP SYNC 
                PIC S9(4).
             05 INFORCE-RECSIZE                      COMP PIC S9(8) 
                VALUE +65000.
             05 FILLER REDEFINES INFORCE-RECSIZE.
                10 FILLER                            PIC X(2).
                10 INFORCE-PRMAX                     COMP PIC 9(4).
             05 INFORCE-MAX-SEGS                     COMP PIC S9(4)
                VALUE +4000.
        * HISTORY READ ONLY DATA
             05 SDT-H-TABLE-NAME                     PIC X(08) 
                VALUE 'CKESDTBH'.
             05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) 
                VALUE +20.
             05 INFORCE-VSAMX-INFO.
                10 FILLER                            PIC X(7) 
                VALUE 'FVDUNLD'.
                10 FILLER                            PIC X 
                VALUE LOW-VALUE.
                10 FILLER                            PIC X 
                VALUE ' '.
                10 FILLER                            PIC X(08) 
                VALUE LOW-VALUES.
             05 HISTORY-VSAMX-INFO.
                10 FILLER           PIC X(7)   
                VALUE 'FVDULHD'.
                10 FILLER           PIC X      
                VALUE LOW-VALUE.
                10 FILLER           PIC X      
                VALUE ' '.
                10 FILLER           PIC X(11)  
                VALUE LOW-VALUES.
        EJECT
        ********************************************************************
        *                V A R I A B L E   D A T A   A R E A S             *
        ********************************************************************
        01 VARIABLE-WORK-AREA.
             05 RECORD-LENGTH        PIC S9(8) COMP.
             05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
             05 WS-ERROR-MSG         PIC X(50).
             05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
             05 WS-SEG-ID            PIC X(02).
             05 WS-STATUS            PIC X(02).
             05 WS-ISSUE-STATE       PIC X(02).
             05 WS-OWNER-STATE       PIC X(02).
             05 WS-PLAN-BASE         PIC X(02).
             05 WS-PLAN-TYPE         PIC X(01).
             05 WS-SEG-SEQ           COMP-3 PIC S9(5)
                VALUE ZERO.
             05 WS-ACF-IO-BYTE       PIC X.
             05 WS-RETURN-CODE       PIC X.
             05 WS-SEG-WORK-AREA     PIC X(25000) 
                VALUE SPACE.
        * DATE AREA
             05 WS-BIRTH-DATE.
                10 WS-BRTH-MM         PIC X(02).
                10 WS-BRTH-DD         PIC X(02).
                10 WS-BRTH-YYYY       PIC X(04).
             05 WS-ISSU-DATE.
                10 WS-ISSU-MM         PIC X(02) VALUE SPACE.
                10 WS-ISSU-DD         PIC X(02) VALUE SPACE.
                10 WS-ISSU-YYYY       PIC X(04) VALUE SPACE.
             05 WS-CURR-DATE.
                10 WS-CURR-YEAR       PIC 9(02).
                10 WS-CURR-MO         PIC 9(02).
                10 WS-CURR-DAY        PIC 9(02).
             05 WS-CURR-CONV-DATE.
                10 WS-CURR-CONV-MM    PIC 9(02).
                10 WS-CURR-CONV-DD    PIC 9(02).
                10 WS-CURR-CONV-CC    PIC 9(02).
                10 WS-CURR-CONV-YY    PIC 9(02).
             05 WS-INT-CURR-DATE     COMP-3.
                10 WS-INT-YEAR        PIC S9(03).
                10 WS-INT-DAY         PIC S9(03).
             05 WS-INT-ISSU-DATE     COMP-3.
                10 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
                10 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
             05 WS-CKDCARTH-CONTANTS.
                10 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
                10 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
                10 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
                10 WS-DCARTH-DIFFERENCE PIC X 
                VALUE '2'.
             05 WS-POLICY-READ-CNT   PIC 9(09) VALUE ZERO.
             05 WS-REC-WRITTEN-CNTR  PIC 9(09) VALUE ZERO.
        EJECT
        ********************************************************************
        * INFORCE RECORD CONTROL SECTION
        ********************************************************************
        
        01 INFORCE-FILE-AREA.
             05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
             05 INFORCE-FILE-KEY.
                10 INFORCE-REC-ID    PIC X(01)
                VALUE SPACE.
                10 INFORCE-USER-ID   PIC X(1)
                VALUE SPACE.
                10 INFORCE-POL-NUMBER PIC X(10)
                VALUE SPACE.
             05 INFORCE-IO-STAT      PIC X(01).
             05 FILLER               PIC X(64985).
        EJECT
        01 INFORCE-FILE-DCB.
        COPY CKDCBMAX.
        EJECT
        01 INFORCE-FILE-AUXDCB      PIC X(25000).
        01 HISTORY-FILE-AUXDCB      PIC X(25000).
        EJECT
        ********************************************************************
        * HISTORY RECORD CONTROL SECTION
        ********************************************************************
        
        COPY CKNRECRC.
             05 FILLER                   PIC X(13980).
        EJECT
        01 HISTORY-FILE-DCB.
        COPY CKDCBLRG.
        COPY CKUBGPRM.
        EJECT
        ********************************************************************
        * OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
        ********************************************************************
        
        01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
        EJECT
        ********************************************************************
        * REPORT RECORD - VUL18 ACTIVE 22 DETAILS
        ********************************************************************
        
        01 RP-RECORD.
             05 RP-POLICY             PIC X(10).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-POL-STATE          PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-STATUS             PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-STATUS-DESC        PIC X(30).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-DATE         PIC X(10).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-PRODUCT-NAME       PIC X(51).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-OWNER-NAME         PIC X(81).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-OWNER-ADDRESS      PIC X(100).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-PLAN-CODE          PIC X(11).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-STATE        PIC X(02).
             05 FILLER                PIC X(01) VALUE X'05'.
             05 RP-ISSUE-AGE          PIC ZZ9.
             05 FILLER                PIC X(01) VALUE X'05'.
        EJECT
        ********************************************************************
        *                    ESSENTIAL SEGMENTS ONLY                        *
        ********************************************************************
        
        COPY CKFRECCV.
        EJECT
        COPY CKFRECAU.
        EJECT
        COPY CKFRECUB.
        EJECT
        * AUXSEG1 RECORD AREA
        COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
                      BY AUX-INF-DCB.
        EJECT
        COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
                      INFORCE-AUX-SDT.
        EJECT
        ********************************************************************
        *                     BATCH  I/O  RECORD                          *
        ********************************************************************
        
        COPY CKBCHCDS REPLACING
             BCHCODES-CALLING-CODES BY BCHCODES.
        01 FILLER PIC X(32)
             VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
        EJECT
         LINKAGE SECTION.
        EJECT
        PROCEDURE DIVISION.
        ********************************************************************
        *                        MAINLINE LOGIC                           *
        ********************************************************************
        
        0000-CONTROL-PROCESS.
             PERFORM 1000-INITIALIZATION
                 THRU 1099-INITIALIZATION-EXIT.
             PERFORM 1100-OPEN-FILES
                 THRU 1199-OPEN-FILES-EXIT.
             SET CONTINUE-PROCESSING TO TRUE.
             MOVE SPACE TO END-OF-FILE-INDICATOR.
             PERFORM 2000-MAIN-PROCESS
                 THRU 2000-MAIN-PROCESS-EXIT
                 UNTIL END-OF-FILE.
             PERFORM EOJ9000-CLOSE-FILES
                 THRU EOJ9999-EXIT.
             GOBACK.
        EJECT
        ********************************************************************
        *                         INITIALIZATION                          *
        ********************************************************************
        
        1000-INITIALIZATION.
             CALL 'CKSETADR' USING BINARY1
                                UBAUHCB-AUX-HIST-DCB-ADDR
                                AUX-HIST-DCB.
             MOVE LOW-VALUES TO AUX-HIST-DCB.
             CALL 'CKSETADR' USING BINARY1
                                AUXSEGDT-PTR OF AUX-HIST-DCB
                                HISTORY-AUX-SDT.
             INITIALIZE END-OF-FILE-INDICATOR.
             MOVE ZERO TO WS-IO-CODE.
             MOVE LOW-VALUES TO INFORCE-FILE-DCB.
             INITIALIZE INFORCE-FILE-AREA.
             INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
        * GET CURRENT DATE
             ACCEPT WS-CURR-DATE FROM DATE.
             MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
             MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
             MOVE 20          TO WS-CURR-CONV-CC.
             MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
             CALL 'CKDCEXIN'
                 USING WS-CURR-CONV-DATE
                       WS-INT-CURR-DATE.
        1099-INITIALIZATION-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                         OPEN ALL FILES                          *
        ********************************************************************
        
        1100-OPEN-FILES.
        * OPEN OUTPUT REPORT FILE
             OPEN OUTPUT REPORT-FILE.
             MOVE '6' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
                        INFORCE-FILE-AREA
                        INFORCE-FILE-LENGTH
                        INF-RECORD-KEY
                        INFORCE-VSAMX-INFO.
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'OPEN INFORCE FAILED'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
             MOVE '6' TO WS-IO-CODE.
             CALL 'CKSDT1IO'
                  USING WS-IO-CODE
                        INFORCE-AUX-SDT.
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
             MOVE '4' TO WS-IO-CODE.
             CALL 'CKETRLST'
                  USING WS-IO-CODE
                        WS-DUMMY
                        INFORCE-FILE-DCB
                        WS-DUMMY
                        WS-DUMMY
             IF WS-IO-CODE NOT EQUAL '0'
                 DISPLAY 'ERROR IN CKETRLST'
                 DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                 GO TO EOJ9900-ABEND
             END-IF.
        1199-OPEN-FILES-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                        MAIN PROCESS                             *
        ********************************************************************
        
        2000-MAIN-PROCESS.
             PERFORM 2100-READ-NEXT-POLICY
                 THRU 2199-READ-NEXT-POLICY-EXIT.
             IF NOT END-OF-FILE
                 PERFORM 2200-PROCESS-POLICY
                     THRU 2299-PROCESS-POLICY-EXIT
             END-IF.
        2000-MAIN-PROCESS-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                    READ NEXT POLICY                             *
        ********************************************************************
        
        2100-READ-NEXT-POLICY.
             MOVE '8' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
                        INFORCE-FILE-AREA
                        INFORCE-FILE-LENGTH
                        INF-RECORD-KEY
                        INFORCE-VSAMX-INFO.
             IF WS-IO-CODE = '6'
                 SET END-OF-FILE TO TRUE
             ELSE
                 IF WS-IO-CODE NOT = '0'
                     DISPLAY 'READ INFORCE FAILED'
                     DISPLAY 'WS-IO-CODE=' WS-IO-CODE
                     GO TO EOJ9900-ABEND
                 END-IF
             END-IF.
        2199-READ-NEXT-POLICY-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                     PROCESS POLICY                              *
        ********************************************************************
        
         2200-PROCESS-POLICY.
             MOVE 'N' TO VUL18-IND
             MOVE 'N' TO ACTIVE-22-IND
        * CHECK FOR VUL18 PRODUCT
             IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
                 SET VUL18-PRODUCT TO TRUE
             END-IF
        * CHECK FOR ACTIVE STATUS 22
             IF VUL18-PRODUCT
                 IF CURR-STAT OF CV-SEGMENT = '22'
                     SET ACTIVE-22-FOUND TO TRUE
                 END-IF
             END-IF
        * IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
             IF VUL18-PRODUCT AND ACTIVE-22-FOUND
                 PERFORM 2300-BUILD-REPORT-RECORD
                     THRU 2399-BUILD-REPORT-RECORD-EXIT
                 PERFORM 2400-WRITE-REPORT-RECORD
                     THRU 2499-WRITE-REPORT-RECORD-EXIT
             END-IF
        2299-PROCESS-POLICY-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                  BUILD REPORT RECORD                            *
        ********************************************************************
        
        2300-BUILD-REPORT-RECORD.
        * MOVE POLICY NUMBER
             MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
        * MOVE POLICY STATE
             MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
        * MOVE STATUS
             MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
        * MOVE STATUS DESCRIPTION
             EVALUATE CURR-STAT OF CV-SEGMENT
                 WHEN '22'
                     MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
                 WHEN OTHER
                     MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
             END-EVALUATE
        * MOVE ISSUE DATE
             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
             MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
        * MOVE PRODUCT NAME
             MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
        * MOVE OWNER NAME
             MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
        * MOVE OWNER ADDRESS
             STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    CITY OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    STATE OF AU-SEGMENT DELIMITED BY SPACE
                    ' ' DELIMITED BY SIZE
                    ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
                    INTO RP-OWNER-ADDRESS
        * MOVE PLAN CODE
             MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
        * MOVE ISSUE STATE
             MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
        * CALCULATE AND MOVE ISSUE AGE
             PERFORM 2500-CALCULATE-ISSUE-AGE
                 THRU 2599-CALCULATE-ISSUE-AGE-EXIT
             MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
        2399-BUILD-REPORT-RECORD-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                 WRITE REPORT RECORD                            *
        ********************************************************************
        
        2400-WRITE-REPORT-RECORD.
             WRITE REPORT-REC FROM RP-RECORD
             IF NOT WRITE-OK
                 DISPLAY 'WRITE ERROR ON REPORT FILE'
                 GO TO EOJ9900-ABEND
             END-IF
             ADD 1 TO WS-REC-WRITTEN-CNTR
        2499-WRITE-REPORT-RECORD-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *               CALCULATE ISSUE AGE                               *
        ********************************************************************
        
        2500-CALCULATE-ISSUE-AGE.
        * CONVERT ISSUE DATE TO INTERNAL FORMAT
             MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
             CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
        * CALCULATE AGE DIFFERENCE
             CALL 'CKDCARTH' USING WS-INT-CURR-DATE
                           WS-INT-ISSU-DATE
                           WS-DCARTH-DIFFERENCE
                           WS-ISSUE-AGE
        2599-CALCULATE-ISSUE-AGE-EXIT.
             EXIT.
        EJECT
        ********************************************************************
        *                        CLOSE FILES                              *
        ********************************************************************
        
         EOJ9000-CLOSE-FILES.
        * CLOSE REPORT FILE
             CLOSE REPORT-FILE
        * CLOSE INFORCE FILE
             MOVE '5' TO WS-IO-CODE.
             CALL 'CKVSAMIO'
                  USING INFORCE-VSAM
                        WS-IO-CODE
        * DISPLAY COUNTERS
             DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
             DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
             GO TO EOJ9999-EXIT.
        EOJ9900-ABEND.
             DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
        EOJ9999-EXIT.
             EXIT.
 
