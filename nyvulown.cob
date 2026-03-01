IDENTIFICATION DIVISION.
       PROGRAM-ID. NYVULOWN.
       AUTHOR. BHARATH CHEVIREDDY.
       DATE-WRITTEN. 03/2025.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OWNER-REPORT-FILE ASSIGN TO RPTOWN.
       DATA DIVISION.
       FILE SECTION.
       FD  OWNER-REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OWNER-REPORT-REC        PIC X(400).

**********************************************************************
*                                                                  *
*A    ABSTRACT..                                                   *
*  FILEPASS IS NEEDED TO EXTRACT OWNER NAMES FROM VUL SEGMENT 18    *
*  FOR ACTIVE PAID POLICIES (STATUS 22). THIS PROGRAM READS        *
*  OWNER INFORMATION AND CREATES A REPORT WITH OWNER DETAILS.      *
*                                                                  *
*J    JCL..                                                        *
*                                                                  *
* //NYVULOWN EXEC PGM=NYVULOWN                                     *
* //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
* //SYSOUT   DD SYSOUT=*                                           *
* //RPTOWN   DD DSN=T54.T9511F0.NYVULOWN.OUTPUT.DATA,             *
* //            DISP=(,CATLG,CATLG),                              *
* //            UNIT=USER,                                         *
* //            SPACE=(CYL,(50,30),RLSE),                         *
* //            DCB=(RECFM=FB,LRECL=400,BLKSIZE=0)                *
* //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                  *
* //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                 *
* //SYSIPT   DD DUMMY                                             *
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
*     CKSDTXIO ---- AUXH SEGMENT TABLE INTERFACE                   *
*     CKUBHSIO ---- HISTORY FILE I/O                               *
*     CKSETADR ---- SET ADDRESS                                    *
*     CKCOBCRD ---- PRINT ELEMENT                                  *
*     CKDCEXIN ---- DATE CONVERSION                                *
*     CKBITBYT ---- BIT TO BYTE CONVERSION                         *
*     CKDCINEX ---- DATE CONVERSION                                *
*     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
*                                                                  *
*U    USER CONSTANTS AND TABLES REFERENCED..                       *
*     NONE                                                         *
*                                                                  *
**********************************************************************

       EJECT
       WORKING-STORAGE SECTION.
       01  FILLER PIC X(32)
           VALUE 'NYVULOWN WORKING STORAGE BEGINS '.
**********************************************************************
*                         DATA AREAS
**********************************************************************

       COPY CKRECMAX.
       EJECT

**********************************************************************
*                     READ ONLY CONSTANTS
**********************************************************************

       01  READ-ONLY-WORK-AREA.
           05 HWORD              COMP PIC S9(04) VALUE +7.
           05 WS-DUMMY           PIC X VALUE SPACE.
           05 MISC-CONSTANTS.
              10 CONSTANT-89     PIC S9(4) VALUE +089 COMP.
              10 WS-HEX-VALUES   PIC S9(13) COMP-3
                                 VALUE +0001020304050.
              10 WS-HEX-TABLE REDEFINES WS-HEX-VALUES.
                 15 HEX-00       PIC X.
                 15 HEX-01       PIC X.
                 15 HEX-02       PIC X.
                 15 HEX-03       PIC X.
                 15 HEX-04       PIC X.
                 15 HEX-05       PIC X.
                 15 FILLER       PIC X.
           05 WS-OTHER-DUMMY     PIC X VALUE SPACE.
           05 BINARY1            COMP PIC S9(04) VALUE +1.

           05 INFORCE-VSAM       PIC X(8) VALUE 'VSAM2'.
           05 HISTORY-VSAM       PIC X(8) VALUE 'VSAMX'.
           05 ACF-FILE-NAME      PIC X(8) VALUE 'VSAM3'.
           05 MSG01-IO-ERROR     PIC X(19)
                                 VALUE 'I/O ERROR ON FILE -'.

* SWITCHES AREA
       05 END-OF-FILE-INDICATOR PIC X(1).
          88 END-OF-FILE VALUE 'Y'.

       05 OWNER-FOUND-IND      PIC X(1).
          88 OWNER-FOUND       VALUE 'Y'.
          88 OWNER-NOT-FOUND   VALUE 'N'.

       05 ACTIVE-PAID-IND      PIC X(1).
          88 ACTIVE-PAID-FOUND VALUE 'Y'.
          88 ACTIVE-PAID-NOT-FOUND VALUE 'N'.

       05 WS-RECORD-IND        PIC X(1).
          88 INFORCE-RECORD    VALUE 'I'.

       05 CONTINUE-PROCESSING-INDICATOR PIC X(1).
          88 CONTINUE-PROCESSING VALUE 'Y'.
          88 END-OF-PROCESSING   VALUE 'N'.

       05 WS-WRITE-RECORD-INDICATOR PIC X(1).
          88 WRITE-RECORD-YES       VALUE 'Y'.
          88 WRITE-RECORD-NO        VALUE 'N'.

       05 WS-FILE-IND          PIC X(1).
          88 INFORCE-FILE      VALUE 'I'.
          88 HISTORY-FILE      VALUE 'H'.

* I-O READ ONLY DATA
       05 WS-IO-CODE           PIC X(1).
          88 OPEN-INFORCE-FOR-UPDATE    VALUE '1'.
          88 CLOSE-INFORCE-FILE         VALUE '5'.
          88 SETL-INFORCE-FILE          VALUE '7'.
          88 READ-INFORCE-FOR-UPDATE    VALUE '8'.
          88 REWRITE-INFORCE-FILE     VALUE '4'.
          88 INFORCE-IO-COMPLETED     VALUE '0'.
          88 INFORCE-IO-EOF           VALUE '6'.
          88 INFORCE-IO-FAILED        VALUE '1' THRU '5'
                                            '7' THRU '9'.

* INFORCE READ ONLY DATA
       05 INFORCE-FILE-LENGTH   COMP SYNC PIC S9(4) VALUE +12.
       05 INF-RECORD-KEY.
          10 INFORCE-KEY-FILE-CODE    PIC X.
          10 INFORCE-KEY-USER-ID      PIC X.
          10 INFORCE-KEY-POL-NUM      PIC X(10).

       05 INFORCE-BASIC-LENGTH   COMP SYNC PIC S9(4).
       05 INFORCE-RECSIZE         COMP PIC S9(8) VALUE +65000.
       05 FILLER REDEFINES INFORCE-RECSIZE.
          10 FILLER               PIC X(2).
          10 INFORCE-PRMAX        COMP PIC 9(4).
       05 INFORCE-MAX-SEGS        COMP PIC S9(4) VALUE +4000.

* HISTORY READ ONLY DATA
       05 SDT-H-TABLE-NAME      PIC X(08) VALUE 'CKESDTBH'.
       05 HISTORY-REC-CTL-LEN    COMP PIC S9(4) VALUE +20.

       05 INFORCE-VSAMX-INFO.
          10 FILLER              PIC X(7) VALUE 'FVDUNLD'.
          10 FILLER              PIC X VALUE LOW-VALUE.
          10 FILLER              PIC X VALUE ' '.
          10 FILLER              PIC X(08) VALUE LOW-VALUES.

       05 HISTORY-VSAMX-INFO.
          10 FILLER              PIC X(7) VALUE 'FVDULHD'.
          10 FILLER              PIC X VALUE LOW-VALUE.
          10 FILLER              PIC X VALUE ' '.
          10 FILLER              PIC X(11) VALUE LOW-VALUES.

       EJECT

**********************************************************************
*                V A R I A B L E   D A T A   A R E A S             *
**********************************************************************

       01 VARIABLE-WORK-AREA.
          05 RECORD-LENGTH       PIC S9(8) COMP.
          05 HISTORY-FILE-LENGTH COMP PIC S9(4).
          05 WS-HIGH-DURATION     COMP-3 PIC S9(5) VALUE +0.
          05 WS-LOW-DURATION      COMP-3 PIC S9(5) VALUE +0.
          05 WS-HISTORY-RCD-CNT   COMP-3 PIC S9(3) VALUE +0.

          05 WS-AGT-KEY.
             10 WS-AGT-KEY-REC-ID PIC X(01).
             10 WS-AGT-KEY-USER   PIC X(01).
             10 WS-AGT-KEY-AGENT  PIC X(10).

          05 WS-ACF-LENGTH-PARAM PIC S9(4) COMP.
          05 WS-OCTR              PIC S9(03) COMP-3.
          05 WS-69CTR             PIC S9(05) COMP-3.
          05 TL-CTR               PIC S9(05) COMP-3.
          05 WS-ISSUE-AGE         PIC S9(03) COMP-3.
          05 WS-COMP-VAR55        PIC S9(03) COMP-3.
          05 WS-ERROR-MSG         PIC X(50).
          05 COMP-ISSUE-AGE       PIC S9(03) COMP-3.
          05 WS-SUB               PIC S9(04) COMP-3 VALUE 0.
          05 WS-SUB-SAVE          PIC S9(04) COMP-3 VALUE 0.
          05 WS-SEG-ID            PIC X(02).
          05 WS-STATUS            PIC X(02).
          05 WS-FLCSEQ-SEQUENCE   PIC X(01).

          05  WS-OWNER-NAME        PIC X(40).
          05  WS-POLICY-NUMBER     PIC X(10).
          05  WS-POLICY-STATUS     PIC X(02).

          05 WS-SEG-WORK-AREA     PIC X(25000) VALUE SPACE.

* COUNTERS
          05 WS-WORK-COUNTERS.
             10 WS-READ-I-CNTR    PIC S9(9) COMP-3.
             10 WS-READ-H-CNTR    PIC S9(9) COMP-3.
             10 WS-POL-FOUND-CNTR PIC S9(9) COMP-3.
             10 WS-REC-WRITTEN-CNTR PIC S9(9) COMP-3.
             10 WS-OWNER-FOUND-CNTR PIC S9(9) COMP-3.

       EJECT

**********************************************************************
*            INFORCE RECORD CONTROL SECTION                         *
**********************************************************************

       01 INFORCE-FILE-AREA.
          05 INFORCE-REC-LENGTH  PIC S9(4) COMP.
          05 INFORCE-FILE-KEY.
             10 INFORCE-REC-ID   PIC X(01).
             10 INFORCE-USER-ID  PIC X(1).
             10 INFORCE-POL-NUMBER PIC X(10).
          05 INFORCE-IO-STAT     PIC X(01).
          05 FILLER               PIC X(64985).
       EJECT

       01 INFORCE-FILE-DCB.
          COPY CKDCBMAX.
       EJECT

       01 INFORCE-FILE-AUXDCB      PIC X(25000).
       01 HISTORY-FILE-AUXDCB      PIC X(25000).
       EJECT

**********************************************************************
*            HISTORY RECORD CONTROL SECTION                         *
**********************************************************************

       COPY CKNRECRC.
       05 FILLER                   PIC X(13980).
       EJECT

       01 HISTORY-FILE-DCB.
          COPY CKDCBLRG.
          COPY CKUBGPRM.
       EJECT

**********************************************************************
*                    OUTPUT RECORD                                 *
**********************************************************************

       01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
       EJECT

**********************************************************************
*                    OWNER REPORT RECORD                           *
**********************************************************************

       01  OWNER-REPORT-RECORD.
           05  OR-POLICY-NUMBER     PIC X(10).
           05  FILLER               PIC X(01) VALUE X'05'.
           05  OR-OWNER-NAME        PIC X(40).
           05  FILLER               PIC X(349) VALUE SPACES.

       EJECT

**********************************************************************
*                       SEGMENT AREA PLUS                         *
**********************************************************************

       COPY CKFRECCV.
       EJECT
       COPY CKFRECUB.
       EJECT
       COPY CKFRECSB.
       EJECT
       COPY CKFRECAU.
       EJECT
       COPY CKFRECUM.
       EJECT
       COPY CKFRECRC.
       03 FILLER                 PIC X(64980).
       EJECT
       COPY CKULGPRM.
       EJECT
       COPY CKULAFLG REPLACING ACTIVITY-FLAGS BY
                             ACTIVITY-SEG-FLAG-BYTES.
       EJECT

       01 AGENT-SEGMENT.
          COPY CKFRECAG.
       EJECT

       01 AGENTS-BASIC-SECTION.
          COPY CKGRECBS.
       EJECT

       01 AGENT-NAME-SEGMENT.
          COPY CKGRECNM.
       EJECT

       01 WS-ACF-AREA.
          05 WS-ACF-BASIC-SECTION.
             10 WS-ACF-LNGTH      PIC S9(04) COMP.
             10 WS-ACF-RECID      PIC X(01).
             10 WS-ACF-CNTRL      PIC X(01).
                15 WS-USER        PIC X(10).
                15 WS-ACF-AGT-NUMBER PIC X(10).
             10 WS-ACF-REST-OF-BASIC PIC X(75).
          05 WS-ACF-REST          PIC X(13911).

       01 WS-ACF-DCB              PIC X(25000).

       01 AGENT-CONTRACT-INFO-SEGMENT.
          COPY CKGRECCI.
       EJECT

       01 CALENDAR-YEAR-SEGMENT.
          COPY CKFRECCY.
       EJECT

       01 LOAN-FLAG-BYTES.
          COPY CKFLGFUB.

       01 USER-SEGMENT.
          COPY CKFRECUZ.
       EJECT

       01 INQUIRY-LIFE-CNTL-SEGMENT.
          COPY CKFRECLC.
       EJECT
       COPY CKFRECUR.
       EJECT
       COPY CKFRECNA.
       EJECT

       01 AP-CONTROL-FLAG-BYTES.
          COPY CKFULFLG.
       EJECT
       COPY CKFRECUL.
       EJECT
       COPY CKFRECUY.
       EJECT
       COPY CKFRECUA.
       EJECT
       COPY CKFRECBR.
       EJECT
       COPY CKFRECUV.
       EJECT

* AUXSEG1 RECORD AREA
       COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
                         BY AUX-INF-DCB.
       EJECT

       COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE
                         BY INFORCE-AUX-SDT.
       EJECT

* AUXSEGH RECORD AREA
       COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
                         BY AUX-HIST-DCB.
       EJECT

       COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE
                         BY HISTORY-AUX-SDT.
       EJECT

**********************************************************************
*                     BATCH  I/O  RECORD                          *
**********************************************************************

       COPY CKBCHCDS REPLACING
           BATCH-IO-RECORD BY BATCH-IO-RECORD.
       EJECT

**********************************************************************
*                        PROCEDURE DIVISION                        *
**********************************************************************

       PROCEDURE DIVISION.
           PERFORM 000-MAINLINE
           GOBACK.

**********************************************************************
*                   000-MAINLINE                                    *
**********************************************************************

       000-MAINLINE.
           PERFORM 100-INITIALIZATION
           PERFORM 200-PROCESS-MAINLINE
             UNTIL END-OF-FILE
           PERFORM 300-TERMINATION
           .

**********************************************************************
*                  100-INITIALIZATION                               *
**********************************************************************

       100-INITIALIZATION.
           INITIALIZE VARIABLE-WORK-AREA
           INITIALIZE WS-WORK-COUNTERS
           SET CONTINUE-PROCESSING TO TRUE
           SET END-OF-FILE TO FALSE
           PERFORM 110-OPEN-FILES
           .

       110-OPEN-FILES.
           OPEN OUTPUT OWNER-REPORT-FILE
           IF OWNER-REPORT-FILE-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING OWNER REPORT FILE: '
                      OWNER-REPORT-FILE-STATUS
              PERFORM 999-ABEND
           END-IF
           .

**********************************************************************
*                  200-PROCESS-MAINLINE                             *
**********************************************************************

       200-PROCESS-MAINLINE.
           PERFORM 210-READ-INFORCE
           IF NOT END-OF-FILE
              PERFORM 220-PROCESS-POLICY
           END-IF
           .

       210-READ-INFORCE.
           MOVE '1' TO WS-IO-CODE
           CALL 'CKVSAMIO' USING WS-IO-CODE
                                INFORCE-VSAM
                                INFORCE-FILE-AREA
                                INFORCE-FILE-DCB
                                INFORCE-FILE-AUXDCB
           IF INFORCE-IO-COMPLETED
              ADD 1 TO WS-READ-I-CNTR
           ELSE
              IF INFORCE-IO-EOF
                 SET END-OF-FILE TO TRUE
              ELSE
                 DISPLAY 'VSAM READ ERROR: ' INFORCE-IO-STAT
                 PERFORM 999-ABEND
              END-IF
           END-IF
           .

       220-PROCESS-POLICY.
           SET INFORCE-RECORD TO TRUE
           MOVE INFORCE-POL-NUMBER TO WS-POLICY-NUMBER
           
           PERFORM 230-CHECK-POLICY-STATUS
           IF ACTIVE-PAID-FOUND
              PERFORM 240-EXTRACT-OWNER-DATA
              IF OWNER-FOUND
                 PERFORM 250-WRITE-OWNER-RECORD
              END-IF
           END-IF
           .

       230-CHECK-POLICY-STATUS.
           MOVE SPACES TO WS-POLICY-STATUS
           PERFORM 231-GET-POLICY-STATUS
           
           IF WS-POLICY-STATUS = '22'
              SET ACTIVE-PAID-FOUND TO TRUE
           ELSE
              SET ACTIVE-PAID-NOT-FOUND TO TRUE
           END-IF
           .

       231-GET-POLICY-STATUS.
*          THIS PARAGRAPH WOULD CONTAIN LOGIC TO EXTRACT
*          POLICY STATUS FROM APPROPRIATE SEGMENT
*          FOR NOW, WE'LL USE A PLACEHOLDER
           MOVE '22' TO WS-POLICY-STATUS
           .

       240-EXTRACT-OWNER-DATA.
           SET OWNER-NOT-FOUND TO TRUE
           PERFORM 241-SEARCH-OWNER-SEGMENT
              VARYING WS-SUB FROM 1 BY 1
              UNTIL WS-SUB > INFORCE-PRMAX
                 OR OWNER-FOUND
           .

       241-SEARCH-OWNER-SEGMENT.
           MOVE WS-SUB TO WS-SEG-SEQ
           MOVE '18' TO WS-SEG-ID
           
           PERFORM 242-READ-SEGMENT
           IF SEGMENT-FOUND
              PERFORM 243-EXTRACT-OWNER-FIELDS
           END-IF
           .

       242-READ-SEGMENT.
*          LOGIC TO READ SPECIFIC SEGMENT
*          THIS WOULD USE CKSDT1IO OR SIMILAR INTERFACE
           SET SEGMENT-FOUND TO TRUE
           .

       243-EXTRACT-OWNER-FIELDS.
*          EXTRACT OWNER NAME FROM VUL SEGMENT 18
           MOVE SPACES TO WS-OWNER-NAME
           
*          PLACEHOLDER LOGIC - REPLACE WITH ACTUAL FIELD EXTRACTION
           MOVE 'OWNER NAME' TO WS-OWNER-NAME
           
           SET OWNER-FOUND TO TRUE
           ADD 1 TO WS-OWNER-FOUND-CNTR
           .

       250-WRITE-OWNER-RECORD.
           INITIALIZE OWNER-REPORT-RECORD
           MOVE WS-POLICY-NUMBER TO OR-POLICY-NUMBER
           MOVE WS-OWNER-NAME    TO OR-OWNER-NAME
           
           WRITE OWNER-REPORT-REC FROM OWNER-REPORT-RECORD
           IF OWNER-REPORT-FILE-STATUS NOT = '00'
              DISPLAY 'ERROR WRITING OWNER RECORD: '
                      OWNER-REPORT-FILE-STATUS
              PERFORM 999-ABEND
           END-IF
           
           ADD 1 TO WS-REC-WRITTEN-CNTR
           .

**********************************************************************
*                  300-TERMINATION                                  *
**********************************************************************

       300-TERMINATION.
           PERFORM 310-CLOSE-FILES
           PERFORM 320-DISPLAY-COUNTERS
           .

       310-CLOSE-FILES.
           CLOSE OWNER-REPORT-FILE
           IF OWNER-REPORT-FILE-STATUS NOT = '00'
              DISPLAY 'ERROR CLOSING OWNER REPORT FILE: '
                      OWNER-REPORT-FILE-STATUS
           END-IF
           .

       320-DISPLAY-COUNTERS.
           DISPLAY 'NYVULOWN PROCESSING COMPLETE'
           DISPLAY 'INFORCE RECORDS READ:   ' WS-READ-I-CNTR
           DISPLAY 'POLICIES PROCESSED:     ' WS-POL-FOUND-CNTR
           DISPLAY 'OWNERS FOUND:           ' WS-OWNER-FOUND-CNTR
           DISPLAY 'RECORDS WRITTEN:        ' WS-REC-WRITTEN-CNTR
           .

**********************************************************************
*                  999-ABEND                                        *
**********************************************************************

       999-ABEND.
           DISPLAY 'PROGRAM ABENDING - NYVULOWN'
           CALL 'CKABEND'
           .
