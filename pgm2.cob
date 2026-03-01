IDENTIFICATION DIVISION.
PROGRAM-ID. NYVULFP.
AUTHOR. BHARATH CHEVIREDDY.
DATE-WRITTEN. 22/2021.
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
*  FILEPASS IS NEEDED FOR RITM0393587/TASK0499302. THIS FILEPASS  *
*  IS REQUESTED BY ANDREW ASTORE TO DETERMINE ALL RMD POLICIES     *
*  ELIGIBLE ALONG WITH PLAN CODES PREMIER, PREMIER PLUS,           *
*  PREMIER PLUS II AND PREMIER PLUS.                               *
*  FILEPASS REPORT.                                                *
*                                                                  *
*J    JCL..                                                        *
*                                                                  *
* //NYFPVUL EXEC PGM=NYFPVUL                                       *
* //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
* //SYSOUT   DD SYSOUT=*                                           *
* //PVERESPL DD DISP=SHR,DSN=P54.CK.PVERESP.CK858A(0)             *
* //         DD DISP=SHR,DSN=P54.CK.PVERESP.CK858B(0)             *
* //         DD DISP=SHR,DSN=P54.CK.PVERESP.CK858F(0)             *
* //RPTOUT   DD DSN=T54.T9511F0.NYFPVUL.OUTPUT.DATA,              *
* //            DISP=(,CATLG,CATLG),                               *
* //            UNIT=USER,                                         *
* //            SPACE=(CYL,(50,30),RLSE),                          *
* //            DCB=(RECFM=FB,LRECL=250,BLKSIZE=0)                 *
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
*//                                                              
********************************************************************

EJECT
WORKING-STORAGE SECTION.
01  FILLER PIC X(32)
    VALUE 'NYFPVUL WORKING STORAGE BEGINS '.
********************************************************************

*    DATA AREAS
********************************************************************
COPY CKRECMAX.
EJECT
********************************************************************
*    READ ONLY CONSTANTS
********************************************************************
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
***NYRBNFOR TABLE AREA
    05 END-OF-FILE-INDICATOR PIC X(1).
       88 END-OF-FILE VALUE 'Y'.

    05 OWNER-AGE-IND PIC X(1).
       88 OWNER-FOUND VALUE 'Y'.

    05 WS-RECORD-IND PIC X(1).
       88 INFORCE-RECORD VALUE 'I'.

    05 CONTINUE-PROCESSING-INDICATOR PIC X(1).
       88 CONTINUE-PROCESSING VALUE 'Y'.
05 CONTINUE-PROCESSING-INDICATOR        PIC X(1).
   88 CONTINUE-PROCESSING               VALUE 'Y'.
   88 END-OF-PROCESSING                 VALUE 'N'.

05 WS-WRITE-RECORD-INDICATOR            PIC X(1).
   88 WRITE-RECORD-YES                  VALUE 'Y'.
   88 WRITE-RECORD-NO                   VALUE 'N'.

05 DESTINY-POLICY-IND                   PIC X(1).
   88 DESTINY-POLICY-VALID              VALUE 'Y'.
   88 DESTINY-POLICY-NOT-VALID          VALUE 'N'.

05 ACTV-AGNT-IND                        PIC X(1).
   88 ACTV-AGNT-FOUND                   VALUE 'Y'.
   88 ACTV-AGNT-NOT-FOUND               VALUE 'N'.

05 GP-BEGIN-DATE-IND                    PIC X(1).
   88 GP-BEGIN-DATE-FOUND               VALUE 'Y'.
   88 GP-BEGIN-DATE-NOT-FOUND           VALUE 'N'.

05 DIFF-DATE-IND                        PIC X(1).
   88 DIFF-DATE-FOUND                   VALUE 'Y'.
   88 DIFF-DATE-NOT-FOUND               VALUE 'N'.

05 GP-END-DATE-IND                      PIC X(1).
   88 LAPSE-DATE-FOUND                  VALUE 'Y'.
   88 LAPSE-DATE-NOT-FOUND              VALUE 'N'.

05 GROSS-LOAN-IND                       PIC X(1).
   88 GROSS-LOAN-FOUND                  VALUE 'Y'.
   88 GROSS-LOAN-NOT-FOUND              VALUE 'N'.

05 CHECK-TRANS-TYPE                     PIC X(02).
   88 PRTL-WTHDRL VALUES
      'SH','SN','SG','SM',
      'SA','SV','SW','ST'.

05 PRTL-WDRWL-IND                       PIC X(01).
   88 PRTL-WTHDRL-FOUND                 VALUE 'Y'.
   88 PRTL-WTHDRL-NOT-FOUND             VALUE 'N'.

05 END-OF-FILE-PVE-INDICATOR            PIC X(1) VALUE 'Y'.
   88 END-OF-FILE-PVE.

05 TD-TRANS-TYPE-IND                    PIC X(1).
   88 TD-TRANS-TYPE-FOUND               VALUE 'Y'.
   88 TD-TRANS-TYPE-NOT-FOUND           VALUE 'N'.

05 WS-WRITE-RECORD                      PIC X(1) VALUE 'N'.
   88 DONT-WRITE-RECORD.
   88 WRITE-RECORD                      VALUE 'Y'.

05 END-OF-TABLE-INDICATOR               PIC X(1).
   88 END-OF-TABLE                      VALUE 'Y'.
   88 CONTINUE-LOADING                  VALUE 'N'.

05 WSP-RIDER-IND                        PIC X(1).
   88 WSP-IND-FOUND                     VALUE 'Y'.
   88 WSP-IND-NOT-FOUND                 VALUE 'N'.

05 WS-FILE-IND                          PIC X(1).
   88 INFORCE-FILE                      VALUE 'I'.
   88 HISTORY-FILE                      VALUE 'H'.

05 WS-SAVE-PLAN                         PIC X(11).
05 WS-TEMP-POLICY                       PIC X(10).

05 CHECK-TRANS-OUT                      PIC X(02).
   88 FULL-SURRENDER                    VALUES 'SF'.
   88 TERM-DEATH                        VALUES 'TD'.
   88 INIT-PAYMENT                      VALUES 'PI','PF'.
   88 ALL-PREM-PAYMENT                  VALUES
      'PI','PF','PA','PR',
      'PT','PU','PB'.
   88 ADDTL-PAYMENT                     VALUES 'PA'.
   88 REG-PAYMENT                       VALUES 'PR'.
   88 REINST-PAYMENT                    VALUES 'PU','PB'.
   88 ALL-WITHDRAWAL                    VALUES
      'SG','SN','SM','SH',
      'SF','SQ'.
   88 ALL-LOANS                         VALUES 'LN','LM','LG'.

05 SURR-DEATH-TERM-IND                  PIC X(01).
   88 IS-SURRENDER                      VALUES 'S'.
   88 IS-TERM-DEATH                     VALUES 'T'.
   88 UNKNOWN                           VALUES ' '.

05 WS-AGENT-IDENT                       PIC X(10).
   88 VALID-AGENT                       VALUE '125647'.

* 05 POLICY-FOUND-IND                   PIC X(01).
*    88 POLICY-FOUND                    VALUES 'Y'.
*    88 POLICY-NOT-FOUND                VALUES 'N'.

05 CHECK-CURR-STATE                     PIC X(02).

* 05 PROCESS-POLICY-IND                 PIC X(01).
*    88 PROCESS-POLICY                  VALUES 'Y'.

05 PROCESS-POLICY-IND                   PIC X(01).
   88 PROCESS-POLICY                    VALUES 'Y'.
   88 DO-NOT-PROCESS                    VALUES 'N'.

05 PROCESS-POLICY-IND                   PIC X(01).
   88 WRITE-RECORD                      VALUES 'Y'.
   88 DONT-WRITE-RECORD                 VALUES 'N'.

* I-O READ ONLY DATA
05 WS-IO-CODE                           PIC X(1).
   88 OPEN-INFORCE-FOR-UPDATE           VALUE '1'.
   88 CLOSE-INFORCE-FILE                VALUE '5'.
   88 SETL-INFORCE-FILE                 VALUE '7'.
   88 READ-INFORCE-FOR-UPDATE           VALUE '8'.
   88 REWRITE-INFORCE-FILE              VALUE '4'.
   88 INFORCE-IO-COMPLETED              VALUE '0'.
   88 INFORCE-IO-EOF                    VALUE '6'.
   88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
                                                '7' THRU '9'.

* INFORCE READ ONLY DATA
05 INFORCE-FILE-LENGTH                  COMP SYNC PIC S9(4) VALUE +12.
05 INF-RECORD-KEY.
   10 INFORCE-KEY-FILE-CODE             PIC X.
   10 INFORCE-KEY-USER-ID               PIC X.
   10 INFORCE-KEY-POL-NUM               PIC X(10).

05 INFORCE-BASIC-LENGTH                 COMP SYNC PIC S9(4).
05 INFORCE-RECSIZE                      COMP PIC S9(8) VALUE +65000.
05 FILLER REDEFINES INFORCE-RECSIZE.
   10 FILLER                            PIC X(2).
   10 INFORCE-PRMAX                     COMP PIC 9(4).
05 INFORCE-MAX-SEGS                     COMP PIC S9(4) VALUE +4000.

* HISTORY READ ONLY DATA
05 SDT-H-TABLE-NAME                     PIC X(08) VALUE 'CKESDTBH'.
05 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) VALUE +20.
05 INFORCE-VSAMX-INFO.
   10 FILLER                            PIC X(7) VALUE 'FVDUNLD'.
   10 FILLER                            PIC X VALUE LOW-VALUE.
   10 FILLER                            PIC X VALUE ' '.
   10 FILLER                            PIC X(08) VALUE LOW-VALUES.

10 FILLER              PIC X      VALUE LOW-VALUE.
10 FILLER              PIC X      VALUE ' '.
10 FILLER              PIC X(08)  VALUE LOW-VALUES.

05 HISTORY-VSAMX-INFO.
   10 FILLER           PIC X(7)   VALUE 'FVDULHD'.
   10 FILLER           PIC X      VALUE LOW-VALUE.
   10 FILLER           PIC X      VALUE ' '.
   10 FILLER           PIC X(11)  VALUE LOW-VALUES.

EJECT

01 LOAD-PREM-GROSS-AMNT.
   05 WS-GROSS-AMNT-REC OCCURS 10 TIMES.
      10 WS-GROSS-AMNT  PIC S9(9)V99 COMP-3.

EJECT

05 INFORCE-VSAMX-INFO.
   10 FILLER            PIC X(7)   VALUE 'FVDUNLD'.
   10 FILLER            PIC X      VALUE LOW-VALUE.
   10 FILLER            PIC X      VALUE ' '.
   10 FILLER            PIC X(08)  VALUE LOW-VALUES.

05 HISTORY-VSAMX-INFO.
   10 FILLER            PIC X(7)   VALUE 'FVDULHD'.
   10 FILLER            PIC X      VALUE LOW-VALUE.
   10 FILLER            PIC X      VALUE ' '.
   10 FILLER            PIC X(11)  VALUE LOW-VALUES.

EJECT

01 LOAD-PREM-GROSS-AMNT.
   05 WS-GROSS-AMNT-REC OCCURS 10 TIMES.
      10 WS-GROSS-AMNT  PIC S9(9)V99 COMP-3.

EJECT

01 LOAD-PLAN-DESC-TABLE.
   05 WS-PLANDESC-REC OCCURS 600 TIMES.
      10 WS-PLAN-NAME   PIC X(11).
      10 WS-PLAN-DESC   PIC X(78).
      10 FILLER         PIC X(14).

EJECT

01 CKUDT103-ENTRY.
   05 CKUDT103-ALPHA-STATE   PIC X(02).
   05 CKUDT103-NUMERIC-STATE PIC 9(02).

EJECT

01 TABLE-PARMS.
   COPY CKTBPARM.

EJECT

01 CKUDT103-TABLE-ENTRY.
   COPY CKUDT103.

EJECT

********************************************************************
*                V A R I A B L E   D A T A   A R E A S             *
********************************************************************

01 WS-PVERESP-EXTRACT.
   05 PVE-CONTRACT-ID      PIC X(08).
   05 FILLER               PIC X(17).
   05 FILLER               PIC X(19).
   05 PVE-CALC-CD          PIC X(03).
PJTEST
   05 PVE-CALC-NUM REDEFINES PVE-CALC-CD PIC 9(03).
PJTEST
   05 FILLER               PIC X(03).
   05 PVE-CALC-VALUE       PIC --------9.99.
   05 FILLER               PIC X(02).
   05 PVE-AS-OF-DATE.
      10 PVE-AS-OF-MONTH   PIC X(02) VALUE SPACES.
      10 SLASH-1           PIC X(01) VALUE '/'.
      10 PVE-AS-OF-DAY     PIC X(02) VALUE SPACES.
      10 SLASH-2           PIC X(01) VALUE '/'.
      10 PVE-AS-OF-YEAR    PIC X(04) VALUE SPACES.
NPVEAD
   05 FILLER               PIC X(13).

********************************************************************
*                V A R I A B L E   D A T A   A R E A S             *
********************************************************************

01 VARIABLE-WORK-AREA.
   05 RECORD-LENGTH        PIC S9(8) COMP.
   05 HISTORY-FILE-LENGTH  COMP PIC S9(4).
   05 WS-HIGH-DURATION     COMP-3 PIC S9(5) VALUE +0.
   05 WS-LOW-DURATION      COMP-3 PIC S9(5) VALUE +0.
   05 WS-HISTORY-RCD-CNT   COMP-3 PIC S9(3) VALUE +0.

   05 WS-MULTIPLE-DURATION-IND PIC X(01).
      88 CHECK-MULTIPLE-DURATION-RECS VALUE 'Y'.

   05 WS-AGT-KEY.
      10 WS-AGT-KEY-REC-ID PIC X(01).
      10 WS-AGT-KEY-USER   PIC X(01).
      10 WS-AGT-KEY-AGENT  PIC X(10).

   05 WS-ACF-LENGTH-PARAM  PIC S9(4) COMP.
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
   05 WS-TOTAL-LOAN        PIC S9(09)V99 COMP-3 VALUE +0.
   05 WS-TOTAL-PREM        PIC S9(11)V99 COMP-3.
   05 WS-CASH-VALUE        PIC S9(9)V99.
   05 WS-PVE-CALC-VALUE    PIC S9(12)V99 COMP-3.
   05 WS-AD-PLAN           PIC X(05).
   05 WS-CTR               PIC S9(5) COMP-3.
   05 WS-49CTR             PIC S9(5) COMP-3.
   05 WS-ISSUE-STATE       PIC X(02).
   05 WS-OWNER-STATE       PIC X(02).
   05 WS-90SEG-ID          PIC X(90).
   05 WS-ADDR              PIC X(30).
   05 WS-CITY              PIC X(02).
   05 WS-STATE             PIC X(13).
   05 WS-ZIP-CODE          PIC X(03).
   05 WS-PLAN-BASE         PIC X(02).
   05 WS-AGT-ID            COMP-3 PIC S9(5).
   05 WS-AGT-SEQ           PIC X(01).
   05 WS-PLAN-TYPE         PIC X(01).
05 WS-AGT-SEQ                COMP-3 PIC S9(5).
05 WS-PLAN-TYPE              PIC X(01).
05 WS-AGENT-ID               PIC X(10).
05 WS-AGENT-NAME             PIC X(30).
05 WS-AGENT-FRST-NAME        PIC X(30).
05 WS-AGENT-LAST-NAME        PIC X(30).
05 WS-AGENT-STATUS           PIC X(10).
05 WS-AGENT-COMM             PIC 9(04).
05 WS-SEG-SEQ                COMP-3 PIC S9(5).
05 WS-89SEG-ID               PIC X(02).
05 WS-89SEG-SEQ              COMP-3 PIC S9(5).
05 WS-90SEG-SEQ              COMP-3 PIC S9(5).
05 WS-ACF-IO-BYTE            PIC X.
05 WS-RETURN-CODE            PIC X.
05 WS-FACE-AMT               PIC S9(11)V99 COMP-3.
05 WS-GROSS-DIST-AMT         PIC S9(12)V99 COMP-3.
05 WS-AGENT-SEG-ID           PIC X(02).
05 WS-AGENT-SEG-SEQ          PIC S9(3) COMP-3.
05 WS-CURR-TYPE              PIC X(02).
05 WS-ISSU-STATE             PIC X(02).

05 WS-SEG-WORK-AREA          PIC X(25000) VALUE SPACE.

05 WS-TABLE-KEY.
   10 WS-TABLE-PLAN          PIC X(10).
   10 WS-TABLE-OPTION        PIC X(01).
   10 FILLER                 PIC X(18) VALUE LOW-VALUES.

05 WS-TRANS-PLAN             PIC X(11).

05 WS-TABLE-OUTPUT.
   10 FILLER                 PIC X(11).
   10 WS-TABLE-PLAN-DESC     PIC X(40).

05 TB1-SEG69-TABLE.
   10 TB1-SEG69-REC OCCURS 20 TIMES.
      15 TB1-TRANS-TYPE      PIC X(2).
      15 TB1-ASOF-DATE       COMP-3 PIC S9(3).
         20 TB1-ASOF-YEAR    PIC S9(3).
         20 TB1-ASOF-DAY     PIC S9(3).
      15 TB1-GROSS-AMT       PIC 9(09)V99 VALUE ZERO.

05 TB1-CTR-SAVE              PIC S9(3) COMP-3.
05 TB1-CTR                   PIC S9(3) COMP-3.

* DATA AREA
05 AGENT-SUB                 PIC S9(4) COMP.
05 WS-ASOF-TD-DATE           PIC X(04).
05 WS-EXT-ASOF-DATE          PIC X(08).
05 WS-EXT-ENTRY-DATE         PIC X(08).

05 WS-EXT-CEASE-DATE         PIC X(08) VALUE SPACE.

05 WS-APP-DATE-IN.
   10 WS-APP-DT-IN-YYYY      PIC S9(3) COMP-3.
   10 WS-APP-DT-IN-DD        PIC S9(3) COMP-3.

CBR1
05 WS-RIDER-ISSU-DATE-IN.
CBR1
   10 WS-RIDR-ISSU-YR        PIC S9(3) COMP-3.
CBR1
   10 WS-RIDR-ISSU-DAY       PIC S9(3) COMP-3.

CBR1
05 WS-RIDER-CEAS-DATE-IN.
CBR1
   10 WS-RIDR-CEAS-YR        PIC S9(3) COMP-3.
CBR1
   10 WS-RIDR-CEAS-DAY       PIC S9(3) COMP-3.

05 WS-INT-WORK-DATE.
   10 WS-INT-WORK-YR         PIC S9(3) COMP-3.
   10 WS-INT-WORK-DAY        PIC S9(3) COMP-3.

05 WS-WORK-CHECK-DATE.
   10 WS-WORK-CHECK-MMYY     PIC S9(3) COMP-3.
   10 WS-WORK-CHECK-DD       PIC S9(3) COMP-3.

05 WS-ASOF1-DATE.
   10 WS-ASOF1-MMYY          PIC S9(3) COMP-3.
   10 WS-ASOF1-DD            PIC S9(3) COMP-3.

05 WS-ASOF2-DATE.
   10 WS-ASOF2-MMYY          PIC S9(3) COMP-3.
   10 WS-ASOF2-DD            PIC S9(3) COMP-3.

05 WS-BIRTH-DATE.
   10 WS-BRTH-MM             PIC X(02).
   10 WS-BRTH-DD             PIC X(02).
   10 WS-BRTH-YYYY           PIC X(04).

05 WS-ASOF-DATE-EXT.
   10 WS-ASOF-MONTH          PIC X(02).
   10 WS-ASOF-DAY            PIC X(02).
   10 WS-ASOF-YEAR           PIC X(04).

05 WS-APP-FORMAT-DATE.
   10 WS-APP-FORMAT-MM       PIC X(02) VALUE SPACE.
   10 WS-APP-FORMAT-DD       PIC X(02) VALUE SPACE.
   10 WS-APP-FORMAT-YYYY     PIC X(04) VALUE SPACE.

05 WS-EFFCTV-FORMT-DATE.
   10 WS-EFFCTV-FORMT-MM     PIC X(02) VALUE SPACE.
   10 WS-EFFCTV-FORMT-DD     PIC X(02) VALUE SPACE.
   10 WS-EFFCTV-FORMT-YYYY   PIC X(04) VALUE SPACE.

05 WS-CHECK-EFFCTV-FORMT-DATE.
   10 WS-CHECK-EFFCTV-FORMT-MM PIC X(02) VALUE SPACE.
   10 FILLER                    PIC X(01) VALUE '/'.
   10 WS-CHECK-EFFCTV-FORMT-DD PIC X(02) VALUE SPACE.
   10 FILLER                    PIC X(01) VALUE '/'.
   10 WS-CHECK-EFFCTV-FORMT-YYYY PIC X(04) VALUE SPACE.

05 WS-ISSU-DATE.
   10 WS-ISSU-MM              PIC X(02) VALUE SPACE.
   10 WS-ISSU-DD              PIC X(02) VALUE SPACE.
   10 WS-ISSU-YYYY            PIC X(04) VALUE SPACE.

05 WS-TOTAL-NET-AMOUNT        PIC S9(09)V99.
05 WS-PREM-TOTAL              PIC S9(11)V99 COMP-3.
05 WS-STORE-NET-AMNT          PIC S9(09)V99 COMP-3.
05 WS-STORE-LOAN-AMNT         PIC S9(09)V99 COMP-3.
05 WS-NET-LOAN-AMNT           PIC S9(9)V99 COMP-3.
05 WS-BONUS-AMT               PIC S9(9)V99 COMP-3.
05 WS-NET-LOAN-AMNT        PIC S9(09)V99 COMP-3.
05 WS-BONUS-AMT            PIC S9(9)V99 COMP-3.
05 WS-GROSS-AMT            PIC S9(9)V99 COMP-3.
05 WS-ADDL-1035-AMT        PIC S9(9)V99 COMP-3.
05 WS-ADDL-NRML-AMT        PIC S9(9)V99 COMP-3.
05 WS-ADDL-ROLL-AMT        PIC S9(9)V99 COMP-3.
05 WS-ADDL-TRST-AMT        PIC S9(9)V99 COMP-3.
05 WS-NET-AMT-ED           PIC ZZZZZZZZ9.99.
05 WS-SURR-CHARGES-ED      PIC ZZZZZZ9.99.
05 WS-ASOF-SAVE-DATE       PIC X(04).
05 WS-PRI-OWNER-ID         PIC X(03).

05 WS-WORK-DATE.
   10 WS-WORK-MM           PIC 9(02).
   10 WS-WORK-DD           PIC 9(02).
   10 WS-WORK-YY           PIC 9(04).

05 WS-WORK-DATE2.
   10 WS-WORK2-MM          PIC 9(02).
   10 WS-WORK2-DD          PIC 9(02).
   10 WS-WORK2-YY          PIC 9(04).

05 WS-QUAL-TYPE            PIC X(01).

* DATE AREA
05 WS-BEGDATE-EXT          PIC X(08).
05 WS-BEGDATE-CF           COMP-3.
   10 WS-BEGDATE-DATE      PIC S9(5).
   10 WS-BEGDATE-DAY       PIC S9(3).

05 WS-ENDDATE-EXT          PIC X(08).
05 WS-ENDDATE-CF           COMP-3.
   10 WS-ENDDATE-DATE      PIC S9(5).
   10 WS-ENDDATE-DAY       PIC S9(3).

05 WS-CURR-DATE.
   10 WS-CURR-YEAR         PIC 9(02).
   10 WS-CURR-MO           PIC 9(02).
   10 WS-CURR-DAY          PIC 9(02).

05 WS-CURR-CONV-DATE.
   10 WS-CURR-CONV-MM      PIC 9(02).
   10 WS-CURR-CONV-DD      PIC 9(02).
   10 WS-CURR-CONV-CC      PIC 9(02).
   10 WS-CURR-CONV-YY      PIC 9(02).

05 WS-INT-CURR-DATE        COMP-3.
   10 WS-INT-YEAR          PIC S9(03).
   10 WS-INT-DAY           PIC S9(03).

05 WS-INT-BEGIN-DATE       COMP-3.
   10 WS-INT-BEGIN-YEAR    PIC S9(3) VALUE +0.
   10 WS-INT-INTERNAL-DAY  PIC S9(3) VALUE +0.

05 WS-INTER-BDATE          PIC X(04).

05 WS-INT-END-DATE         COMP-3.
   10 WS-INT-END-YEAR      PIC S9(3) VALUE +0.
   10 WS-INT-END-DAY       PIC S9(3) VALUE +0.

05 WS-INTER-EDATE          PIC X(04).

05 WS-ASOF-EXT-DATE.
   10 WS-ASOF-EXT-MM       PIC X(02) VALUE SPACE.
   10 WS-ASOF-EXT-DD       PIC X(02) VALUE SPACE.
   10 WS-ASOF-EXT-YEAR     PIC X(04) VALUE SPACE.

05 WS-INT-DATE.
   10 WS-INT-YYYY          PIC S9(3) COMP-3.
   10 WS-INT-DD            PIC S9(3) COMP-3.

05 WS-INT-ISSU-DATE.
   10 WS-INT-ISSU-MMYY     PIC S9(3) COMP-3.
   10 WS-INT-ISSU-DD       PIC S9(3) COMP-3.

05 WS-FORMAT-DATE.
   10 WS-FORMAT-MM         PIC X(02) VALUE SPACE.
   10 WS-FORMAT-DD         PIC X(02) VALUE SPACE.
   10 WS-FORMAT-YYYY       PIC X(04) VALUE SPACE.

05 WS-GP-BEGIN-DATE.
   10 WS-GP-BEGIN-MM       PIC X(02) VALUE SPACE.
   10 WS-GP-BEGIN-DD       PIC X(02) VALUE SPACE.
   10 WS-GP-BEGIN-YYYY     PIC X(04) VALUE SPACE.

05 WS-GP-END-DATE.
   10 WS-GP-END-MM         PIC X(02) VALUE SPACE.
   10 WS-GP-END-DD         PIC X(02) VALUE SPACE.
   10 WS-GP-END-YYYY       PIC X(04) VALUE SPACE.

05 WS-GRACE-RESPONSE-DATE.
   10 WS-RESP-MM           PIC X(02) VALUE SPACE.
   10 WS-RESP-DD           PIC X(02) VALUE SPACE.
   10 WS-RESP-YYYY         PIC X(04) VALUE SPACE.

05 WS-LAPSE-DATE.
   10 WS-LAPSE-MM          PIC X(02) VALUE SPACE.
   10 WS-LAPSE-DD          PIC X(02) VALUE SPACE.
   10 WS-LAPSE-YYYY        PIC X(04) VALUE SPACE.

CBR2
05 WS-EXT-CEAS-DATE        PIC X(08) VALUE SPACE.
CBR2
05 WS-EXT-DOD-DATE         PIC X(08) VALUE SPACE.

05 WS-INT-GP-WORK-DATE     COMP-3.
   10 WS-GP-WORK-MMYY      PIC S9(3).
   10 WS-GP-WORK-DD        PIC S9(3).

05 WS-EFF-DATE             COMP-3.
   10 WS-EFF-MMYY          PIC S9(3).
   10 WS-EFF-DD            PIC S9(3).

05 WS-FORMAT2-DATE.
   10 WS-FORMAT2-MM        PIC X(02) VALUE SPACE.
   10 WS-FORMAT2-DD        PIC X(02) VALUE SPACE.
   10 WS-FORMAT2-YYYY      PIC X(04) VALUE SPACE.

05 WS-IPP-CEASE-DATE.
   10 WS-IPP-CEASE-MM      PIC X(02) VALUE SPACE.
   10 WS-IPP-CEASE-DD      PIC X(02) VALUE SPACE.
   10 WS-IPP-CEASE-YYYY    PIC X(04) VALUE SPACE.

05 WS-FORMAT-RMD-DATE.
   10 WS-FORMAT-RMD-MM     PIC X(02) VALUE SPACE.
   10 WS-FORMAT-RMD-DD     PIC X(02) VALUE SPACE.
   10 WS-FORMAT-RMD-YYYY   PIC X(04) VALUE SPACE.
10 WS-FORMAT-RMD-DD        PIC X(02) VALUE SPACE.
10 WS-FORMAT-RMD-YYYY      PIC X(04) VALUE SPACE.

05 WS-BEGIN-DATE-FORMAT    PIC X(10).
05 WS-END-DATE-FORMAT      PIC X(10).

05 WS-ISSUE-DATE.
   10 WS-ISSUE-MM          PIC X(02) VALUE SPACE.
   10 WS-ISSUE-DD          PIC X(02) VALUE SPACE.
   10 WS-ISSUE-YYYY        PIC X(04) VALUE SPACE.

05 WS-APPL-DATE.
   10 WS-APPL-MM           PIC X(02) VALUE SPACE.
   10 WS-APPL-DD           PIC X(02) VALUE SPACE.
   10 WS-APPL-YYYY         PIC X(04) VALUE SPACE.

05 WS-INT-ISSUE-DATE       COMP-3.
   10 WS-INT-ISSUE-YEAR    PIC S9(03).
   10 WS-INT-ISSUE-DAY     PIC S9(03).

05 WS-AWD-CEASE-DATE       COMP-3.
   10 WS-AWD-CEASE-MMYY    PIC S9(03).
   10 WS-AWD-CEASE-DD      PIC S9(03).

05 WS-EFFECTIVE-DATE       COMP-3.
   10 WS-EFFECT-YEAR       PIC S9(03).
   10 WS-EFFECT-DAY        PIC S9(03).

05 WS-CHECK-DATE           PIC X(08).

05 WS-CHECK-MONTHS         PIC S9(3) COMP-3.
05 WS-CHECK-DAYS           PIC S9(3) COMP-3.
05 WS-PD-SUB               PIC S9(04) COMP-3 VALUE 0.
05 WS-SUB                  PIC S9(3) COMP-3.
05 WS-SUB2                 PIC S9(3) COMP-3.
05 WS-POLICY-READ-CNT      PIC 9(09) VALUE ZERO.
05 WS-POLICY-READ-2        PIC 9(09) VALUE ZERO.
05 TERMINATED-COUNT        PIC 9(09) VALUE ZERO.
05 PASSED-TERMINATED-COUNT PIC 9(09) VALUE ZERO.
05 ACTIVE-COUNT            PIC 9(09) VALUE ZERO.

05 WS-PD-IND               PIC X(1).
   88 WS-PD-FOUND          VALUE 'Y'.
   88 WS-PD-NOT-FOUND      VALUE 'N'.

05 WS-CKDCARTH-CONTANTS.
   10 WS-DCARTH-MONTHS     PIC S9(3) COMP-3.
   10 WS-DCARTH-DAYS       PIC S9(3) COMP-3.
   10 WS-DCARTH-YEARS      PIC S9(3) COMP-3.
   10 WS-DCARTH-ADD        PIC X VALUE '0'.
   10 WS-DCARTH-SUBTRACTION PIC X VALUE '1'.
   10 WS-DCARTH-DIFFERENCE PIC X VALUE '2'.

NA336A
* COUNTERS
05 WS-WORK-COUNTERS.
   10 WS-READ-I-CNTR       PIC S9(9) COMP-3.
   10 WS-READ-H-CNTR       PIC S9(9) COMP-3.
   10 WS-FULLSURR-CNTR     PIC S9(9) COMP-3.
   10 WS-TERM-DEAD-CNTR    PIC S9(9) COMP-3.
   10 WS-PRTWITH-CNTR      PIC S9(9) COMP-3.
   10 WS-INIT-PAY-CNTR     PIC S9(9) COMP-3.
   10 WS-ADDTL-PAY-CNTR    PIC S9(9) COMP-3.
   10 WS-REG-PAY-CNTR      PIC S9(9) COMP-3.
   10 WS-REIN-PAY-CNTR     PIC S9(9) COMP-3.
   10 WS-LOAN-PAY-CNTR     PIC S9(9) COMP-3.
   10 WS-POL-FOUND-CNTR    PIC S9(9) COMP-3.
   10 WS-REC-WRITTEN-CNTR  PIC S9(9) COMP-3.
   10 WS-COUNTER-WITH      PIC S9(9) COMP-3.
   10 WS-COUNTER-SURR-CHG  PIC S9(9) COMP-3.

EJECT
********************************************************************
*      STRT      END
* INPUT DATE CARD AREA - DATE FORMAT MMDDYYYYMMDDYYYY
********************************************************************

01 WS-CARDIN-AREA.
   05 BEGIN-DATE           PIC X(08).
   05 END-DATE             PIC X(08).
EJECT

********************************************************************
* INFORCE RECORD CONTROL SECTION
********************************************************************

01 INFORCE-FILE-AREA.
   05 INFORCE-REC-LENGTH   PIC S9(4) COMP.
   05 INFORCE-FILE-KEY.
      10 INFORCE-REC-ID    PIC X(01).
      10 INFORCE-USER-ID   PIC X(1).
      10 INFORCE-POL-NUMBER PIC X(10).
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
* OUTPUT RECORD
********************************************************************

01 OFFLINE-RECORD           PIC X(400) VALUE SPACE.
EJECT

********************************************************************
* REPORT RECORD
********************************************************************

01 RP-RECORD.
   05 RP-POLICY             PIC X(10).
   05 FILLER                PIC X(01) VALUE X'05'.
   05 RP-POL-STATUS         PIC X(02).
   05 FILLER                PIC X(01) VALUE X'05'.
05 FILLER                   PIC X(01) VALUE X'05'.
05 RP-POL-STATUS            PIC X(02).
05 FILLER                   PIC X(01) VALUE X'05'.
05 RP-STATUS                PIC X(30).
05 FILLER                   PIC X(01) VALUE X'05'.
05 RP-PLAN-CODE             PIC X(11).
05 FILLER                   PIC X(01) VALUE X'05'.
05 RP-POLICY-DATE           PIC X(10).
05 FILLER                   PIC X(01) VALUE X'05'.
05 RP-ISSUE-DATE            PIC X(10).
05 FILLER                   PIC X(01) VALUE X'05'.

EJECT

01 WS-TRANS-AMOUNT          PIC 9(14)V9(02).
01 TRANS-STORE-GROSS        PIC S9(9)V99 COMP-3.
01 TRANS-STORE-WS-AMT       PIC S9(9)V99 COMP-3.
01 TRANS-STORE-WH-AMT       PIC S9(9)V99 COMP-3.
01 WS-TOT-LOAN-AMNT         PIC ZZZZZZZZ9.99.

EJECT
********************************************************************
*                       SEGMENT AREA PLUS                         *
********************************************************************

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
03 FILLER                    PIC X(64980).
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
      10 WS-ACF-LNGTH       PIC S9(04) COMP.
      10 WS-ACF-RECID       PIC X(01).
      10 WS-ACF-CNTRL       PIC X(01).
         15 WS-USER         PIC X(10).
         15 WS-ACF-AGT-NUMBER PIC X(10).
      10 WS-ACF-REST-OF-BASIC PIC X(75).
   05 WS-ACF-REST           PIC X(13911).

01 WS-ACF-DCB               PIC X(25000).

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

COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
                    INFORCE-AUX-SDT.
EJECT

* AUXSEGH RECORD AREA
COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
                    BY AUX-HIST-DCB.
EJECT

COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
                    HISTORY-AUX-SDT.
EJECT

********************************************************************
*                     BATCH  I/O  RECORD                          *
********************************************************************

COPY CKBCHCDS REPLACING
     BCHCODES-CALLING-CODES BY BCHCODES.

01 FILLER PIC X(32)
   VALUE 'NYF22766 WORKING STORAGE ENDS  '.
EJECT

LINKAGE SECTION.
VALUE 'NYF22766 WORKING STORAGE ENDS '.
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

    INITIALIZE END-OF-FILE-PVE-INDICATOR.

    MOVE ZERO TO WS-IO-CODE.
    MOVE ZERO TO WS-PD-SUB.
    MOVE LOW-VALUES TO INFORCE-FILE-DCB.
    INITIALIZE INFORCE-FILE-AREA.

    MOVE 'N' TO END-OF-TABLE-INDICATOR.
    INITIALIZE WS-WORK-COUNTERS.

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

* OPEN OUTPUT REPORT RECORD
PVFIL
    OPEN OUTPUT REPORT-FILE.

* OPEN AGENT CONTROL FILE

    MOVE '6' TO WS-ACF-IO-BYTE.
    CALL 'CKVSAMIO'
         USING ACF-FILE-NAME
               WS-ACF-IO-BYTE.

    IF WS-ACF-IO-BYTE IS NOT EQUAL TO ZERO
        DISPLAY 'AGENT CONTROL FILE OPEN ERROR'
        GO TO EOJ9900-ABEND
    END-IF.

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

    MOVE '6' TO WS-IO-CODE.
    CALL 'CKSDTXIO'
         USING WS-IO-CODE
               HISTORY-AUX-SDT
               SDT-H-TABLE-NAME.

    IF WS-IO-CODE NOT EQUAL '0'
        DISPLAY 'OPEN OF AUXSEGH FILE FAILED'
        DISPLAY 'WS-IO-CODE=' WS-IO-CODE
        GO TO EOJ9900-ABEND
    END-IF.

    MOVE '4' TO WS-IO-CODE.
    CALL 'CKETRLST'
         USING WS-IO-CODE
               WS-DUMMY
               INFORCE-FILE-DCB
               WS-DUMMY
               WS-DUMMY.
MOVE '4' TO WS-IO-CODE.
CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       HISTORY-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-HIST-DCB
                       HISTORY-AUX-SDT.

* OPEN HISTORY AND AUXSEGH FILES

MOVE HISTORY-VSAMX-INFO TO UBHIST-HISTORY-VSAMX.
MOVE '6' TO WS-IO-CODE.

CALL 'CKVSAMIO' USING HISTORY-VSAM
                       WS-IO-CODE
                       HISTORY-RECORD
                       HISTORY-FILE-LENGTH
                       NRCSRCHK-SEARCH-KEY
                       UBHIST-HISTORY-VSAMX.

IF WS-IO-CODE NOT EQUAL '0'
    DISPLAY 'OPEN HISTORY FAILED'
    DISPLAY 'WS-IO-CODE=' WS-IO-CODE
    GO TO EOJ9900-ABEND.
    
MOVE '6' TO WS-IO-CODE.

CALL 'CKSDTXIO' USING WS-IO-CODE
                       HISTORY-AUX-SDT
                       SDT-H-TABLE-NAME.

IF WS-IO-CODE NOT EQUAL '0'
    DISPLAY 'OPEN OF AUXSEGH FILE FAILED'
    DISPLAY 'WS-IO-CODE=' WS-IO-CODE
    GO TO EOJ9900-ABEND.

MOVE '4' TO WS-IO-CODE.

CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       HISTORY-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-HIST-DCB
                       HISTORY-AUX-SDT.

1199-OPEN-FILES-EXIT.
EXIT.
EJECT

********************************************************************
*                         MAIN PROCESS                            *
********************************************************************

2000-MAIN-PROCESS.

    PERFORM 2100-READ-INFORCE
        THRU 2199-READ-INFORCE-EXIT.

    IF END-OF-FILE
        GO TO 2000-MAIN-PROCESS-EXIT
    END-IF.

CONTINUE-RTN.

    INITIALIZE RP-RECORD.
    SET DONT-WRITE-RECORD TO TRUE.

    SET INFORCE-FILE TO TRUE.
    MOVE FRCCNTRT-CONTRACT-NUMBER TO RP-POLICY.

    PERFORM 3000-PROCESS-SEG-01
        THRU 3999-PROCESS-SEG-01-EXIT.

    PERFORM 3500-PROCESS-SEG-02
        THRU 3599-PROCESS-SEG-02-EXIT.

    PERFORM P200-PROCESS-SEG-52
        THRU P200-PROCESS-SEG-52-EXIT.

CBFIX
    INITIALIZE REPORT-REC
CBFIX
    WRITE REPORT-REC FROM RP-RECORD.

2000-MAIN-PROCESS-EXIT.
EXIT.
EJECT

********************************************************************
*T  THIS ROUTINE WILL READ THE INFORCE FILE AND DO A TRAILER
*T  LIST ON THE RECORD.
********************************************************************

2100-READ-INFORCE.

MOVE '4' TO WS-IO-CODE.

CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       INFORCE-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-INF-DCB.

MOVE '4' TO WS-IO-CODE.

CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       HISTORY-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-HIST-DCB
                       HISTORY-AUX-SDT.

MOVE '2' TO WS-IO-CODE.
CALL 'CKVSAMIO' USING INFORCE-VSAM
MOVE '2' TO WS-IO-CODE.
CALL 'CKVSAMIO' USING INFORCE-VSAM
                       WS-IO-CODE
                       INQUIRY-RECORD-CONTROL-SECTION
                       INFORCE-FILE-LENGTH
                       INF-RECORD-KEY
                       INFORCE-VSAMX-INFO.

IF WS-IO-CODE = '6'
   OR FRCCNTRT-CONTRACT-NUMBER EQUAL HIGH-VALUES
      SET END-OF-FILE        TO TRUE
      SET END-OF-PROCESSING  TO TRUE
      GO TO 2199-READ-INFORCE-EXIT.

IF FRCCNTRT-CONTRACT-NUMBER EQUAL 'A'
      GO TO 2100-READ-INFORCE.

IF WS-IO-CODE NOT EQUAL '0'
      DISPLAY 'INFORCE READ ERROR'
      DISPLAY 'WS-IO-CODE=' WS-IO-CODE
      GO TO EOJ9900-ABEND.

MOVE '4' TO WS-IO-CODE.
CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       INFORCE-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-INF-DCB.

MOVE '4' TO WS-IO-CODE.
CALL 'CKETRLST' USING WS-IO-CODE
                       WS-DUMMY
                       HISTORY-FILE-DCB
                       WS-DUMMY
                       WS-DUMMY
                       WS-DUMMY
                       AUX-HIST-DCB
                       HISTORY-AUX-SDT.

MOVE '0' TO WS-IO-CODE.
MOVE 'C' TO AUXEFLG1 OF AUX-INF-DCB.
MOVE 'S' TO AUXEFLG2 OF AUX-INF-DCB.
MOVE 'I' TO AUXEFLG3 OF AUX-INF-DCB.
MOVE 'F' TO AUXEFLG4 OF AUX-INF-DCB.
MOVE 'V' TO AUXEFLG6 OF AUX-INF-DCB.

MOVE AUX-INF-DCB TO INFORCE-FILE-AUXDCB.

MOVE LENGTH OF INQUIRY-RECORD-CONTROL-SECTION
        TO INFORCE-BASIC-LENGTH.

CALL 'CKETRLST' USING WS-IO-CODE
                       INQUIRY-RECORD-CONTROL-SECTION
                       INFORCE-FILE-DCB
                       INFORCE-BASIC-LENGTH
                       INFORCE-PRMAX
                       INFORCE-MAX-SEGS
                       INFORCE-FILE-AUXDCB
                       INFORCE-AUX-SDT.

IF WS-IO-CODE NOT EQUAL '0'
      DISPLAY 'ERROR IN SDT BUILD AUX1, RC = ' WS-IO-CODE
      DISPLAY 'POLICY NUMBER = ' FRCCNTRT-CONTRACT-NUMBER
      GO TO EOJ9900-ABEND
END-IF.

MOVE '0' TO WS-IO-CODE.
MOVE 'C' TO AUXEFLG1 OF AUX-HIST-DCB.
MOVE 'S' TO AUXEFLG2 OF AUX-HIST-DCB.
MOVE 'I' TO AUXEFLG3 OF AUX-HIST-DCB.
MOVE 'F' TO AUXEFLG4 OF AUX-HIST-DCB.
MOVE 'V' TO AUXEFLG6 OF AUX-HIST-DCB.

MOVE AUX-HIST-DCB TO HISTORY-FILE-AUXDCB.

CALL 'CKETRLST' USING WS-IO-CODE
                       HISTORY-RECORD
                       HISTORY-FILE-DCB
                       INFORCE-BASIC-LENGTH
                       INFORCE-PRMAX
                       INFORCE-MAX-SEGS
                       HISTORY-FILE-AUXDCB
                       HISTORY-AUX-SDT.

IF WS-IO-CODE NOT EQUAL '0'
      DISPLAY 'ERROR IN SDT BUILD AUXH, RC = ' WS-IO-CODE
      DISPLAY 'POLICY NUMBER = ' FRCCNTRT-CONTRACT-NUMBER
      GO TO EOJ9900-ABEND
END-IF.

2199-READ-INFORCE-EXIT.
EJECT

******************************************************************
*                       END OF JOB                              *
******************************************************************

EOJ9900-ABEND.
    CALL 'CKABEND'.
    EJECT

******************************************************************
*                       CLOSE FILES                             *
******************************************************************

EOJ9000-CLOSE-FILES.

* CLOSE OUTPUT REPORT RECORD
PVEFIL
    CLOSE REPORT-FILE.

* CLOSE INFORCE FILE
    MOVE '5' TO WS-IO-CODE.
    CALL 'CKVSAMIO' USING INFORCE-VSAM
                           WS-IO-CODE.

    IF WS-IO-CODE NOT EQUAL '0'
        DISPLAY 'CLOSE INFORCE FAILED'.
IF WS-IO-CODE NOT EQUAL '0'
    DISPLAY 'CLOSE INFORCE FAILED'
    DISPLAY 'WS-IO-CODE=' WS-IO-CODE
END-IF.

MOVE '5' TO WS-IO-CODE.
CALL 'CKSDT1IO' USING WS-IO-CODE
                       INFORCE-AUX-SDT.

DISPLAY ' !! PROGRAM COMPLETED SUCCESSFULLY !!'.
DISPLAY ' '.

EOJ9999-EXIT.
    EXIT.
    EJECT
******************************************************************
*T                 PROCESS POLICY BASIC SEGMENT                  *
******************************************************************

3000-PROCESS-SEG-01.

    MOVE +0  TO WS-SEG-SEQ.
    MOVE '01' TO WS-SEG-ID.

    PERFORM SUB7000-GET-TRLR
        THRU SUB7999-GET-TRLR-EXIT.

    IF WS-IO-CODE IS NOT EQUAL TO '0'
        GO TO 3999-PROCESS-SEG-01-EXIT
    END-IF.

    MOVE WS-SEG-WORK-AREA
        TO INQUIRY-BASIC-SECTION.

    MOVE FBRSTAT-STATUS
        TO RP-POL-STATUS.

    EVALUATE TRUE
        WHEN FBRSTAT-STATUS EQUAL '12'
            MOVE 'NEW BUSINESS NOT PAID' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '22'
            MOVE 'ACTIVE' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '32'
            MOVE 'DISABILITY OR COI' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '33'
            MOVE 'DISABILITY WAIVER' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '38'
            MOVE 'DEATH CLAIM PEND' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '95'
            MOVE 'POLICY RESCINDED' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '97'
            MOVE 'REINSTATEMENT PENDING' TO RP-STATUS

        WHEN FBRSTAT-STATUS EQUAL '98'
           OR FBRSTAT-STATUS EQUAL '99'
            MOVE 'TERMINATED' TO RP-STATUS

        WHEN OTHER
            MOVE FBRSTAT-STATUS TO RP-STATUS
    END-EVALUATE.

    IF FBRSTAT1-BYTE1 EQUAL '9'
        EVALUATE TRUE
            WHEN FBRLSTEN-LAST EQUAL 'L'
                MOVE 'DECEASED' TO RP-STATUS

            WHEN FBRLSTEN-LAST EQUAL 'Q'
                MOVE 'LAPSED' TO RP-STATUS

            WHEN FBRLSTEN-LAST EQUAL 'M'
                MOVE 'MATURED' TO RP-STATUS

            WHEN FBRLSTEN-LAST EQUAL 'X'
                MOVE 'FREE LOOK' TO RP-STATUS

            WHEN OTHER
                MOVE 'SURRENDERED' TO RP-STATUS
        END-EVALUATE
    END-IF.

3999-PROCESS-SEG-01-EXIT.
    EXIT.
    EJECT
******************************************************************
*             PROCESS POLICY COVERAGE SEGMENT                    *
******************************************************************

3500-PROCESS-SEG-02.

    MOVE +0  TO WS-SEG-SEQ WS-IO-CODE.
    MOVE '02' TO WS-SEG-ID.

3520-NEXT-SEG-02.

    PERFORM SUB7000-GET-TRLR
        THRU SUB7999-GET-TRLR-EXIT.

    IF WS-IO-CODE NOT EQUAL '0'
        GO TO 3599-PROCESS-SEG-02-EXIT
    END-IF.

    MOVE WS-SEG-WORK-AREA
        TO INQUIRY-COVERAGE-SEGMENT.

    IF NOT FCVPHASE-BASIC-COV-PHASE
        GO TO 3520-NEXT-SEG-02
    END-IF.

    IF FCVPDSKY-PLAN-DESC-SEARCH-KEY (1:5) = 'VUL18'
       OR FCVPDSKY-PLAN-DESC-SEARCH-KEY (1:5) = 'VUL22'
        CONTINUE
        MOVE FCVPDSKY-PLAN-DESC-SEARCH-KEY
            TO RP-PLAN-CODE
    ELSE
        GO TO 2000-MAIN-PROCESS
    END-IF.

    CALL 'CKDCCFEX'
         USING FCVISSSF-ISSUE-DATE-FIELDS
               WS-ISSUE-DATE.

    MOVE WS-ISSUE-MM   TO RP-POLICY-DATE (1:2).
    MOVE '/'           TO RP-POLICY-DATE (3:1).
    MOVE WS-ISSUE-DD   TO RP-POLICY-DATE (4:2).
    MOVE '/'           TO RP-POLICY-DATE (6:1).
    MOVE WS-ISSUE-YYYY TO RP-POLICY-DATE (7:4).

3599-PROCESS-SEG-02-EXIT.
    EXIT.
    EJECT
******************************************************************
*T                GET USER INFO – SEGMENT 52                     *
******************************************************************

P200-PROCESS-SEG-52.

    MOVE +0  TO WS-SEG-SEQ.
    MOVE '52' TO WS-SEG-ID.
    PERFORM SUB7000-GET-TRLR
        THRU SUB7999-GET-TRLR-EXIT.

    IF WS-IO-CODE IS NOT EQUAL TO '0'
        GO TO P200-PROCESS-SEG-52-EXIT
    END-IF.

    MOVE WS-SEG-WORK-AREA
        TO USER-SEGMENT.

    CALL 'CKDCINEX'
         USING FUZINFDT-INFORCE-DATE
               WS-ISSUE-DATE.

    MOVE WS-ISSUE-MM   TO RP-ISSUE-DATE (1:2).
    MOVE '/'           TO RP-ISSUE-DATE (3:1).
    MOVE WS-ISSUE-DD   TO RP-ISSUE-DATE (4:2).
    MOVE '/'           TO RP-ISSUE-DATE (6:1).
    MOVE WS-ISSUE-YYYY TO RP-ISSUE-DATE (7:4).

P200-PROCESS-SEG-52-EXIT.
    EXIT.
******************************************************************
*            THIS ROUTINE WILL GET A SEGMENT                     *
******************************************************************

SUB7000-GET-TRLR.

    ADD +1 TO WS-SEG-SEQ.
    INITIALIZE WS-SEG-WORK-AREA.

    IF HISTORY-FILE
        CALL 'CKETRGET' USING WS-SEG-ID
                               WS-SEG-SEQ
                               WS-SEG-WORK-AREA
                               WS-DUMMY
                               WS-IO-CODE
                               WS-DUMMY
                               HISTORY-FILE-DCB
    END-IF.

    IF INFORCE-FILE
        CALL 'CKETRGET' USING WS-SEG-ID
                               WS-SEG-SEQ
                               WS-SEG-WORK-AREA
                               WS-DUMMY
                               WS-IO-CODE
                               WS-DUMMY
                               INFORCE-FILE-DCB
    END-IF.

SUB7999-GET-TRLR-EXIT.
    EXIT.
    EJECT
******************************************************************
*         THIS ROUTINE WILL GET THE 89 SEGMENT                   *
******************************************************************

SUB7000-GET-89TRLR.

    ADD +1 TO WS-89SEG-SEQ.

    CALL 'CKETRGET' USING WS-89SEG-ID
                           WS-89SEG-SEQ
                           INQUIRY-LIFE-CNTL-SEGMENT
                           WS-DUMMY
                           WS-IO-CODE
                           WS-DUMMY
                           INFORCE-FILE-DCB.

SUB7999-GET-89TRLR-EXIT.
    EXIT.
    EJECT
******************************************************************
*                     BATCH IO ROUTINE                           *
******************************************************************

SUB4000-IOROUTINE.

    CALL 'CKBATCHC' USING BCHCODES-CALLING-FILE
                           BCHCODES-CALLING-RETURN
                           OFFLINE-RECORD
                           RECORD-LENGTH.

    IF BCHCODES-CALLING-RETURN NOT EQUAL
       BCHNORM-NORMAL-RETURN
       AND BCHEOF-END-OF-FILE

        DISPLAY MSG01-IO-ERROR
                SPACE
                BCHCODES-CALLING-FILE
                SPACE
                BCHCODES-CALLING-RETURN

        CALL 'CKCOBCLS'
        CALL 'CKABEND'
    END-IF.

SUB4999-IO-ROUTINE-EXIT.
    EXIT.
    EJECT
******************************************************************
*                        READ ACF FILE                            *
******************************************************************

2200-READ-ACF.

    SET ACTV-AGNT-NOT-FOUND TO TRUE.
    INITIALIZE WS-AGENT-STATUS.
    MOVE SPACE TO WS-AGENT-NAME.

    MOVE 'E'  TO WS-AGT-KEY-REC-ID.
    MOVE +0   TO WS-ACF-LENGTH-PARAM.
    MOVE HEX-00 TO WS-AGT-KEY-USER.
    MOVE WS-AGENT-ID TO WS-AGT-KEY-AGENT.

    MOVE '2' TO WS-ACF-IO-BYTE.

    CALL 'CKVSAMIO' USING ACF-FILE-NAME
                           WS-ACF-IO-BYTE
                           WS-ACF-AREA
                           WS-ACF-LENGTH-PARAM
                           WS-AGT-KEY.

    IF WS-ACF-IO-BYTE IS NOT EQUAL TO '0'
        DISPLAY 'ERROR IN READING ACF FILE, RC : '
                WS-ACF-IO-BYTE
                ' FOR AGENT : '
                WS-AGENT-ID
        GO TO 2200-READ-ACF-EXIT
    END-IF.

    CALL 'CKETRLST' USING WS-ACF-IO-BYTE
                           WS-ACF-AREA
                           WS-ACF-DCB
                           CONSTANT-89.

    CALL 'CKETRLST' USING WS-ACF-IO-BYTE
                           WS-ACF-AREA
                           WS-ACF-DCB
                           CONSTANT-89
                           AGTMAX-RECORD
                           AGTTRLR-TRAILERS.

    IF WS-ACF-IO-BYTE IS NOT EQUAL TO '0'
        DISPLAY 'AGENT ETRLST FAILED ' RP-POLICY ' '
                WS-AGENT-ID
        GO TO 2200-READ-ACF-EXIT
    END-IF.

    MOVE WS-ACF-BASIC-SECTION
        TO AGENTS-BASIC-SECTION.

    IF GBSASACT-ACTIVE
        SET ACTV-AGNT-FOUND TO TRUE
        MOVE 'ACTIVE' TO WS-AGENT-STATUS
    ELSE
        SET ACTV-AGNT-NOT-FOUND TO TRUE
        MOVE 'TERMINATED' TO WS-AGENT-STATUS
    END-IF.

    MOVE '10' TO WS-AGT-ID.
    MOVE +1   TO WS-AGT-SEQ.

    CALL 'CKETRGET' USING WS-AGT-ID
                           WS-AGT-SEQ
                           WS-SEG-WORK-AREA
                           WS-DUMMY
                           WS-IO-CODE
                           WS-DUMMY
                           WS-ACF-DCB.

    MOVE WS-SEG-WORK-AREA
        TO AGENT-NAME-SEGMENT.

    IF GNMSSID-SS-TID-TYPE = 'C'
        MOVE GNMCORP-CORPORATION
            TO WS-AGENT-NAME
    ELSE
        STRING GNMLNAME-LAST-NAME DELIMITED BY ' '
               ','
               GNMFNAME-FIRST-NAME DELIMITED BY ' '
          INTO WS-AGENT-NAME
    END-IF.

    INITIALIZE WS-AGENT-FRST-NAME
               WS-AGENT-LAST-NAME.

    MOVE GNMFNAME-FIRST-NAME
        TO WS-AGENT-FRST-NAME.

    MOVE GNMLNAME-LAST-NAME
        TO WS-AGENT-LAST-NAME.

2200-READ-ACF-EXIT.
    EXIT.

PLANCD*************************************************************
PLANCD*             DECODED PLAN CODE PROCESS                    *
PLANCD*************************************************************

PLANCD*7100-DEC-PLAN-PROCESS.

PLANCD*    INITIALIZE TABLE-PARMS
PLANCD*               RP-PRODUCT-NAME.

PLANCD*    MOVE 'NYUTBSPL'
PLANCD*         TO TBPTBNAM-TABLE-NAME.

PLANCD*    MOVE X'00'
PLANCD*         TO TBPUSRID-USER-ID.

PLANCD*    SET TBPQKE-EXACT-MATCH
PLANCD*         TO TRUE.

PLANCD*    MOVE WS-TRANS-PLAN
PLANCD*         TO WS-TABLE-PLAN.

PLANCD*    MOVE FULPLOPT-PLAN-OPTION
PLANCD*         TO WS-TABLE-OPTION.

PLANCD*    MOVE +11
PLANCD*         TO TBPKEYLN-KEY-LENGTH.

PLANCD*    MOVE ZERO
PLANCD*         TO TBPKEYPO-KEY-POSITION.

PLANCD*    CALL 'CKTBLOCT'
PLANCD*         USING TABLE-PARMS
PLANCD*               WS-TABLE-KEY
PLANCD*               WS-TABLE-OUTPUT.

PLANCD*    IF TBPRNORM-EXACT-MATCH-FOUND
PLANCD*        MOVE WS-TABLE-PLAN-DESC
PLANCD*             TO RP-PRODUCT-NAME
PLANCD*    ELSE
PLANCD*        MOVE WS-TABLE-PLAN
PLANCD*             TO RP-PRODUCT-NAME
PLANCD*    END-IF.

PLANCD*7199-DEC-PLAN-EXIT.
PLANCD*    EXIT.
    EJECT