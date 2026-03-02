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
25*J    JCL..                                                        *
26*                                                                  *
27* //NYFPOWFF EXEC PGM=NYFPOWFF                                     *
28* //SYSPDUMP DD SYSOUT=U,HOLD=YES                                  *
29* //SYSOUT   DD SYSOUT=*                                           *
3* //RPTOUT   DD DSN=T54.T9511F.NYFPOWFF.OUTPUT.DATA,             *
31* //            DISP=(,CATLG,CATLG),                               *
32* //            UNIT=USER,                                         *
33* //            SPACE=(CYL,(5,3),RLSE),                          *
34* //            DCB=(RECFM=FB,LRECL=4,BLKSIZE=)                 *
35* //VSAM2    DD DISP=SHR,DSN=P54.CK.BASEB.POLICY                   *
36* //FVDSEG1  DD DISP=SHR,DSN=P54.CK.BASEB.AUXSEG1                  *
37* //SYSIPT   DD DUMMY                                              *
38* //*                                                               *
39*                                                                  *
4*P    ENTRY PARAMETERS..                                           *
41*     NONE.                                                        *
42*                                                                  *
43*E    ERRORS DETECTED BY THIS ELEMENT..                            *
44*     I/O ERROR ON FILES                                           *
45*                                                                  *
46*C    ELEMENTS INVOKED BY THIS ELEMENT..                           *
47*                                                                  *
48*     CKVSAMIO ---- VSAM I/O INTERFACE                             *
49*     CKABEND  ---- FORCE A PROGRAM INTERUPT                       *
5*     CKETRLST ---- TRAILER LIST ELEMENT                           *
51*     CKETRGET ---- TRAILER GET ELEMENT                            *
52*     CKSDT1IO ---- AUX SEGMENT TABLE INTERFACE                    *
53*     CKSETADR ---- SET ADDRESS                                    *
54*     CKCOBCRD ---- PRINT ELEMENT                                  *
55*     CKDCEXIN ---- DATE CONVERSION                                *
56*     CKDCINEX ---- DATE CONVERSION                                *
57*     CKBATCHC ---- SEQUENTIAL FILE I/O                            *
58*                                                                  *
59*U    USER CONSTANTS AND TABLES REFERENCED..                       *
6*     NONE                                                         *
61*                                                                  *
62********************************************************************
63 EJECT
64 WORKING-STORAGE SECTION.
65 1  FILLER PIC X(32)
66     VALUE 'NYFPOWFF WORKING STORAGE BEGINS '.
67********************************************************************
68*    DATA AREAS
69********************************************************************
COPY CKRECMAX.
71 EJECT
72********************************************************************
73*    READ ONLY CONSTANTS
74********************************************************************
75 01  READ-ONLY-WORK-AREA.
76     05 HWORD              COMP PIC S9(04) VALUE +7.
77     05 WS-DUMMY           PIC X VALUE SPACE.
78     05 BINARY1            COMP PIC S9(04) VALUE +1.
79     05 INFORCE-VSAM       PIC X(08) VALUE 'VSAM2'.
8     05 MSG01-IO-ERROR     PIC X(19)
81                           VALUE 'I/O ERROR ON FILE -'.
82* SWITCHES AREA
83     05 END-OF-FILE-INDICATOR PIC X(01).
84        88 END-OF-FILE VALUE 'Y'.
85        88 CONTINUE-PROCESSING VALUE 'Y'.
86     05 VUL18-IND           PIC X(01).
87        88 VUL18-PRODUCT     VALUE 'Y'.
88        88 VUL18-NOT-PRODUCT VALUE 'N'.
89     5 ACTIVE-22-IND       PIC X(1).
9        88 ACTIVE-22-FOUND   VALUE 'Y'.
91        88 ACTIVE-22-NOT-FOUND VALUE 'N'.
92* I-O READ ONLY DATA
93 5 WS-IO-CODE                           PIC X(1).
94     88 INFORCE-IO-COMPLETED              VALUE '0'.
95     88 INFORCE-IO-EOF                    VALUE '6'.
96     88 INFORCE-IO-FAILED                 VALUE '1' THRU '5',
97                                                 '7' THRU '9'.
98* INFORCE READ ONLY DATA
99 5 INFORCE-FILE-LENGTH                  COMP SYNC PIC S9(4) VALUE +12.
1 5 INF-RECORD-KEY.
11     1 INFORCE-KEY-FILE-CODE             PIC X.
12     1 INFORCE-KEY-USER-ID               PIC X.
13     1 INFORCE-KEY-POL-NUM               PIC X(1).
14 5 INFORCE-BASIC-LENGTH                 COMP SYNC PIC S9(4).
15 5 INFORCE-RECSIZE                      COMP PIC S9(8) VALUE +65.
16 5 FILLER REDEFINES INFORCE-RECSIZE.
17     1 FILLER                            PIC X(2).
18     1 INFORCE-PRMAX                     COMP PIC 9(4).
19 5 INFORCE-MAX-SEGS                     COMP PIC S9(4) VALUE +4.
11* HISTORY READ ONLY DATA
111 5 SDT-H-TABLE-NAME                     PIC X(8) VALUE 'CKESDTBH'.
112 5 HISTORY-REC-CTL-LEN                  COMP PIC S9(4) VALUE +20.
113 5 INFORCE-VSAMX-INFO.
114     1 FILLER                            PIC X(7) VALUE 'FVDUNLD'.
115     1 FILLER                            PIC X VALUE LOW-VALUE.
116     1 FILLER                            PIC X VALUE ' '.
117     1 FILLER                            PIC X(8) VALUE LOW-VALUES.
118 5 HISTORY-VSAMX-INFO.
119     1 FILLER           PIC X(7)   VALUE 'FVDULHD'.
12     1 FILLER           PIC X      VALUE LOW-VALUE.
121     1 FILLER           PIC X      VALUE ' '.
122     1 FILLER           PIC X(11)  VALUE LOW-VALUES.
123 EJECT
124********************************************************************
125*                V A R I A B L E   D A T A   A R E A S             *
126********************************************************************
127 1 VARIABLE-WORK-AREA.
128     5 RECORD-LENGTH        PIC S9(8) COMP.
129     5 WS-ISSUE-AGE         PIC S9(3) COMP-3.
13     5 WS-ERROR-MSG         PIC X(5).
131     5 WS-SUB               PIC S9(4) COMP-3 VALUE .
132     5 WS-SEG-ID            PIC X(2).
133     5 WS-STATUS            PIC X(2).
134     5 WS-ISSUE-STATE       PIC X(2).
135     5 WS-OWNER-STATE       PIC X(2).
136     5 WS-PLAN-BASE         PIC X(2).
137     5 WS-PLAN-TYPE         PIC X(1).
138     5 WS-SEG-SEQ           COMP-3 PIC S9(5).
139     5 WS-ACF-IO-BYTE       PIC X.
14     5 WS-RETURN-CODE       PIC X.
141     5 WS-SEG-WORK-AREA     PIC X(25) VALUE SPACE.
142* DATE AREA
143     5 WS-BIRTH-DATE.
144         1 WS-BRTH-MM         PIC X(2).
145         1 WS-BRTH-DD         PIC X(2).
146         1 WS-BRTH-YYYY       PIC X(4).
147     5 WS-ISSU-DATE.
148         1 WS-ISSU-MM         PIC X(2) VALUE SPACE.
149         1 WS-ISSU-DD         PIC X(2) VALUE SPACE.
15         1 WS-ISSU-YYYY       PIC X(4) VALUE SPACE.
151     5 WS-CURR-DATE.
152         1 WS-CURR-YEAR       PIC 9(2).
153         1 WS-CURR-MO         PIC 9(2).
154         1 WS-CURR-DAY        PIC 9(2).
155     5 WS-CURR-CONV-DATE.
156         1 WS-CURR-CONV-MM    PIC 9(2).
157         1 WS-CURR-CONV-DD    PIC 9(2).
158         1 WS-CURR-CONV-CC    PIC 9(2).
159         1 WS-CURR-CONV-YY    PIC 9(2).
16     5 WS-INT-CURR-DATE     COMP-3.
161         1 WS-INT-YEAR        PIC S9(3).
162         1 WS-INT-DAY         PIC S9(3).
163     5 WS-INT-ISSU-DATE     COMP-3.
164         1 WS-INT-ISSU-MMYY   PIC S9(3) COMP-3.
165         1 WS-INT-ISSU-DD     PIC S9(3) COMP-3.
166     5 WS-CKDCARTH-CONTANTS.
167         1 WS-DCARTH-MONTHS   PIC S9(3) COMP-3.
168         1 WS-DCARTH-DAYS     PIC S9(3) COMP-3.
169         1 WS-DCARTH-YEARS    PIC S9(3) COMP-3.
17         1 WS-DCARTH-DIFFERENCE PIC X VALUE '2'.
171     5 WS-POLICY-READ-CNT   PIC 9(9) VALUE ZERO.
172     5 WS-REC-WRITTEN-CNTR  PIC 9(9) VALUE ZERO.
173 EJECT
174********************************************************************
175* INFORCE RECORD CONTROL SECTION
176********************************************************************
177
178 1 INFORCE-FILE-AREA.
179     5 INFORCE-REC-LENGTH   PIC S9(4) COMP.
18     5 INFORCE-FILE-KEY.
181         1 INFORCE-REC-ID    PIC X(1).
182         1 INFORCE-USER-ID   PIC X(1).
183         1 INFORCE-POL-NUMBER PIC X(1).
184     5 INFORCE-IO-STAT      PIC X(1).
185     5 FILLER               PIC X(64985).
186 EJECT
187 1 INFORCE-FILE-DCB.
188     COPY CKDCBMAX.
189 EJECT
19 1 INFORCE-FILE-AUXDCB      PIC X(25).
191 1 HISTORY-FILE-AUXDCB      PIC X(25).
192 EJECT
193********************************************************************
194* HISTORY RECORD CONTROL SECTION
195********************************************************************
196
197 COPY CKNRECRC.
198 5 FILLER                   PIC X(1398).
199 EJECT
2 1 HISTORY-FILE-DCB.
21     COPY CKDCBLRG.
22     COPY CKUBGPRM.
23 EJECT
24********************************************************************
25* OUTPUT RECORD - VUL18 ACTIVE 22 FILEPASS
26********************************************************************
27
28 1 OFFLINE-RECORD           PIC X(4) VALUE SPACE.
29 EJECT
21********************************************************************
211* REPORT RECORD - VUL18 ACTIVE 22 DETAILS
212********************************************************************
213
214 1 RP-RECORD.
215     5 RP-POLICY             PIC X(1).
216     5 FILLER                PIC X(1) VALUE X'5'.
217     5 RP-POL-STATE          PIC X(2).
218     5 FILLER                PIC X(1) VALUE X'5'.
219     5 RP-STATUS             PIC X(2).
22     5 FILLER                PIC X(1) VALUE X'5'.
221     5 RP-STATUS-DESC        PIC X(3).
222     5 FILLER                PIC X(1) VALUE X'5'.
223     5 RP-ISSUE-DATE         PIC X(1).
224     5 FILLER                PIC X(1) VALUE X'5'.
225     5 RP-PRODUCT-NAME       PIC X(51).
226     5 FILLER                PIC X(1) VALUE X'5'.
227     5 RP-OWNER-NAME         PIC X(81).
228     5 FILLER                PIC X(1) VALUE X'5'.
229     5 RP-OWNER-ADDRESS      PIC X(1).
23     5 FILLER                PIC X(1) VALUE X'5'.
231     5 RP-PLAN-CODE          PIC X(11).
232     5 FILLER                PIC X(1) VALUE X'5'.
233     5 RP-ISSUE-STATE        PIC X(2).
234     5 FILLER                PIC X(1) VALUE X'5'.
235     5 RP-ISSUE-AGE          PIC ZZ9.
236     5 FILLER                PIC X(1) VALUE X'5'.
237 EJECT
238********************************************************************
239*                    ESSENTIAL SEGMENTS ONLY                        *
24********************************************************************
241
242 COPY CKFRECCV.
243 EJECT
244 COPY CKFRECAU.
245 EJECT
246 COPY CKFRECUB.
247 EJECT
248* AUXSEG1 RECORD AREA
249 COPY CKAUXDCB REPLACING AUXBLOCK-AUXILIARY-DCB
25                     BY AUX-INF-DCB.
251 EJECT
252 COPY CKESDTB1 REPLACING SEGMENT-DEFINITION-TABLE BY
253                     INFORCE-AUX-SDT.
254 EJECT
255********************************************************************
256*                     BATCH  I/O  RECORD                          *
257********************************************************************
258
259 COPY CKBCHCDS REPLACING
26      BCHCODES-CALLING-CODES BY BCHCODES.
261 1 FILLER PIC X(32)
262    VALUE 'NYFPOWFF WORKING STORAGE ENDS  '.
263 EJECT
264 LINKAGE SECTION.
265 EJECT
266 PROCEDURE DIVISION.
267********************************************************************
268*                        MAINLINE LOGIC                           *
269********************************************************************
27271 -CONTROL-PROCESS.
272     PERFORM 1-INITIALIZATION
273         THRU 199-INITIALIZATION-EXIT.
274     PERFORM 11-OPEN-FILES
275         THRU 1199-OPEN-FILES-EXIT.
276     SET CONTINUE-PROCESSING TO TRUE.
277     MOVE SPACE TO END-OF-FILE-INDICATOR.
278     PERFORM 2-MAIN-PROCESS
279         THRU 2-MAIN-PROCESS-EXIT
28         UNTIL END-OF-FILE.
281     PERFORM EOJ9-CLOSE-FILES
282         THRU EOJ9999-EXIT.
283     GOBACK.
284 EJECT
285********************************************************************
286*                         INITIALIZATION                          *
287********************************************************************
288
289 1-INITIALIZATION.
29     CALL 'CKSETADR' USING BINARY1
291                            UBAUHCB-AUX-HIST-DCB-ADDR
292                            AUX-HIST-DCB.
293     MOVE LOW-VALUES TO AUX-HIST-DCB.
294     CALL 'CKSETADR' USING BINARY1
295                            AUXSEGDT-PTR OF AUX-HIST-DCB
296                            HISTORY-AUX-SDT.
297     INITIALIZE END-OF-FILE-INDICATOR.
298     MOVE ZERO TO WS-IO-CODE.
299     MOVE LOW-VALUES TO INFORCE-FILE-DCB.
3     INITIALIZE INFORCE-FILE-AREA.
31     INITIALIZE WS-POLICY-READ-CNT WS-REC-WRITTEN-CNTR.
32* GET CURRENT DATE
33     ACCEPT WS-CURR-DATE FROM DATE.
34     MOVE WS-CURR-MO  TO WS-CURR-CONV-MM.
35     MOVE WS-CURR-DAY TO WS-CURR-CONV-DD.
36     MOVE 2          TO WS-CURR-CONV-CC.
37     MOVE WS-CURR-YEAR TO WS-CURR-CONV-YY.
38     CALL 'CKDCEXIN'
39         USING WS-CURR-CONV-DATE
31               WS-INT-CURR-DATE.
311 199-INITIALIZATION-EXIT.
312     EXIT.
313 EJECT
314********************************************************************
315*                         OPEN ALL FILES                          *
316********************************************************************
317
318 11-OPEN-FILES.
319* OPEN OUTPUT REPORT FILE
32     OPEN OUTPUT REPORT-FILE.
321     MOVE '6' TO WS-IO-CODE.
322     CALL 'CKVSAMIO'
323          USING INFORCE-VSAM
324                WS-IO-CODE
325                INFORCE-FILE-AREA
326                INFORCE-FILE-LENGTH
327                INF-RECORD-KEY
328                INFORCE-VSAMX-INFO.
329     IF WS-IO-CODE NOT EQUAL ''
33         DISPLAY 'OPEN INFORCE FAILED'
331         DISPLAY 'WS-IO-CODE=' WS-IO-CODE
332         GO TO EOJ99-ABEND
333     END-IF.
334     MOVE '6' TO WS-IO-CODE.
335     CALL 'CKSDT1IO'
336          USING WS-IO-CODE
337                INFORCE-AUX-SDT.
338     IF WS-IO-CODE NOT EQUAL ''
339         DISPLAY 'OPEN OF AUXSEG1 FILE FAILED'
34         DISPLAY 'WS-IO-CODE=' WS-IO-CODE
341         GO TO EOJ99-ABEND
342     END-IF.
343     MOVE '4' TO WS-IO-CODE.
344     CALL 'CKETRLST'
          USING WS-IO-CODE
                WS-DUMMY
                INFORCE-FILE-DCB
                WS-DUMMY
                WS-DUMMY
     IF WS-IO-CODE NOT EQUAL ''
         DISPLAY 'ERROR IN CKETRLST'
         DISPLAY 'WS-IO-CODE=' WS-IO-CODE
         GO TO EOJ99-ABEND
     END-IF
35 1199-OPEN-FILES-EXIT.
351     EXIT.
352 EJECT
353********************************************************************
354*                        MAIN PROCESS                             *
356
357 2-MAIN-PROCESS.
358     PERFORM 21-READ-NEXT-POLICY
359         THRU 2199-READ-NEXT-POLICY-EXIT.
36     IF NOT END-OF-FILE
361         PERFORM 22-PROCESS-POLICY
362             THRU 2299-PROCESS-POLICY-EXIT
363     END-IF.
364 2-MAIN-PROCESS-EXIT.
365     EXIT.
366 EJECT
367********************************************************************
368*                    READ NEXT POLICY                             *
369********************************************************************
37371 21-READ-NEXT-POLICY.
372     MOVE '8' TO WS-IO-CODE.
373     CALL 'CKVSAMIO'
374          USING INFORCE-VSAM
375                WS-IO-CODE
376                INFORCE-FILE-AREA
377                INFORCE-FILE-LENGTH
378                INF-RECORD-KEY
379                INFORCE-VSAMX-INFO.
38     IF WS-IO-CODE = '6'
381         SET END-OF-FILE TO TRUE
382     ELSE
383         IF WS-IO-CODE NOT = ''
384             DISPLAY 'READ INFORCE FAILED'
385             DISPLAY 'WS-IO-CODE=' WS-IO-CODE
386             GO TO EOJ99-ABEND
387         END-IF
388     END-IF.
389 2199-READ-NEXT-POLICY-EXIT.
39     EXIT.
391 EJECT
392********************************************************************
393*                     PROCESS POLICY                              *
394********************************************************************
395
396 22-PROCESS-POLICY.
397     MOVE 'N' TO VUL18-IND
398     MOVE 'N' TO ACTIVE-22-IND
399* CHECK FOR VUL18 PRODUCT
4     IF PLAN-CODE OF CV-SEGMENT = 'VUL18'
41         SET VUL18-PRODUCT TO TRUE
42     END-IF
43* CHECK FOR ACTIVE STATUS 22
44     IF VUL18-PRODUCT
45         IF CURR-STAT OF CV-SEGMENT = '22'
46             SET ACTIVE-22-FOUND TO TRUE
47         END-IF
48     END-IF
49* IF VUL18 AND ACTIVE 22, PROCESS THE POLICY
41     IF VUL18-PRODUCT AND ACTIVE-22-FOUND
411         PERFORM 23-BUILD-REPORT-RECORD
412             THRU 2399-BUILD-REPORT-RECORD-EXIT
413         PERFORM 24-WRITE-REPORT-RECORD
414             THRU 2499-WRITE-REPORT-RECORD-EXIT
415     END-IF
416 2299-PROCESS-POLICY-EXIT.
417     EXIT.
418 EJECT
419********************************************************************
42*                  BUILD REPORT RECORD                            *
421********************************************************************
422
423 23-BUILD-REPORT-RECORD.
424* MOVE POLICY NUMBER
425     MOVE POLICY-NUM OF CV-SEGMENT TO RP-POLICY
426* MOVE POLICY STATE
427     MOVE STATE OF CV-SEGMENT TO RP-POL-STATE
428* MOVE STATUS
429     MOVE CURR-STAT OF CV-SEGMENT TO RP-STATUS
43* MOVE STATUS DESCRIPTION
431     EVALUATE CURR-STAT OF CV-SEGMENT
432         WHEN '22'
433             MOVE 'ACTIVE INFORCE' TO RP-STATUS-DESC
434         WHEN OTHER
435             MOVE 'UNKNOWN STATUS' TO RP-STATUS-DESC
436     END-EVALUATE
437* MOVE ISSUE DATE
438     MOVE ISSUE-DATE OF CV-SEGMENT TO WS-ISSU-DATE
439     MOVE WS-ISSU-DATE TO RP-ISSUE-DATE
44* MOVE PRODUCT NAME
441     MOVE PLAN-NAME OF CV-SEGMENT TO RP-PRODUCT-NAME
442* MOVE OWNER NAME
443     MOVE OWNER-NAME OF AU-SEGMENT TO RP-OWNER-NAME
444* MOVE OWNER ADDRESS
445     STRING ADDRESS-1 OF AU-SEGMENT DELIMITED BY SPACE
446            ' ' DELIMITED BY SIZE
447            ADDRESS-2 OF AU-SEGMENT DELIMITED BY SPACE
448            ' ' DELIMITED BY SIZE
449            CITY OF AU-SEGMENT DELIMITED BY SPACE
45            ' ' DELIMITED BY SIZE
451            STATE OF AU-SEGMENT DELIMITED BY SPACE
452            ' ' DELIMITED BY SIZE
453            ZIP-CODE OF AU-SEGMENT DELIMITED BY SPACE
454            INTO RP-OWNER-ADDRESS
455* MOVE PLAN CODE
456     MOVE PLAN-CODE OF CV-SEGMENT TO RP-PLAN-CODE
457* MOVE ISSUE STATE
458     MOVE ISSUE-STATE OF CV-SEGMENT TO RP-ISSUE-STATE
459* CALCULATE AND MOVE ISSUE AGE
46     PERFORM 25-CALCULATE-ISSUE-AGE
461         THRU 2599-CALCULATE-ISSUE-AGE-EXIT
462     MOVE WS-ISSUE-AGE TO RP-ISSUE-AGE
463 2399-BUILD-REPORT-RECORD-EXIT.
464     EXIT.
465 EJECT
466********************************************************************
467*                 WRITE REPORT RECORD                            *
468********************************************************************
469
47 24-WRITE-REPORT-RECORD.
471     WRITE REPORT-REC FROM RP-RECORD
472     IF NOT WRITE-OK
473         DISPLAY 'WRITE ERROR ON REPORT FILE'
474         GO TO EOJ99-ABEND
475     END-IF
476     ADD 1 TO WS-REC-WRITTEN-CNTR
477 2499-WRITE-REPORT-RECORD-EXIT.
478     EXIT.
479 EJECT
48********************************************************************
481*               CALCULATE ISSUE AGE                               *
482********************************************************************
483
484 25-CALCULATE-ISSUE-AGE.
485* CONVERT ISSUE DATE TO INTERNAL FORMAT
486     MOVE ISSUE-DATE OF CV-SEGMENT TO WS-INT-ISSU-DATE
487     CALL 'CKDCINEX' USING WS-ISSU-DATE WS-INT-ISSU-DATE
488* CALCULATE AGE DIFFERENCE
489     CALL 'CKDCARTH' USING WS-INT-CURR-DATE
49                           WS-INT-ISSU-DATE
491                           WS-DCARTH-DIFFERENCE
492                           WS-ISSUE-AGE
493 2599-CALCULATE-ISSUE-AGE-EXIT.
494     EXIT.
495 EJECT
496********************************************************************
497*                        CLOSE FILES                              *
498********************************************************************
499
5 EOJ9-CLOSE-FILES.
51* CLOSE REPORT FILE
52     CLOSE REPORT-FILE
53* CLOSE INFORCE FILE
54     MOVE '5' TO WS-IO-CODE.
55     CALL 'CKVSAMIO'
56          USING INFORCE-VSAM
57                WS-IO-CODE
58* DISPLAY COUNTERS
59     DISPLAY 'POLICIES READ: ' WS-POLICY-READ-CNT
51     DISPLAY 'RECORDS WRITTEN: ' WS-REC-WRITTEN-CNTR
511     GO TO EOJ9999-EXIT.
512 EOJ99-ABEND.
513     DISPLAY 'PROGRAM ABENDING DUE TO ERROR'
514 EOJ9999-EXIT.
515     EXIT.