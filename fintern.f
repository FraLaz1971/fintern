C=============================================================
C INITIALIZE MEMORY ARRAY WITH BLANKS
C=============================================================
      SUBROUTINE INIT_MEM(MEM, N)
        CHARACTER*80 MEM(*)
        INTEGER I, N
        DO 10 I = 1, N
           MEM(I) = ' '
10      CONTINUE
        RETURN
      END


C=============================================================
C STORE (MARSHALL) A RECORD INTO MEM
C=============================================================
       SUBROUTINE STORE_REC(MEM, NMAX, POS, ID, NAME, AGE, SCORE,STATUS)
         CHARACTER*80 MEM(*)
         CHARACTER*(*) NAME, STATUS
         INTEGER ID, AGE, POS, NMAX
         REAL SCORE
         IF (POS .LT. 1 .OR. POS .GT. NMAX) RETURN
         WRITE(MEM(POS), '(I6,1X,A20,1X,I3,1X,F7.2,1X,A10)')
     &   ID, NAME, AGE, SCORE, STATUS
         RETURN
       END

C=============================================================
C LOAD (UNMARSHALL) A RECORD FROM MEM
C=============================================================
        SUBROUTINE LOAD_REC(MEM, POS, ID, NAME, AGE, SCORE, STATUS)
          CHARACTER*80 MEM(*)
          CHARACTER*(*) NAME, STATUS
          INTEGER ID, AGE, POS
          REAL SCORE
          READ(MEM(POS), '(I6,1X,A20,1X,I3,1X,F7.2,1X,A10)')
     &  ID, NAME, AGE, SCORE, STATUS
          RETURN
        END


C=============================================================
C PRINT ALL RECORDS (RAW)
C=============================================================
        SUBROUTINE PRINT_MEM(MEM, N)
          CHARACTER*80 MEM(*)
          INTEGER I, N
          DO 10 I = 1, N
            IF (MEM(I) .NE. ' ') THEN
              PRINT *, I, ': "', MEM(I), '"'
            ENDIF
10       CONTINUE
         RETURN
        END

C=============================================================
C FIND RECORD BY ID
C=============================================================
        SUBROUTINE FIND_BY_ID(MEM, NMAX, ID_SEARCH, FOUND_POS)
          CHARACTER*80 MEM(*)
          CHARACTER*10 TMPSTATUS
          CHARACTER*20 TMPNAME
          INTEGER I
          INTEGER NMAX, FOUND_POS, ID_SEARCH
          INTEGER TMPID
          INTEGER TMPAGE
          REAL    TMPSCORE
          FOUND_POS = 0
          DO 10 I = 1, NMAX
            IF (MEM(I) .NE. ' ') THEN
              READ(MEM(I), '(I6)') TMPID
              IF (TMPID .EQ. ID_SEARCH) THEN
                 FOUND_POS = I
                 RETURN
              ENDIF
            ENDIF
10        CONTINUE
          RETURN
        END
C=======================================================

      PROGRAM FINTERN
        IMPLICIT NONE
        INTEGER MAXREC
        PARAMETER (MAXREC = 5)
C     OUR IN-MEMORY "FILE": 80 CHARS PER RECORD
        CHARACTER*80 MEM(MAXREC)
C     LOCAL VARIABLES FOR LOADING
        INTEGER ID, AGE
        REAL SCORE
        CHARACTER*20 NAME
        CHARACTER*10 STATUS
        INTEGER FOUND
C         C     INITIALIZE MEMORY TO BLANKS
        CALL INIT_MEM(MEM, MAXREC)
C
C      ADD RECORDS
      CALL STORE_REC(MEM, MAXREC, 1, 101, 'ALPHA', 25, 88.5, 'ACTIVE')
      CALL STORE_REC(MEM, MAXREC, 2, 102, 'BETA',  31, 72.0, 'INACTIVE')
      CALL STORE_REC(MEM, MAXREC, 3, 103, 'GAMMA', 27, 99.2, 'ACTIVE')

C      PRINT DATABASE CONTENTS
      PRINT *, '--- DATABASE CONTENTS ---'
      CALL PRINT_MEM(MEM, MAXREC)

C      SEARCH FOR RECORD BY ID
       CALL FIND_BY_ID(MEM, MAXREC, 102, FOUND)
      IF (FOUND .GT. 0) THEN
          CALL LOAD_REC(MEM, FOUND, ID, NAME, AGE, SCORE, STATUS)
          PRINT *, '--- RECORD FOUND ---'
          PRINT *, 'ID    = ', ID
          PRINT *, 'NAME  = ', NAME
          PRINT *, 'AGE   = ', AGE
          PRINT *, 'SCORE = ', SCORE
          PRINT *, 'STATUS= ', STATUS
      ELSE
          PRINT *, 'RECORD NOT FOUND.'
      ENDIF

C     UPDATE RECORD BY ID
       CALL FIND_BY_ID(MEM, MAXREC, 103, FOUND)
       IF (FOUND .GT. 0) THEN
           CALL STORE_REC(MEM, MAXREC, FOUND, 103, 'GAMMA', 28,
     +      100.0, 'ACTIVE')
       ENDIF

C     PRINT AFTER UPDATE
      PRINT *, '--- DATABASE AFTER UPDATE ---'
       CALL PRINT_MEM(MEM, MAXREC)
       STOP
      END
