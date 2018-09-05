      IMPLICIT DOUBLE PRECISION(A-H,O-Z),INTEGER(I-N)
C
      DOUBLE PRECISION X( 100 ), Y( 100 ), A( 100 ), FWHM( 100 )
      DOUBLE PRECISION          DY( 100 ),DA( 100 )
      DOUBLE PRECISION FY( 100 ), DF( 100 )
      INTEGER          IX( 100 ), rindex
      CHARACTER*1      T1, T2, T3, T4
      CHARACTER*8      SPEC
      CHARACTER*4      SPECNUM
      CHARACTER*80     TEXT
C
      DATA Y/100*0.0D+00/
      DATA DY/100*0.0D+00/
      DATA IX/100*0/
      DATA X/100*0.0D+00/
      DATA A/100*0.0D+00/
      DATA DA/100*0.0D+00/
      DATA FY/100*0.0D+00/
      DATA DF/100*0.0D+00/
      DATA FWHM/100*0.0D+00/
C
      OPEN( 2, file = '/tmp/gains.log', access = 'APPEND',
     +      status = 'OLD' )
      OPEN( 4, file = '/tmp/gains.txt', access = 'APPEND',
     +      status = 'OLD' )

C     Reads in the first line from peakfind
C     and separates the spectra file name

      READ( 5,980 ) TEXT
      i = RINDEX( TEXT(1:80), '/' )
      j = RINDEX( TEXT(1:80), ',' )
      SPEC(1:8) = TEXT(i+1:j-1)

C     Removes the s and .msf from the file name

      k = index(SPEC , 's' )
      IF ( SPEC( lnblnk(SPEC)-3:lnblnk(SPEC) ).EQ.'.msf'
     +     .OR.
     +     SPEC( lnblnk(SPEC)-3:lnblnk(SPEC) ).EQ.'.MSF' ) THEN
       SPECNUM = SPEC(k+1:lnblnk(SPEC)-4)
      ELSE
       SPECNUM = SPEC(k+1:)
      ENDIF

      READ( 5, 980 ) TEXT
      READ( 5, *, END = 10 )
     +    ( IX( I ), Y( I ), DY( I ), A( I ), DA( I ),
     +      FWHM( I ), I = 1, 100    )
   10 CONTINUE
      NUMBER = I - 1

      WRITE( 2, * ) SPECNUM
      WRITE( 2, * ) TEXT
      IF ( NUMBER.LT.10 ) THEN
       DO i = 1, number
        WRITE( 2, 9040 )
     +    IX( I ), Y( I ), DY( I ), A( I ), DA( I ), FWHM( I )
       ENDDO
      ENDIF

C     Writes the data to the output files

      IF ( NUMBER.EQ.3 ) THEN
       TEXT = ''
       WRITE( 4, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
       WRITE( 6, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
      ELSEIF( NUMBER.LT.3 ) THEN
       TEXT = ' < 3 peaks'
       WRITE( 4, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
       WRITE( 6, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
      ELSEIF( NUMBER.GT.3 .AND. NUMBER.LE.6 ) THEN
       TEXT = ' 3 < peaks < 6'
       WRITE( 4, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
       WRITE( 6, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
      ELSEIF( NUMBER.GT.6 ) THEN
       TEXT = ' > 6 peaks or no data'
       NUMBER = 1
       WRITE( 4, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
       WRITE( 6, 9030 ) SPECNUM, ( Y(i), DY(i), i=1, 6 ), NUMBER, TEXT
      ENDIF

      STOP

 980  FORMAT( A80 )
 990  FORMAT(I3,2X,F7.2,A1,F5.3,A1,1X,F10.1,A1,F6.1,A1,3X,F5.2)
1000  FORMAT( 1X, A8, 1X, F8.2, 1X, F8.2, 1X, F8.2 )
1010  FORMAT( 1X, A8, 1X, F8.2, 1X, F8.2 )
1020  FORMAT( 1X, A8  )
C     1030  FORMAT( 1x, a8, 1x, i3, <number>(1x,f9.3) )
9000  FORMAT( ' *** ERROR: <3 peaks found in spectrum: ',a8 )
9010  FORMAT( ' *** ERROR: >3 peak found in spectrum: ',a8 )
9020  FORMAT( ' ***', i12, ' peaks found in spectrum: ',a8 )
9030  FORMAT( a8, 6(1x, f7.2, 1x, f5.2), 1x, i8, 1x, a32 )
9040  FORMAT( i3, 2x, F7.2, 1x, F5.3, 1x, F10.1, 1x, F6.1, 4x, F5.2 )

      END

      INTEGER FUNCTION rindex( string, substring )

      INTEGER i, j, k
      CHARACTER*255 string
      CHARACTER substring

      j = lnblnk( substring )
      k = 0

      DO i = 1, 255

       IF ( string(i:i+j-1) .EQ. substring(1:j) ) k=i

      ENDDO
      
      rindex = k

      RETURN
      END
