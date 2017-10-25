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
      OPEN( 2, file = '/tmp/offsets.log', access = 'APPEND',
     +      status = 'OLD' )
      OPEN( 4, file = '/tmp/offsets.txt', access = 'APPEND',
     +      status = 'OLD' )
C
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

      IF ( NUMBER.LT.3 .OR. NUMBER.GT.9 ) GOTO 900

      EX = 0.0D+00
      EY = 0.0D+00
      EXY = 0.0D+00
      EXSQ = 0.0D+00
      EW = 0.0D+00
      DO 500 J = 1,NUMBER
      WEIGHT = 1.0 / ( DY(J) * DY(J) )
      X( J ) = DBLE( IX( J ) )
      EX = X(J) * WEIGHT + EX
      EY = Y(J) * WEIGHT + EY
      EXY = X(J) * Y(J) * WEIGHT + EXY
      EXSQ = X(J) * X(J) * WEIGHT + EXSQ
      EW = WEIGHT + EW
  500 CONTINUE
C
      DELTA = EW * EXSQ - ( EX * EX )
      XM = ( EW * EXY - EX * EY ) / DELTA
      XC = ( EXSQ * EY - EX * EXY ) / DELTA
      ERRXC = EXSQ / DELTA
      ERRXM = EW / DELTA
      ERRXC = DSQRT(ERRXC)
      ERRXM = DSQRT(ERRXM)
C
      WRITE( 2, 1100 ) SPEC
      DFMAX = 0.0D+00
      EFWHM = 0.0D+00
      DO 600 J = 1, NUMBER
      EFWHM = EFWHM + FWHM( J )
      FY( J ) = XM * X( J ) + XC
      DF( J ) = FY( J ) - Y( J )
      ABSDF = DSQRT( DF( J ) * DF( J ) )
      IF ( ABSDF.GT.DFMAX ) THEN
      DFMAX = ABSDF
      ENDIF
      WRITE( 2, 1000 ) X( J ), Y( J ), FY( J ), DF( J )
  600 CONTINUE
C
      WRITE( 2, 1010) XM, ERRXM
      WRITE( 2, 1020) XC, ERRXC
      ETA = ( DFMAX * 1.0D+02 ) / ( Y( NUMBER ) - Y( 1 ) )
      WRITE( 2, 1030) ETA
      WRITE( 2, 1040) EFWHM/NUMBER

C     Writes the data to the output files

  900 CONTINUE

      length = lnblnk( SPEC )
      IF ( NUMBER.GE.3 .AND. NUMBER.LE.9 ) THEN
       IF ( ETA.LT.0.1D+00 ) THEN
        TEXT = ''
        WRITE( 4, 1200 ) SPECNUM, XC, ERRXC, ETA, NUMBER, TEXT
        WRITE( 6, 1300 ) SPEC, XM, XC, ETA, TEXT
       ELSE
        TEXT = ' INL > 0.1%'
        WRITE( 4, 1200 ) SPECNUM, XC, ERRXC, ETA, NUMBER, TEXT
        WRITE( 6, 1300 ) SPEC, XM, XC, ETA, TEXT
       ENDIF
      ELSEIF( NUMBER.LT.3 ) THEN
       TEXT = ' < 3 peaks'
       XC = -9999.99
       ERRXC = 0.0
       ETA = 0.0
       WRITE( 4, 1200 ) SPECNUM, XC, ERRXC, ETA, NUMBER, TEXT
       WRITE( 6, 1300 ) SPEC, XM, XC, ETA, TEXT
      ELSEIF( NUMBER.GT.9 ) THEN
       TEXT = ' > 9 peaks or no data'
       ERRXC = 0.0
       ETA = 0.0
       XC = -9999.99
       WRITE( 4, 1200 ) SPECNUM, XC, ERRXC, ETA, NUMBER, TEXT
       WRITE( 6, 1300 ) SPEC, XM, XC, ETA, TEXT
      ENDIF

      STOP

 980  FORMAT( A80 )
 990  FORMAT(I3,2X,F7.2,A1,F5.3,A1,1X,F10.1,A1,F6.1,A1,3X,F5.2)
 999  FORMAT( ' *** ERROR: Less than 3 peaks found in spectrum: ',a8 )
1000  FORMAT( F5.1, 1X, F8.2, 1X, F8.2, 2X, F8.2 )
1010  FORMAT(/'                m = ',F8.2,' +/-',F8.2 )
1020  FORMAT( '                c = ',F8.2,' +/-',F8.2 )
1030  FORMAT( '    non-linearity = ',F6.3,' %')
1035  FORMAT(/' *** ERROR: Unacceptable integral non-linearity',
     +        ' found in spectrum: ', a8 / )
1040  FORMAT( '        mean fwhm = ',F6.3,' channels'/)
C     1050  FORMAT( 8X, 'offset( ', a<length-1>, ' ) = ', f9.3 )
1100  FORMAT( '*** Weighted least squares fit to spectrum: ', a8,
     +       //'X nom.  Y exp.   Y fit    Y fit - Y exp.')
1200  FORMAT( a8, 1X, F8.2, 1X, F8.2, 1X, F7.3, 1X, i3, 1x,  a32 )
1300  FORMAT( a8, 1x, F9.2, 1x, F8.2, 1x, F7.3, 1x, a32 )

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
