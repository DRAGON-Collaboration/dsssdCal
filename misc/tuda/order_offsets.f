
      IMPLICIT none

      INTEGER specnum, number
      REAL xc, errxc, eta
      CHARACTER*32 text

      REAL offset( 0:1023 ), doffset( 0:1023 ), inl( 0:1023 )
      INTEGER count( 0:1023 ), i, j
      CHARACTER*32 error( 0:1023 )

      OPEN( 1, file = '/tmp/offsets.txt', status = 'OLD' )
      OPEN( 2, file = '/tmp/offsets.dat', access = 'APPEND',
     +      status = 'OLD' )

      DO i = 0, 1023
       READ( 1, 1200, END=10 ) SPECNUM, XC, ERRXC, ETA, NUMBER, TEXT
       offset( specnum ) = xc
       doffset( specnum ) = errxc
       inl( specnum ) = eta
       count( specnum ) = number
       error( specnum ) = text
      ENDDO

   10 CONTINUE
      j = i - 1

      DO i = 0, 1023
       IF ( count(i).NE.0 ) THEN
        WRITE( 2, 1200 ) i, offset(i), doffset(i), inl(i),
     +                  count(i), error(i)
       ENDIF
      ENDDO
     
      STOP

1200  FORMAT( i8, 1X, F8.2, 1X, F8.2, 1X, F7.3, 1X, i3, 1x,  a32 )

      END

