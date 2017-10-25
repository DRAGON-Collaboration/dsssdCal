
      IMPLICIT none

      INTEGER specnum, number
      REAL y( 6 ), dy( 6 )
      CHARACTER*32 text

      REAL centroid( 0:1023, 6 ), dcentroid( 0:1023, 6 )
      INTEGER count( 0:1023 ), i, j, k
      CHARACTER*32 error( 0:1023 )

      OPEN( 1, file = '/tmp/gains.txt', status = 'OLD' )
      OPEN( 2, file = '/tmp/gains.dat', access = 'APPEND',
     +      status = 'OLD' )

      DO i = 0, 1023
       DO j = 1, 6
        y(j) = 0.0
        dy(j) = 0.0
       ENDDO
       READ( 1, 1100, END=10 ) SPECNUM, ( Y(j), DY(j), j=1, 6 ),
     +                      NUMBER, TEXT
       DO J = 1, 6
        centroid( specnum, j ) = y( j )
        dcentroid( specnum, j ) = dy( j )
       ENDDO
       count( specnum ) = number
       error( specnum ) = text
      ENDDO

   10 CONTINUE
      k = i - 1

      DO i = 0, 1023
       IF ( count(i).NE.0 ) THEN
        WRITE( 2, 1200 ) i, ( centroid( i, j ), dcentroid( i, j ),
     +                  j = 1, 6 ), count(i), error(i)
       ENDIF
      ENDDO
     
      STOP

1100  FORMAT( i8, 6(1x, f7.2, 1x, f5.2), 1x, i8, 1x, a32 )
1200  FORMAT( i8, 6(1X, F8.2, 1X, F8.2), 1x, i3, 1x, a32 )

      END

