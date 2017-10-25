C      IMPLICIT undefined (a-z)
C
C     Parameter variables
C
      INTEGER max
      PARAMETER (max = 1024)
C
C     Local variables
C
      DOUBLE PRECISION doffset(0:max-1), dpeak(0:max-1,3)
      DOUBLE PRECISION meanrgain(0:max-1), minpeak, offset(0:max-1)
      DOUBLE PRECISION peak(0:max-1,3), rgain(0:max-1,3), sumrgain
      DOUBLE PRECISION inl(0:max-1), y(6), dy(6), mean, sum
C
      INTEGER i, j, min, ncentroids(0:max-1), noffsets, npeaks, spectrum
      INTEGER count(0:max-1), imax, lo, hi, k
C
      CHARACTER*32 text(0:max-1)
C
      LOGICAL valid(0:max-1)
C
C     Initialise data

      do i = 0, max-1
       offset( i ) = -9999.99d+00
       doffset( i ) = 0.0d+00
       ncentroids( i ) = 0
       meanrgain( i ) = -1.9999999d+00
       valid( i ) = .false.
       do j = 1, 3
        peak( i, j ) = -9999.99d+00
        dpeak( i, j ) = 0.0d+00
        rgain( i, j ) = -1.9999999d+00
       enddo
      enddo

      WRITE( 6, 9050 )
      READ( 5, * ) imax

      WRITE( 6, 9060 )
      READ( 5, * ) lo, hi

C----67---------------------------------------------------------------72------80

      open( 1, file='/tmp/offsets.dat', status = 'OLD' )

      open( 2, file='/tmp/gains.dat', status = 'OLD' )

C      open( 4, file='/tmp/variables.dat', status = 'NEW' )

C     Read offset data

      do i = 0, max-1

       READ( 1, 1200, END=10 ) k, offset(k), doffset(k), inl(k),
     +                  count(k), text(k)

      enddo
   10 continue
      noffsets = i - 1

      write( 6, * ) noffsets, ' offset data points read'

C     Read (alpha) calibration peak centroids

      do i = 0, max-1

       READ( 2, 1100, END=20 ) k, ( Y(j), DY(j), j=1, 6 ),
     +                      count(k), text(k)


       DO J = 1, 6
        peak( k, j ) = y( j )
        dpeak( k, j ) = dy( j )
       ENDDO
       write( 6, * ) k, peak(k,1)
      ENDDO

   20 continue
      npeaks = i - 1

      write( 6, * ) npeaks, ' calibration peak centroids read'

C     Validate data

      do i = 0, imax-1
       write( 6,*) i, peak(i,1)
      enddo

      do i = 0, imax-1
       write( 6, * ) i, offset(i), peak(i,1), count(i)
       if ( offset(i).gt.-999.9d+00
     +      .and. peak(i,1).gt.0.0d+00
     +      .and. count(i).EQ.3 ) then
        valid( i ) = .true.
       else
       endif
      enddo

C     Offset correction of (alpha) calibration peak centroids

      do i = 0, imax-1
       do j = 1, 3
        if ( valid( i ) ) then
         peak( i, j ) = peak( i, j ) - offset( i )
        endif
       enddo
      enddo

C     Determine channel with minimum gain

      sum = 0.0d+00
      k =0
      do i = lo, hi
       if ( valid(i) ) then
        sum = sum + peak(i,2)
        k = k + 1
       endif
      enddo
      mean = sum / dfloat( k )

      write( 6, * ) ' mean centroid:', mean
      min = -1
      minpeak = 65535.0d+00
      do i = lo, hi
       if ( peak( i, 2 ).lt.minpeak .and. valid(i)
     +      .and. peak(i,2).GT.( 0.7d+00 * mean ) ) then
        min = i
        minpeak = peak( i, 2 )
       endif
      enddo
      write( 6, * ) ' min. gain channel: ', min

C     Determine gains of each channel relative to minimum gain channel

      do i = 0, imax-1
       do j = 1, 3
        if ( valid( i ) ) then
         rgain( i, j ) = peak( min, j ) / peak( i, j )
        endif
       enddo
      enddo

C     Determine mean relative gain of each channel

      do i = 0, imax-1
       sumrgain = 0.0d+00
       do j = 1, 3
        sumrgain = sumrgain + rgain( i, j )
       enddo
       if ( valid(i) ) then
        meanrgain( i ) = sumrgain / 3.0d+00
       endif
      enddo

C     Output results

      write( 4, 9070 )

      do i = 0, imax-1
       write( 4, 9080 ) i, offset( i )
      enddo

      do i = 0, imax-1
       write( 4, 9090 ) i, meanrgain( i )
      enddo

      write( 4, 9100 )

      do i = 0, imax-1
       write( 6, 9020 ) i, valid(i),offset(i),meanrgain( i )
      enddo

      stop

 9020 format( ' channel: ', i4, ' valid: ', l2, ' offset: ' ,f8.2,
     +        ' gain: ', f12.7 )
1200  FORMAT( i8, 1X, F8.2, 1X, F8.2, 1X, F7.3, 1X, i3, 1x,  a32 )
1100  FORMAT( i8, 6(1X, F8.2, 1X, F8.2), 1x, i3, 1x, a32 )
 9050 FORMAT(/' *** Program variables',
     +       /' *** Enter number of channels (max 1024): ' $ )
 9060 FORMAT( ' *** Enter range of channels for min. gain channel'
     +        ' search: ' $ )
 9070 FORMAT( '  $variables' )
 9080 format( '  offset(', i4, ') = ', f8.2 )
 9090 format( '  gain(', i4, ') = ', f12.7 )
 9100 FORMAT( '  $[end]' )

C----67---------------------------------------------------------------72------80

      end
