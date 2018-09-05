C----67-----------
      CHARACTER*80     text
      
    1 CONTINUE
      read( 5, 9000, end=100 ) text
      i = index( text(1:80), '(' )
      text(i:i) = ' '
      i = index( text(1:80), '(' )
      text(i:i) = ' '
      i = index( text(1:80), ')' )
      text(i:i) = ' '
      i = index( text(1:80), ')' )
      text(i:i) = ' '
      write( 6, 9000 ) text
      GOTO 1
  100 CONTINUE
 9000 FORMAT( A80 )
 
      stop
      end