program test1

  use sort_functions
  use file_functions, only: open, line_count
  use string_functions, only: ucase

  implicit none

  character(len=6)  :: strings(5) = (/ 'aaaaab', 'aaaaab', 'aaaAba', 'aaaabA', 'Aaaaba' /)
  integer           :: integers(5) = (/ 1, -123, 5, 7001, 5 /)
  real              :: reals(5) = (/ 1., -123., 5.4, 7001., 5.4 /)
  real(kind=8)      :: doubles(5) = (/ 1.d0, -123.d0, 5.4d0, 7001.d0, 5.4d0 /)

  character(len=11), allocatable :: words(:)
  character(len=11) :: word

  integer :: i, unit

  call announce ('Testing string sort')

  ! Produce a sorted strings array by using sorted index produced with .sort. operator
  if ( any(strings(.sort. strings) /= (/'Aaaaba','aaaAba','aaaaab','aaaaab','aaaabA'/)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  ! Now use the subroutine call to alter the actual array
  call sort(strings)

  ! Now array should be as above
  if ( any(strings /= (/'Aaaaba','aaaAba','aaaaab','aaaaab','aaaabA'/)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  ! Find the unique entry mask using the operator interface
  if ( any((.unique. strings) .neqv. (/.TRUE., .TRUE., .TRUE., .FALSE., .TRUE. /)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  call ok

  call announce ('Testing extracting unique strings')

  ! Now spit out the unique entries using the function call
  if ( any(unique(strings) /= (/'Aaaaba','aaaAba','aaaaab','aaaabA'/)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  ! Make all the strings upper case
  call ucase(strings)

  ! Sort them again
  call sort(strings)

  ! Should only have two unique entries left
  if ( any(unique(strings) /= (/'AAAAAB','AAAABA'/)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  allocate(words(line_count('random.txt')))

  ! Read a file of randomly ordered words into our array
  unit = open('random.txt',status='old')
  do i=1,size(words)
     read(unit,*) words(i)
  end do
  close(unit)

  ! Sort them ...
  call sort(words)

  ! Open another file with the same words, but sorted alphabetically
  ! and do a word by word comparison to make sure our sort was correct
  unit = open('ordered.txt',status='old')
  do i=1,size(words)
     read(unit,*) word
     if (word /= words(i)) then
        print *,'line: ',i,'   ',word,' /= ',words(i)
        call failed()
        stop
     end if
  end do

  close(unit)

  call ok

  call announce ('Testing integer sort')

  ! Test the functional interface
  if ( any(integers(.sort. integers) /= (/ -123, 1, 5, 5, 7001 /))) then
     call failed()
     stop
  end if

  ! Same but with a twist (sort by absolute value)
  if ( any(integers(.sort. abs(integers)) /= (/ 1, 5, 5, -123, 7001 /))) then
     call failed()
     stop
  end if

  ! Subroutine interface
  call sort(integers)
  if ( any(integers /= (/ -123, 1, 5, 5, 7001 /))) then
     call failed()
     stop
  end if

  call ok

  call announce ('Testing extracting unique integers')

  ! Test unique
  if ( any(unique(integers) /= (/ -123, 1, 5, 7001 /))) then
     call failed()
     stop
  end if

  ! Test unique
  if ( num_unique((/ 1, 1, 1, 1 /) ) /= 1 ) then
     call failed()
     stop
  end if


  call ok()

  call announce ('Testing real sort')

  ! real              :: reals(5) = (/ 1., -123., 5.4, 7001., 5.4 /)
  ! Test the functional interface
  if ( any(reals(.sort. reals) /= (/ -123., 1., 5.4, 5.4, 7001. /))) then
     call failed()
     stop
  end if

  ! Same but with a twist (sort by absolute value)
  if ( any(reals(.sort. abs(reals)) /= (/ 1., 5.4, 5.4, -123., 7001. /))) then
     call failed()
     stop
  end if

  ! Subroutine interface
  call sort(reals)
  if ( any(reals /= (/ -123., 1., 5.4, 5.4, 7001. /))) then
     call failed()
     stop
  end if

  call ok

  call announce ('Testing extracting unique reals')

  if ( any(unique(reals) /= (/ -123., 1., 5.4, 7001. /))) then
     call failed()
     stop
  end if

  call ok()

  call announce ('Testing double precision sort')

  ! real              :: reals(5) = (/ 1., -123., 5.4, 7001., 5.4 /)
  ! Test the functional interface
  if ( any(doubles(.sort. doubles) /= (/ -123.d0, 1.d0, 5.4d0, 5.4d0, 7001.d0 /))) then
     call failed()
     stop
  end if

  ! Same but with a twist (sort by absolute value)
  if ( any(doubles(.sort. abs(doubles)) /= (/ 1.d0, 5.4d0, 5.4d0, -123.d0, 7001.d0 /))) then
     call failed()
     stop
  end if

  ! Subroutine interface
  call sort(doubles)
  if ( any(doubles /= (/ -123.d0, 1.d0, 5.4d0, 5.4d0, 7001.d0 /))) then
     call failed()
     stop
  end if

  call ok

  call announce ('Testing extracting unique double precision values')

  if ( any(unique(doubles) /= (/ -123.d0, 1.d0, 5.4d0, 7001.d0 /))) then
     call failed()
     stop
  end if

  call ok()

  call announce ('Testing reverse')

  if ( any(reverse(integers) /= (/ 7001, 5, 5, 1, -123 /))) then
     call failed()
     stop
  end if

  if ( any(reverse(reals) /= (/ 7001., 5.4, 5.4, 1., -123. /))) then
     call failed()
     stop
  end if

  if ( any(reverse(doubles) /= (/ 7001.d0, 5.4d0, 5.4d0, 1.d0, -123.d0 /))) then 
     print *,reverse(doubles)
     call failed()
     stop
  end if

  strings = (/ 'aaaaab', 'aaaaab', 'aaaAba', 'aaaabA', 'Aaaaba' /)
  call sort(strings)

  if ( any(reverse(strings) /= (/'aaaabA', 'aaaaab', 'aaaaab', 'aaaAba', 'Aaaaba'/)) ) then
     write(*,'(6A10)')strings
     call failed
     stop
  end if

  call ok()


  ! Tests finished

contains 
  
  subroutine announce(text)
    character(len=*) :: text
    write (*,'(A)',advance='no') text
  end subroutine announce
  
  subroutine ok
    print *,' ........... ok'
  end subroutine ok

  subroutine failed
    print *,' ........... FAILED!'
  end subroutine failed
  
end program test1
