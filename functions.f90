module functions
  use sizes
  !use ga_functions
  implicit none
  type pos          ! Type for city positions
    real(rk) :: x
    real(rk) :: y
  end type
  real(rk) :: mutation_prob
  
  contains
  
  function route_length(route, positions) result(res)
    integer :: route(:)
    type(pos) :: positions(:)
    integer :: i, N
    real(rk) :: res
    
    res = 0.0
    N = size(route)
    ! Calculate the distance from first to last city
    do i = 1, N - 1
      res = res + city_distance(positions(route(i)), positions(route(i+1)))
    end do
    ! Add distance from last back to first city
    res = res + city_distance(positions(route(N)), positions(route(1)))
  end function
  
  function city_distance(city1, city2) result(res)
    type(pos) :: city1, city2
    real(rk) :: res
    res = sqrt((city1%x - city2%x)**2 + (city1%y - city2%y)**2)
    !print *, res ! Debug
  end function
  
  function gen_route(N) result(res)
    integer, intent(in) :: N
    integer, dimension(N) :: res
    integer :: i
    ! Initialize
    do i = 1, N
      res(i) = i
    end do
    call shuffle(res)
  end function

  subroutine shuffle(a) ! Fisher-Yates shuffle implementation from http://rosettacode.org/wiki/Knuth_shuffle
    integer, intent(inout) :: a(:)
    integer :: i, randpos, temp
    real :: r
 
    do i = size(a), 2, -1
      call random_number(r)
      randpos = int(r * i) + 1
      temp = a(randpos)
      a(randpos) = a(i)
      a(i) = temp
    end do
 
  end subroutine
  
  ! Generates N positions with x- and y-values of 0..scale
  function gen_positions(N, scale) result(res)
    integer :: N, i
    real(rk) :: scale, ran
    type(pos) :: res(N)
    
    do i = 1, N
      call random_number(ran)
      res(i)%x = scale * ran
      call random_number(ran)
      res(i)%y = scale * ran
    end do
        
  end function
  
  subroutine print_positions(positions)
    integer :: i
    type(pos) :: positions(:)
    ! Formatted for easy input into Python plotting
    write (*,'(a)', advance='no') 'x = ['
    do i = 1, size(positions)-1
      write (*,'(f6.2,a)', advance='no') positions(i)%x, ','
    end do
    write (*,'(f6.2)', advance='no') positions(i)%x ! No comma after the last element
    write (*,'(a)') ']'
    
    write (*,'(a)', advance='no') 'y = ['
    do i = 1, size(positions)-1
      write (*,'(f6.2,a)', advance='no') positions(i)%y, ','
    end do
    write (*,'(f6.2)', advance='no') positions(i)%y
    write (*,'(a)') ']'
  end subroutine
  
end module
