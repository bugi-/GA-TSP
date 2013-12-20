program test_suite
  use sizes
  use functions
  use ga_functions
  implicit none
  
  call test_route_length()
  print *, ''
  call test_gen_route()
  print *, ''
  call test_gen_positions()
  print *, ''
  call test_create_child()
  contains
  subroutine test(value, expected)
    real, parameter :: eps = 0.0001
    real(rk) :: value, expected

    print *, 'Result   ', value
    print *, 'Expected ', expected
    if (abs(value - expected) < eps) then ! Float equality check
      print *, 'Passed'
    else
      print *, 'Failed test!'
      stop
    end if
  end subroutine
  subroutine test_route_length() ! Some simple tests to get results right
    integer, parameter :: N = 5
    type(pos), dimension(:), allocatable :: positions
    integer, dimension(:), allocatable :: route
    real(rk) :: res

    allocate(positions(N))
    allocate(route(N)) ! No need for implicit return to first  
    ! First a rather trivial case to check distance calculation
    route = (/1,2,3,4,5/)
    positions(1)%x = 0.0; positions(1)%y = 0.0
    positions(2)%x = 1.0; positions(2)%y = 0.0
    positions(3)%x = 1.0; positions(3)%y = 1.0
    positions(4)%x = 1.0; positions(4)%y = 2.0
    positions(5)%x = 2.0; positions(5)%y = 2.0
    res = route_length(route, positions)
    print *, 'Route 1:'
    call test(res, 4 + 2*sqrt(2.0_rk))
    
    ! Reverse should be of equal length
    route = (/5,4,3,2,1/)
    print *, 'Route 2:'
    res = route_length(route, positions)
    call test(res, 4 + 2*sqrt(2.0_rk))
    
    ! Now shuffle a bit
    route = (/5,3,4,1,2/)
    print *, 'Route 3:'
    res = route_length(route, positions)
    call test(res, sqrt(2.0_rk) + 1 + sqrt(5.0_rk) + 1 + sqrt(5.0_rk)) ! The distances are in lexical order    
    print *, 'Route distance checks passed!'

  end subroutine
  
  subroutine test_gen_route()
    integer :: N, i
    integer, dimension(:), allocatable :: route
    
    print *, 'Printing some random routes:'
    do i = 4, 12
      allocate(route(i))
      route = gen_route(i)
      write (*,'(*(i3.1))') route
      deallocate(route)
    end do
  end subroutine
  
  subroutine test_gen_positions()
    integer, parameter :: N = 10
    integer :: i
    real(rk) :: scale = 10.0
    type(pos) :: positions(N)
    
    positions = gen_positions(N, scale)
    
    print *, 'Printing cordinates of positions'
    call print_positions(positions)
  end subroutine
  
  subroutine test_create_child()
    integer, parameter :: N = 10
    type(pos) :: positions(N)
    integer, dimension(N) :: parent1, parent2, child
    
    parent1 = gen_route(N)
    parent2 = gen_route(N)
    positions = gen_positions(N, 1.0_rk)
    print *, 'Parents:'
    write (*,'(*(i3.1))', advance='no') parent1
    print *, 'Length:', route_length(parent1, positions)
    write (*,'(*(i3.1))', advance='no') parent2
    print *, 'Length:', route_length(parent2, positions)
    child = create_child(parent1, parent2, positions)
    print *, 'Child:'
    write (*,'(*(i3.1))', advance='no') child
    print *, 'Length:', route_length(child, positions)
    if (.not. is_valid_route(child)) print *, 'Problem with child!'
    if (.not. is_valid_route(parent1)) print *, 'Problem with parent1!'
    if (.not. is_valid_route(parent2)) print *, 'Problem with parent2!'
  end subroutine
  
  function is_valid_route(route) result(res)
    logical :: res
    integer :: route(:)
    integer :: i
    res = .true.
    do i = 1, size(route)
      if (.not. in_array(route, i)) then
        res = .false.
        return
      end if
    end do
  end function
end program
