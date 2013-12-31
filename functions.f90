module functions
  use sizes
  !use ga_functions
  implicit none
  type pos          ! Type for city positions
    real(rk) :: x
    real(rk) :: y
  end type
  real(rk) :: mutation_prob
  character(len=10) :: pos_format = '(f6.2)'
  
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
  
  ! Convenience function for calculating all route lengths in a population
  function calc_route_lengths(population, positions) result(res)
    integer :: population(:,:)
    type(pos) :: positions(:)
    real(rk), allocatable :: res(:)
    integer :: i, pop_size
    
    pop_size = size(population, 1)
    allocate(res(pop_size))
    do i = 1, pop_size
      res(i) = route_length(population(i,:), positions)
    end do
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
  
  ! Prints the locations of cities' x- and y-cordinates
  subroutine print_positions(positions)
    integer :: i
    type(pos) :: positions(:)
    ! Formatted for easy input into Python plotting
    write (*,'(a)', advance='no') 'x = ['
    do i = 1, size(positions)-1
      write (*,'(f6.2,a)', advance='no') positions(i)%x, ','
    end do
    write (*,pos_format, advance='no') positions(i)%x ! No comma after the last element
    write (*,'(a)') ']'
    
    write (*,'(a)', advance='no') 'y = ['
    do i = 1, size(positions)-1
      write (*,'(f6.2,a)', advance='no') positions(i)%y, ','
    end do
    write (*,pos_format, advance='no') positions(i)%y
    write (*,'(a)') ']'
  end subroutine

  ! Writes the cordinates of cities in a given route in the right order to a file.
  subroutine write_route(unt, route, positions)
    integer :: route(:), i, unt ! unt if unit to write to
    type(pos) :: positions(:)
    
    ! Formatted for easy input into Python plotting
    write (unt,'(a)', advance='no') 'x = ['
    do i = 1, size(positions)
      write (unt,'(f6.2,a)', advance='no') positions(route(i))%x, ','
    end do
    write (unt,pos_format, advance='no') positions(route(1))%x ! Write the first one again to make the plot return to start. No comma after the last element.
    write (unt,'(a)') ']'
    
    write (unt,'(a)', advance='no') 'y = ['
    do i = 1, size(positions)
      write (unt,'(f6.2,a)', advance='no') positions(route(i))%y, ','
    end do
    write (unt,pos_format, advance='no') positions(route(1))%y
    write (unt,'(a)') ']'
  end subroutine
  
  ! Prints some information about current populations and returns the index of shortest route
  function get_min_and_print_stats(populations, positions) result(indices)
    integer :: populations(:,:,:)
    type(pos) :: positions(:)
    real(rk), allocatable :: route_lengths(:)
    real(rk) :: min_length
    integer :: N, min_ind, min_ind_pop, min_pop, pop
    integer :: indices(2)
	
	min_length = huge(min_length)
	min_pop = -1
	min_ind = -1
	
    N = size(populations, 2) ! Size of 2. dim (number of cities in a population)
    allocate(route_lengths(N))
    do pop = 1, size(populations, 1) ! This size is the number of different populations
      route_lengths = calc_route_lengths(populations(pop, :, :), positions)
      min_ind_pop = minloc(route_lengths, 1)
      if (route_lengths(min_ind_pop) < min_length) then
        min_pop = pop
        min_ind = min_ind_pop
        min_length = route_lengths(min_ind)
      end if
    end do
    print *, 'Min length:', min_length
    indices(1) = min_pop; indices(2) = min_ind
    !print *, 'Std dev:   ', sqrt((sum(route_lengths**2)-sum(route_lengths)**2/size(route_lengths))/(size(route_lengths)-1))
    deallocate(route_lengths)
  end function
  
  ! Sets the whole seed for intrinsic RNG with a single integer. Uses the gnu extension irand function for setting random_seed().
  subroutine set_seed(s)
    integer :: s, seed_size, i
    integer, allocatable :: seed(:)
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    call srand(s)
    do i = 1, seed_size
      seed(i) = irand()
    end do
    call random_seed(put=seed)
    deallocate(seed)
  end subroutine
end module
