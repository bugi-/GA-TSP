! This module includes functions related to TSP
module TSP_functions
  use sizes
  use helper_functions
  
  implicit none

  ! Type for city positions
  type pos
    real(rk) :: x
    real(rk) :: y
  end type

  ! Lengths of all routes in all populations and indices to the shortest one. pops_stats would be more descriptive, but it sounds weird.
  type pop_stats
    integer :: min_pop ! Population where the shortest route is
    integer :: min_ind ! Index within that population
    real(rk), allocatable :: route_lengths(:,:)
  end type
  
  real(rk) :: mutation_prob = 0.0_rk ! This is set by main as read from the references file
  character(len=10) :: pos_format = '(f6.2)'
  
  contains
  
  ! Calculates the length of a route
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
  
  ! Calculates the distance between two cities
  function city_distance(city1, city2) result(res)
    type(pos) :: city1, city2
    real(rk) :: res
    res = sqrt((city1%x - city2%x)**2 + (city1%y - city2%y)**2)
    !print *, res ! Debug
  end function
  
  ! Generates a route
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
  
  ! Generates N positions with x- and y-values of 0..scale
  function gen_positions(N, scale) result(res)
    integer :: N, i
    real(rk) :: scale, ran
    type(pos), allocatable :: res(:)
    
    allocate(res(N))
    
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
  
  ! Returns the indices of the shortest route and all route lengths
  function get_stats(populations, positions) result(stats)
    integer :: populations(:,:,:)
    type(pos) :: positions(:)
    real(rk), allocatable :: route_lengths(:)
    real(rk) :: min_length
    integer :: N, min_ind, min_ind_pop, min_pop, pop
    type(pop_stats) :: stats
	
    allocate(stats%route_lengths(size(populations, 1), size(populations, 2)))
	
    min_length = huge(min_length)
    min_pop = -1
    min_ind = -1

    N = size(populations, 2) ! Size of 2. dim (number of cities in a population)
    allocate(route_lengths(N))
    do pop = 1, size(populations, 1) ! This size is the number of different populations
      route_lengths = calc_route_lengths(populations(pop, :, :), positions)
      stats%route_lengths(pop, :) = route_lengths
      min_ind_pop = minloc(route_lengths, 1)
      if (route_lengths(min_ind_pop) < min_length) then
        min_pop = pop
        min_ind = min_ind_pop
        min_length = route_lengths(min_ind)
      end if
    end do
    stats%min_pop = min_pop
    stats%min_ind = min_ind
    deallocate(route_lengths)
  end function
  
  ! Prints information from the statistics type
  subroutine print_stats(stats)
    type(pop_stats) :: stats
    real(rk) :: mean
    integer :: N
    
    N = size(stats%route_lengths)
    mean = sum(stats%route_lengths) / N
    
    print *, 'Min length:', stats%route_lengths(stats%min_pop, stats%min_ind), &
    & 'in population', stats%min_pop, 'index', stats%min_ind
    ! This is the usual formula for corrected standard deviation of a sample. The formula is std_dev^2 = sum(i=1..N)(x_i - x_mean)^2 / (N-1)
    print *, 'Std dev:    ', sqrt(sum((stats%route_lengths - mean)**2) / (N-1))
    print *, ''
  end subroutine
end module
