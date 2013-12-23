program main
  use sizes
  use functions
  use ga_functions
  implicit none
  integer, parameter :: unt = 20 ! Unit for preferences file
  integer :: i, gen, iost
  character(len=20) :: pref_file = 'preferences'
  integer :: N ! Number of cities
  integer :: MAX_GEN ! Maximum number of generations
  integer :: print_freq ! Printing frequency
  integer :: pop_size ! Population size
  
  type(pos), allocatable :: positions(:)
  integer, allocatable :: population(:,:)
  real(rk), allocatable :: route_lengths(:)
  
  ! Open preferences file and read it's contents
  open(unt, file=pref_file, status='old', iostat=iost)
  if (iost /= 0) then
    print *, 'Something wrong with preferences file! Quitting.'
    stop
  end if
  read (unt, *) N
  read (unt, *) pop_size
  read (unt, *) MAX_GEN
  read (unt, *) print_freq
  read (unt, *) mutation_prob
  
  mut_freq = mutation_prob ! Set the value from preferences to module specific variable
  
  ! Allocation
  !allocate(positions(N))
  allocate(population(pop_size, N))
  allocate(route_lengths(N))
  
  ! Generate positions
  positions = gen_positions(N, 1.0_rk)
  !call print_positions(positions)
  
  ! Generate the population
  do i = 1, pop_size
    population(i,:) = gen_route(N)
  end do
  if (print_freq > 0) then
    call print_stats(population, positions)
  end if
  
  ! Loop over generations
  do gen = 1, MAX_GEN
    do i = 1, pop_size
      
    end do
    ! Print some stats at given intervals
    if (modulo(i, print_freq) == 0) then
      route_lengths = calc_route_lengths(population, positions)
      print *, 'Min length:', minval(route_lengths)
      print *, 'Std dev:   ', sqrt((sum(route_lengths**2)-sum(route_lengths)**2/size(route_lengths))/(size(route_lengths)-1))
    end if
  end do
  
end program
