program main
  use sizes
  use functions
  use ga_functions
  implicit none
  
  integer, parameter :: pref_unit = 20 ! Unit for preferences file
  character(len=20) :: pref_file = 'preferences' ! Name of preferences file
  integer, parameter :: output_unit = 30 ! Unit for writing cordinates
  character(len=20) :: output_file = 'main.out'
  integer :: shortest_indices(2) ! Used for output
  integer :: i, j, gen, iost
  integer :: N ! Number of cities
  integer :: MAX_GEN ! Maximum number of generations
  integer :: print_freq ! Printing frequency
  integer :: pop_size ! Population size
  integer :: num_pop ! Number of populations
  integer :: seed ! Seed for RNG
  integer :: write_to_file ! >0 to enable
  
  type(pos), allocatable :: positions(:)
  integer, allocatable :: populations(:,:,:) ! 3d because every individual population is 2d
  integer, allocatable :: pop_temp(:) ! One route from the population must be saved
  real(rk), allocatable :: route_lengths(:)
  real :: t0, t1

  call cpu_time(t0)
  
  ! Open preferences file and read it's contents
  open(pref_unit, file=pref_file, status='old', iostat=iost)
  if (iost /= 0) then
    print *, 'Something wrong with preferences file! Quitting.'
    stop
  end if
  
  read (pref_unit, *)
  read (pref_unit, *) N
  read (pref_unit, *) pop_size
  read (pref_unit, *) num_pop
  read (pref_unit, *) MAX_GEN
  read (pref_unit, *) print_freq
  read (pref_unit, *) mut_freq ! Set the value from preferences to module specific variable
  read (pref_unit, *) seed
  read (pref_unit, *) write_to_file
  close(pref_unit)
  
  print *, 'Read preferences from file', pref_file
  print *, 'Printing every', print_freq, ' generations.'
  print *, ''
  print *, 'Starting'
  
  ! Open output file if needed
  if (write_to_file > 0) then
    open(output_unit, file=output_file, status='replace', iostat=iost)
    if (iost /= 0) then
      print *, 'Can not open output file! Quitting.'
      stop
    end if
  end if
  
  ! Allocation
  !allocate(positions(N))
  allocate(populations(num_pop, pop_size, N))
  allocate(pop_temp(N))
  allocate(route_lengths(N))
  
  ! Set the seed
  call set_seed(seed)
  
  ! Generate positions
  positions = gen_positions(N, 1.0_rk)
  !call print_positions(positions)
  
  ! Generate the population
  do i = 1, num_pop
    do j = 1, pop_size
      populations(i, j, :) = gen_route(N)
    end do
  end do
  if (print_freq /= 0) then
    shortest_indices = get_min_and_print_stats(populations, positions)
    if (write_to_file > 0) then
      call write_route(output_unit, populations(shortest_indices(1), shortest_indices(2), :), positions)
    end if
  end if
  
  ! Loop over generations
  do gen = 1, MAX_GEN
    ! Generate next generation of the population by replacing i with the child of i and i+1.
    !$omp parallel do
    do j = 1, num_pop
      pop_temp = populations(j,1,:) ! First one is saved for later use
      do i = 1, pop_size-1
        populations(j,i,:) = create_child(populations(j,i,:), populations(j,i+1,:), positions)
      end do
      ! Add the child of last and first
      populations(j,pop_size,:) = create_child(populations(j,pop_size,:), pop_temp, positions)
    end do
    ! Print some stats at given intervals
    if (modulo(gen, print_freq) == 0) then
      print *, 'Generation', gen
      shortest_indices = get_min_and_print_stats(populations, positions)
      if (write_to_file > 0) then
        call write_route(output_unit, populations(shortest_indices(1), shortest_indices(2), :), positions)
      end if
    end if
  end do
  
  close(output_unit)
  
  call cpu_time(t1)
  
  print *, 'Time taken by program:', t1 - t0
end program
