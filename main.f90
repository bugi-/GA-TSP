program main
  use sizes
  use TSP_functions
  use ga_functions
  !$ use omp_lib
  implicit none
  
  integer, parameter :: pref_unit = 20 ! Unit for preferences file
  character(len=20) :: pref_file = 'preferences' ! Name of preferences file
  integer, parameter :: output_unit = 30 ! Unit for writing cordinates
  character(len=20) :: output_file = 'main.out'
  type(pop_stats) :: stats! Used for output
  integer :: i, j, gen, iost
  integer :: n_threads ! Number of threads in OpenMP run
  integer :: num_cities ! Number of cities
  integer :: num_generations ! Maximum number of generations
  integer :: print_freq ! Printing frequency
  integer :: migration_freq ! Frequency of migration between populations
  integer :: pop_size ! Population size
  integer :: num_pop ! Number of populations
  integer :: seed ! Seed for RNG
  integer :: write_to_file ! >0 to enable
  
  type(pos), allocatable :: positions(:)
  integer, allocatable :: populations(:,:,:) ! 3d because every individual population is 2d
  integer, allocatable :: population(:,:) ! A single population
  real(rk), allocatable :: route_lengths(:)
  real(rk) :: t0, t1

  call cpu_time(t0)
  !$ t0 = omp_get_wtime()
  
  ! Open preferences file and read it's contents
  open(pref_unit, file=pref_file, status='old', iostat=iost)
  if (iost /= 0) then
    print *, 'Something wrong with preferences file! Quitting.'
    stop
  end if
  
  read (pref_unit, *)
  read (pref_unit, *) n_threads
  read (pref_unit, *) num_cities
  read (pref_unit, *) pop_size
  read (pref_unit, *) num_pop
  read (pref_unit, *) num_generations
  read (pref_unit, *) mut_prob ! Set the value from preferences to module specific variable
  read (pref_unit, *) migration_freq
  read (pref_unit, *) migrators ! Once again set the variable in module
  read (pref_unit, *) seed
  read (pref_unit, *) print_freq
  read (pref_unit, *) write_to_file
  close(pref_unit)
  
  ! Open output file if needed
  if (write_to_file > 0) then
    open(output_unit, file=output_file, status='replace', iostat=iost)
    if (iost /= 0) then
      print *, 'Can not open output file! Quitting.'
      stop
    end if
  end if
  
  ! Allocation
  allocate(populations(num_pop, pop_size, num_cities))
  allocate(population(pop_size, num_cities))
  allocate(route_lengths(num_cities))

  !$ call omp_set_num_threads(n_threads)
  
  call set_seed(seed)
  
  print *, 'Read preferences from file ', pref_file
  print *, 'Printing every', print_freq, ' generations.'
  !$ n_threads = omp_get_num_threads()
  !$ print *, 'Using ', n_threads, 'threads.'
  print *, ''
  print *, 'Starting...'
  print *, ''
  
  ! Generate positions
  positions = gen_positions(num_cities, 1.0_rk)
  !call print_positions(positions)
  
  ! Generate the population
  do i = 1, num_pop
    do j = 1, pop_size
      populations(i, j, :) = gen_route(num_cities)
    end do
  end do
  if (print_freq /= 0) then ! Print if that is enabled
    print *, 'Generation 0'
    stats = get_stats(populations, positions)
    call print_stats(stats)
    if (write_to_file > 0) then
      write(output_unit, '(a)') 'Generation   0'
      call write_route(output_unit, populations(stats%min_pop, stats%min_ind, :), positions)
    end if
  end if
  
  ! Loop over generations
  do gen = 1, num_generations
    ! Generate next generation of the population by replacing i with the child of i and i+1.
    !$omp parallel do private(population, i) shared(populations)
    do j = 1, num_pop
      population = populations(j,:,:) ! Get the current population
      population = gen_new_generation(population, positions)
      populations(j,:,:) = population ! Put the updated population back
    end do
    !$omp end parallel do
    ! Print some stats at given intervals
    if (modulo(gen, print_freq) == 0) then
      print *, 'Generation', gen
      stats = get_stats(populations, positions)
      call print_stats(stats)
      if (write_to_file > 0) then
        write(output_unit, '(a,i5)') 'Generation', gen
        call write_route(output_unit, populations(stats%min_pop, stats%min_ind, :), positions)
      end if
    end if
    ! Do migration
    if (modulo(gen, migration_freq) == 0) then
      stats = get_stats(populations, positions)
      call migrate(populations, stats)
    end if
  end do
  
  close(output_unit)
  
  deallocate(populations)
  deallocate(population)
  deallocate(route_lengths)
  
  call cpu_time(t1)
  !$ t1 = omp_get_wtime()
  
  print *, 'Time taken by program:', t1 - t0
end program
