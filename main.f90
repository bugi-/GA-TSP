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
  integer :: seed ! Seed for RNG
  
  type(pos), allocatable :: positions(:)
  integer, allocatable :: population(:,:)
  integer, allocatable :: pop_temp(:) ! One route from the population must be saved
  real(rk), allocatable :: route_lengths(:)
  real :: t0, t1

  integer :: id,ntasks,source_id,dest_id,rc ! MPI related

  ! Start mpi
  call mpi_init(rc)
  if (rc/=mpi_success) then
    print *,’MPI initialization failed.’
    stop
  end if

  call mpi_comm_size(mpi_comm_world,ntasks,rc)
  call mpi_comm_rank(mpi_comm_world,id,rc)

  t0 = MPI_Wtime()
  
  ! Root handles reading the file
  if (id == 0) then
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
    read (unt, *) mut_freq ! Set the value from preferences to module specific variable
    read (unt, *) seed
    print *, 'Read preferences from file', pref_file
  
    print *, 'Printing every', print_freq, ' generations.'
    print *, ''
  end if
  
  call mpi_bcast(N,1,MPI_INTEGER,0,mpi_comm_world,rc)
  call mpi_bcast(pop_size,1,MPI_INTEGER,0,mpi_comm_world,rc)
  call mpi_bcast(MAX_GEN,1,MPI_INTEGER,0,mpi_comm_world,rc)
  call mpi_bcast(seed,1,MPI_INTEGER,0,mpi_comm_world,rc)
  
  ! Allocation
  !allocate(positions(N))
  allocate(population(pop_size, N))
  allocate(pop_temp(N))
  allocate(route_lengths(N))
  
  ! Set the seed
  seed = seed + id ! Put some offset to get individual seeds for all ids
  call set_seed(seed)
  
  ! Generate positions
  positions = gen_positions(N, 1.0_rk)
  !call print_positions(positions)
  
  ! Generate the population
  do i = 1, pop_size
    population(i,:) = gen_route(N)
  end do
  print *, 'Initial state:'
  if (print_freq > 0) then
    call print_stats(population, positions)
  end if
  
  print *, 'Starting simulation'
  ! Loop over generations
  do gen = 1, MAX_GEN
    ! Generate next generation of the population by replacing i with the child of i and i+1.
    pop_temp = population(1,:) ! First one is saved for later use
    do i = 1, pop_size-1
      population(i,:) = create_child(population(i,:), population(i+1,:), positions)
    end do
    ! Add the child of last and first
    population(pop_size, :) = create_child(population(pop_size, :), pop_temp, positions)
    ! Print some stats at given intervals
    if (modulo(gen, print_freq) == 0) then
      print *, 'Generation', gen
      call print_stats(population, positions)
    end if
  end do
  
  t0 = MPI_Wtime()
  
  print *, 'Time taken by program:', t1 - t0
  print *, 'Ho ho ho!'
end program
