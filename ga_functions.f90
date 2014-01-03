! This module contains procedures related to the genetic algorithm used.
module ga_functions
  use sizes
  use TSP_functions
  use helper_functions
  
  implicit none

  real(rk) :: mut_prob = 0.0 ! Mutation frequency. Initiliazed to 0%.
  integer :: migrators = 0 ! Number of best routes to send to neighboring populations
  
  contains
  
  function gen_new_generation(old_pop, positions) result(population)
    integer :: old_pop(:,:)
    type(pos) :: positions(:)
    integer, allocatable :: population(:,:)
    integer, allocatable :: route_temp(:)
    integer :: i, pop_size
    
    allocate(population(size(old_pop, 1), size(old_pop, 2)))
    allocate(route_temp(size(old_pop, 2)))
    pop_size = size(population, 1)
    population = old_pop
    
    route_temp = population(1,:) ! First one is saved for later use
    do i = 1, pop_size-1
      population(i,:) = create_child(population(i,:), population(i+1,:), positions)
    end do
    ! Add the child of last and first
    population(pop_size,:) = create_child(population(pop_size,:), route_temp, positions)
  end function
  
  ! Child creation process as explained in project description
  ! 
  function create_child(parent1, parent2, positions) result(child)
    type(pos) :: positions(:)
    integer :: parent1(:), parent2(:)
    integer, allocatable :: child(:)
    integer :: i, N, route_sum
    real(rk) :: ran, dist1, dist2
    integer :: par1, par2        ! Candidate cities
    integer :: prev, next, other ! Other variables containing cities
    logical :: has_1, has_2
    
    N = size(parent1)
    allocate(child(N))
    child = [(0, i = 1, N)] ! Initialize child to 0 for error checking

    route_sum = N*(1+N)/2 ! Sum of the cities' numbers in a route. Helps finding last city required.
    
    ! Choose first city randomly from a parent
    call random_number(ran)
    if (ran < 0.5_rk) then ! Equal probability
      child(1) = parent1(1)
    else
      child(1) = parent2(1)
    end if
    ! Rest of the cities
    do i = 2, N - 1 ! First and last are selected separately
      prev = child(i-1)
      ! Search for the cities following the current one
      par1 = lin_search(parent1, prev) + 1
      par2 = lin_search(parent2, prev) + 1
      ! Cyclical boundary condition
      if (par1 > N) par1 = par1 - N
      if (par2 > N) par2 = par2 - N
      ! Check which is better. The losing candidate is also stored for later reference.
      dist1 = city_distance(positions(prev), &
                     &  positions(parent1(par1)))
      dist2 = city_distance(positions(prev), &
                     &  positions(parent2(par2)))
      if (dist1 < dist2) then
        next = parent1(par1)
        other = parent2(par2)
      else
        next = parent2(par2)
        other = parent1(par1)
      end if
      if (in_array(child, next)) then ! Candidate already in child. Choose another.
        ! See if the other candidate is in child
        if (.not. in_array(child, other)) then ! Not found. The candidate can be included.
          next = other
        else ! Go trough both parents and choose one that has not been included already
          do while(.true.)
            ! Both candidates already in child, try next cities of parents
            ! See if any of them is already included
            par1 = par1 + 1
            par2 = par2 + 1
            if (par1 > N) par1 = par1 - N
            if (par2 > N) par2 = par2 - N
            if (in_array(child, parent1(par1))) then
              has_1 = .true.
            else
              has_1 = .false.
            end if
            if (in_array(child, parent2(par2))) then
              has_2 = .true.
            else
              has_2 = .false.
            end if
            ! If only one is included already, choose the other one
            if (has_1 .and. .not. has_2) then
              next = parent2(par2)
              exit
            else if (has_2 .and. .not. has_1) then
              next = parent1(par1)
              exit
            else if (has_1 .and. has_2) then ! Both included
              cycle ! Check next ones'
            else ! Both candidates are not included already. Choose one at random
              call random_number(ran)
              if (ran < 0.5_rk) then
                next = parent1(par1)
              else
                next = parent2(par2)
              end if
              exit
            end if
          end do
        end if
      end if
      !print *, prev, next, city_distance(positions(prev), positions(next)) ! Debug
      child(i) = next
    end do
    ! Choose the last city
    !print *, route_sum, sum(child)
    child(N) = route_sum - sum(child) ! route_sum is the expected sum of the arithmetic series and sum(child) is the sum without the last element
    call mutate(child)
  end function
  
  ! Mutates the given route by exchanging 2 random elements with probability of mut_prob
  subroutine mutate(route)
    integer :: route(:)
    integer :: el1, el2, temp, N
    real :: ran
    
    N = size(route)
    
    call random_number(ran)
    if (ran < mut_prob) then ! Mutate
      ! Generate random ints for indices to switch
      el1 = rand_int(N)
      el2 = rand_int(N)
      ! Exchange the elements
      temp = route(el1)
      route(el1) = route(el2)
      route(el2) = temp
    end if
  end subroutine
  
  ! Migrates best routes to neighboring populations
  subroutine migrate(populations, stats)
    integer :: populations(:,:,:)
    type(pop_stats) :: stats
    integer, allocatable :: shortest(:,:)
    integer :: pop, num_pop, num_cities, i, prev, next, ind
    
    if (migrators <= 0) then ! Skip the subprogram if no migration
      return
    end if
    
    num_pop = size(populations, 1) ! Number of populations
    num_cities = size(populations, 3)
    
    allocate(shortest(num_pop, migrators)) ! We need to save the indices of all migrators for all populations
    
    ! Find the n shortest routes first
    do pop = 1, num_pop
      shortest(pop, :) = n_smallest(stats%route_lengths(pop, :), migrators)
    end do
    
    ! Some inline testing
    
    !do pop = 1, num_pop
    !  print *, 'Population', pop
    !  do i = 1, migrators
    !    print *, stats%route_lengths(pop, shortest(pop, i))
    !  end do
    !end do

    ! Do the actual migration
    do pop = 1, num_pop
      ! Calculate where to send the best routes and check for boundaries
      prev = pop - 1
      if (prev <= 0) prev = prev + num_pop
      next = pop + 1
      if (next > num_pop) next = next - num_pop
      
      do i = 1, migrators
        ! Find an index to put a route in a neighbour. The neighbour's shortest routes can not be replaced.
        do while (.True.)
          ind = rand_int(num_cities)
          if (.not. in_array(shortest(prev, :), ind)) exit
        end do
        !Copy the route
        populations(prev, ind, :) = populations(pop, shortest(pop, i), :)
        ! Do the same to the other neighbor
        do while (.True.)
          ind = rand_int(num_cities)
          if (.not. in_array(shortest(next, :), ind)) exit
        end do
        !Copy the route
        populations(next, ind, :) = populations(pop, shortest(pop, i), :)
      end do
    end do
    
    deallocate(shortest)
  end subroutine
end module
