module ga_functions
  use sizes
  use functions
  implicit none

  real(rk) :: mut_freq = 0.0 ! Mutation frequency. Initiliazed to 0%.
  
  contains
  ! Child creation process as explained in project description
  !!!! Could use some refactoring!!!
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
      !print *, prev, next ! Debug
      child(i) = next
    end do
    ! Choose the last city
    !print *, route_sum, sum(child)
    child(N) = route_sum - sum(child) ! route_sum is the expected sum of the arithmetic series and sum(child) is the sum without the last element
    call mutate(child)
  end function
  
  ! Mutates the given route by exchanging 2 random elements with probability of mut_freq
  subroutine mutate(route)
    integer :: route(:)
    integer :: el1, el2, temp, N
    real :: ran
    
    N = size(route)
    
    call random_number(ran)
    if (ran < mut_freq) then ! Mutate
      ! Generate random ints for indexes to switch
      call random_number(ran)
      el1 = int(ran*(N))+1
      call random_number(ran)
      el2 = int(ran*(N))+1
      ! Exchange the elements
      temp = route(el1)
      route(el1) = route(el2)
      route(el2) = temp
    end if
  end subroutine
  
  ! Performs linear search of array for element. Returns index of (first instance of) element or -1 if not found.
  function lin_search(array, element) result(ind)
    integer :: array(:), ind, i, element
    
    do i = 1, size(array)
      if (array(i) == element) then
        ind = i
        return
      end if
    end do
    ! Not found
    ind = -1
  end function
  
  ! Convenience function to check if given element in given array. Returns if .true. if the element is found and .false. if not.
  function in_array(array, element) result(res)
    integer :: array(:), element
    logical :: res
    res = .false.
    if (lin_search(array, element) > 0) res = .true.
  end function
end module
