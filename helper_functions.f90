! This module includes some functions for doing things not directly related to the TSP.
module helper_functions
  use sizes
contains

  ! Returns a random integer in the range 1..max_val
  function rand_int(max_val) result(res)
    integer :: max_val, res
    real(rk) :: ran
    call random_number(ran)
    res = int(ran * max_val) + 1
  end function

  ! Shuffles the input list
  subroutine shuffle(a) ! Fisher-Yates shuffle implementation from http://rosettacode.org/wiki/Knuth_shuffle
    integer, intent(inout) :: a(:)
    integer :: i, randpos, temp
 
    do i = size(a), 2, -1
      randpos = rand_int(i)
      temp = a(randpos)
      a(randpos) = a(i)
      a(i) = temp
    end do
  end subroutine
  
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
  
  ! Convenience function to check if given element in given array. Returns .true. if the element is found and .false. if not.
  function in_array(array, element) result(res)
    integer :: array(:), element
    logical :: res
    res = .false.
    if (lin_search(array, element) > 0) res = .true.
  end function
  
  ! Finds and returns an array of indices of n smallest values in array. The method used is not very fast, but it should not be a problem because n should always be small.
  function n_smallest(array, n) result(res)
    real(rk) :: array(:)
    integer :: n, i
    logical, allocatable :: mask(:)
    integer, allocatable :: res(:)
    
    allocate(res(n))
    allocate(mask(size(array)))
    mask = .True.
    
    do i = 1, n
      res(i) = minloc(array, 1, mask) ! Use intrinsic to find minimum value
      mask(res(i)) = .False. ! Remove the saved indices from future searches
    end do
  end function
end module
