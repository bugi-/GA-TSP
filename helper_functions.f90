! This module includes some functions for doing things not directly related to the TSP.
module helper_functions

contains

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
end module
