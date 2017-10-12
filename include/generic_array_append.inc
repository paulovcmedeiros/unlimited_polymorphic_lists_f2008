!==========================================================================
!> \ingroup array_append_routines
!==========================================================================
subroutine SPECIFIC_ARRAY_APPEND_SUBROUTINE(array, val)
implicit none
#ifdef ARRAY_IS_CHAR
    character(len=:), allocatable, intent(inout) :: array(:)
    character(len=:), allocatable :: aux_array(:)
    character(len=*), intent(in) :: val
#else
    ARRAY_TYPE, dimension(:), allocatable, intent(inout) :: array
    ARRAY_TYPE, dimension(:), allocatable :: aux_array
    ARRAY_TYPE, intent(in) :: val
#endif
integer :: orig_size, new_size

    if(allocated(array))then
        orig_size = size(array)
        new_size = orig_size + 1
#       ifdef ARRAY_IS_CHAR
            allocate(&
                character(len=len(array(1))) :: aux_array(1:new_size) &
            )
#       else
            allocate(aux_array(1:new_size))
#       endif
        aux_array(1:orig_size) = array
        aux_array(new_size) = val
        call move_alloc(from=aux_array, to=array)
    else
#       ifdef ARRAY_IS_CHAR
            allocate(&
                character(len=len(val)) :: array(1:1) &
            )
#       else
            allocate(array(1:1))
#       endif
        array(1) = val
    endif

end subroutine SPECIFIC_ARRAY_APPEND_SUBROUTINE
