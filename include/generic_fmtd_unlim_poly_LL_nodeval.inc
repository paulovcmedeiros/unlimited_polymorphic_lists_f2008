select type(val=>dtv%val)
    ! Intrinsic types (including their variants used here)
    type is (integer)
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (real)
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (real(kind=dp))
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (complex)
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (complex(kind=dp))
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (logical)
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val
    type is (character(len=*))
        ! I've added the preproc. conditional here to be able to use 'quote'
#       ifdef ASTERISK_FMT_SPEC
            write(unit, *, iostat=iostat, iomsg=iomsg, delim='quote') &
                trim(adjustl(val))
#       else
            write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) &
                trim(adjustl(val))
#       endif
    class is (vector)
        ! User-defined types work too. This can be useful to
        ! store non-scalar variables of any dimension\rank
        ! without having to modify the structure of the list 
        ! nodes. Using class instead of type allows this to work
        ! with extensions of the "vector" type
        write(unit, SPECIFIC_FMT_SPEC, iostat=iostat, iomsg=iomsg) val%comp(:)
    class default
        iostat = -3
        iomsg = new_line("A")//"ERROR: "//&
                "Could not print node value: "//&
                "Unsupported data type for output."//&
                new_line("A")//&
                "Please edit the file "//&
                '"generic_fmtd_unlim_poly_LL_nodeval.inc"'//&
                new_line("A")//&
                "and recompile the code to add support to output of new types."
end select
