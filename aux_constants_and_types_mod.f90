module aux_constants_and_types
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: sp, dp, kind_cplx, imag_i, vector
    integer, parameter :: sp=REAL32, & ! Single precision
                          dp=REAL64, & ! Double precision
                          kind_cplx = dp

    complex(kind=kind_cplx) :: imag_i=cmplx(0.0, 1.0, kind=kind_cplx)
    type vector
        real(kind=dp), dimension(1:3) :: comp
    end type vector

end module aux_constants_and_types
