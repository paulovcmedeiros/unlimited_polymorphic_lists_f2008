!! Copyright (C) 2013-2017 Paulo V. C. Medeiros
!!
!! This program is free software: you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation, either version 3 of the License, or
!! (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module storage_size_wrappers
    use aux_constants_and_types, only : dp
    implicit none
    private
    public :: bits_to_MB, array_storage_size_in_MB

    interface bits_to_MB
        module procedure bits_to_MB_long_int, bits_to_MB_regular_int
    end interface bits_to_MB

    contains

    function bits_to_MB_long_int(val_in_bits) result(val_in_MB)
    ! Adopts the 2^n conversion factor convention
    implicit none
    real(kind=dp) :: val_in_MB
    integer(kind=selected_int_kind(15)), intent(in) :: val_in_bits

        val_in_MB = real(val_in_bits, kind=dp) / real(8 * 1024**2, kind=dp)

    end function bits_to_MB_long_int
    function bits_to_MB_regular_int(val_in_bits) result(val_in_MB)
    ! Adopts the 2^n conversion factor convention
    implicit none
    real(kind=dp) :: val_in_MB
    integer, intent(in) :: val_in_bits
    ! Local vars
    integer(kind=selected_int_kind(15)) :: val_in_bits_long

        val_in_bits_long = int(val_in_bits, kind=kind(val_in_bits_long))
        val_in_MB = bits_to_MB_long_int(val_in_bits_long)

    end function bits_to_MB_regular_int

    function array_storage_size_in_MB(array) result(rtn)
    implicit none
    class(*), dimension(:), intent(in) :: array
    real(kind=dp) :: rtn
    ! Local variables
    integer(kind=selected_int_kind(15)) :: memsize_in_bits    

        memsize_in_bits = size(array, kind=kind(memsize_in_bits)) * &
                          storage_size(array, kind=kind(memsize_in_bits))
        rtn = bits_to_MB(memsize_in_bits)

    end function array_storage_size_in_MB

end module storage_size_wrappers
