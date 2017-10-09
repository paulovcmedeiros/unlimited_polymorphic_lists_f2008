program test_lists
use aux_constants_and_types, only : dp, kind_cplx, vector, imag_i
use storage_size_wrappers, only : array_storage_size_in_MB
use unlimited_polymorphic_lists, only : &
    doubly_linked_list, &
    indexed_doubly_linked_list, &
    doubly_linked_list_node, &
    append_to_array
implicit none
type(doubly_linked_list) :: list
type(indexed_doubly_linked_list) :: indexed_list
class(*), pointer :: ptr=>null()
class(doubly_linked_list_node), pointer :: ptr_node=>null()
! nagfor fails if using the unlimited polymorphic pointer above to access
! individual elements of "char_array" (below). I'll use char_ptr untill
! this is resolved.
character(:), pointer :: char_ptr
integer :: i, j, n_nodes
integer, dimension(:), allocatable, target :: int_array
real(kind=dp), dimension(:), allocatable, target :: real_array
complex(kind=kind_cplx), dimension(:), allocatable, target :: complex_array
type(vector) :: my_vector
character(len=8), parameter :: demo_string='A string'
character(len=:), allocatable, target :: char_array(:)
!integer, parameter :: n_nodes=200000
integer, parameter :: max_n_nodes=100000
integer, parameter :: n_items_to_print=10
real(kind=dp) :: t_start, t_end, rand_num, rand_num2, rand_num3, &
                 rand_float, rand_float2
!
integer, dimension(:), allocatable :: rand_int_array
logical, dimension(:), allocatable :: rand_logical_array
real(kind=dp), dimension(:), allocatable :: rand_float_array
complex(kind=kind_cplx), dimension(:), allocatable :: rand_cplx_array
type(vector), dimension(:), allocatable :: rand_vector_array
character(len=64), dimension(:), allocatable :: rand_char_array
character(len=:), allocatable :: rand_str

! Create one max_n_nodes-long random array for each type
allocate(rand_int_array(1:max_n_nodes))
allocate(rand_logical_array(1:max_n_nodes))
allocate(rand_float_array(1:max_n_nodes))
allocate(rand_cplx_array(1:max_n_nodes))
allocate(rand_vector_array(1:max_n_nodes))
allocate(rand_char_array(1:max_n_nodes))
do i=1, max_n_nodes
    call random_number(rand_num)
    call random_number(rand_num2)
    rand_float = max_n_nodes * rand_num
    rand_float2 = max_n_nodes * rand_num2
    call random_number(rand_num3)
    if(rand_num3<0.25)then
        rand_float = -1.0_dp * rand_float
        rand_float2 = -1.0_dp * rand_float2
    else if(rand_num3>=0.25 .and. rand_num3<0.5)then
        rand_float = -1.0_dp * rand_float
    else if(rand_num3>=0.5 .and. rand_num3<0.75)then
        rand_float2 = -1.0_dp * rand_float2
    endif

    rand_int_array(i) = nint(rand_float)
    rand_float_array(i) = rand_float
    rand_cplx_array(i) = rand_float + imag_i*rand_float2
    rand_vector_array(i)%comp(:) = &
        [rand_float, rand_float2, sqrt(abs(rand_float*rand_float2))]
   
    call random_number(rand_num)
    allocate(character(len=nint(126*rand_num)+1) :: rand_str)
    do j=1, len(rand_str)
        call random_number(rand_num)
        rand_str(j:j) = char(nint(78.0*rand_num + 36))
    enddo
    rand_char_array(i)(:) = rand_str(:)
    deallocate(rand_str)

enddo

! Choose a random number of elements
call random_number(rand_num)
n_nodes = nint(max_n_nodes * rand_num)

time_link_list = 0.0_dp
time_int_array = 0.0_dp
time_logical_array = 0.0_dp
time_real_array = 0.0_dp
time_cplx_array = 0.0_dp
time_char_array = 0.0_dp

! Populate list with items of different types chosen at random
print "(A,I0,A)", "Number of elements in the list: ",n_nodes," elements"
print *, ''
! Building lists
call cpu_time(t_start)
do i=1, n_nodes
    ! Appending to linked list
    call random_number(rand_num)
    if(rand_num<0.2)then
        call list%append(rand_int_array(i))
    else if(rand_num>=0.2 .and. rand_num<0.4)then
        call list%append(rand_float_array(i))
    else if(rand_num>=0.4 .and. rand_num<0.6)then
        call list%append(rand_cplx_array(i))
    else if(rand_num>=0.6 .and. rand_num<0.8)then
        call list%append(rand_char_array(i))
    else
        call list%append(rand_vector_array(i))
    endif
    ! Appending to regular arrays
    ! Deciding whether to stop or not
    call random_number(rand_num)
enddo

print "(A,I0,A)",&
    "Printing the first ", n_items_to_print," items of the list: "
do i=1, min(n_items_to_print, list%len)
    print *, list%item(i) ! TEST
enddo
print *, ''

call cpu_time(t_end)
print "(A,f0.3,A)",&
    "Time spent appending to linked list (LL) holding arbitrary types: ", &
    t_end-t_start,"s"

! Creating arrays of intrinsic types
! Appending is done on purpose, to emulate a situation where the final
! length of the arrays will be. Of course, if the final size is initially
! known, then lined lists is not the way to go.
! Integer
call cpu_time(t_start)
do i=1, n_nodes
    call append_to_array(int_array, i)
enddo
call cpu_time(t_end)
print "(A,f0.3,A)",&
    "Time spent appending to integer array with same size as the LL: ", &
    t_end-t_start,"s"
! Real
call cpu_time(t_start)
do i=1, n_nodes
    call append_to_array(real_array, real(i, kind=dp))
enddo
call cpu_time(t_end)
print "(A,f0.3,A)",&
    "Time spent appending to real array with same size as the LL: ", &
    t_end-t_start,"s"
! Complex
call cpu_time(t_start)
do i=1, n_nodes
    call append_to_array(complex_array, cmplx(i, kind=kind_cplx))
enddo
call cpu_time(t_end)
print "(A,f0.3,A)",&
    "Time spent appending to complex array with same size as the LL: ", &
    t_end-t_start,"s"
! Character
if(n_nodes>50000)then
    print "(A)", "Not creating character array: Too many items."
else
    call cpu_time(t_start)
    do i=1, n_nodes
        call append_to_array(char_array, demo_string)
    enddo
    call cpu_time(t_end)
    print "(A,f0.3,A)",&
        "Time spent appending to character array with same size as the LL: ", &
        t_end-t_start,"s"
endif

! Copying elements into indexed list
indexed_list%doubly_linked_list = list%copy()
! Building index
call cpu_time(t_start)
call indexed_list%build_index()
call cpu_time(t_end)
print *, '' 
print "(A,f0.3,A)", "Storage size of regular list: ", &
    list%storage_size(), " MB"
print "(A,f0.3,A)", "Storage size of indexed list: ", &
    indexed_list%storage_size(), " MB"
print "(A,f0.3,A)", "    * Storage size of index: ", &
    indexed_list%storage_size_of_index(), " MB"
print "(A,f0.3,A)",&
    "    * Time spent building index: ", &
    t_end-t_start,"s"

print *, '' 
print "(A,f0.3,A)", "Storage size of integer array: ", &
    array_storage_size_in_MB(int_array)," MB"
print "(A,f0.3,A)", "Storage size of real array: ", &
    array_storage_size_in_MB(real_array)," MB"
print "(A,f0.3,A)", "Storage size of complex array: ", &
    array_storage_size_in_MB(complex_array)," MB"
print "(A,f0.3,A)", "Storage size of char array: ", &
    array_storage_size_in_MB(char_array)," MB"


print *, ''
call cpu_time(t_start)
do i=1, list%len
    ptr => int_array(i)
enddo
call cpu_time(t_end)
print "(A,f0.3,A)",&
    "Time to index-acess all items in integer array: ", &
    t_end-t_start,"s"

call cpu_time(t_start)
do i=1, list%len
    ptr => real_array(i)
enddo
call cpu_time(t_end)
print "(A,f0.3,A)" &
    ,"Time to index-acess all items in real array: ", &
    t_end-t_start,"s"

call cpu_time(t_start)
do i=1, list%len
    ptr => complex_array(i)
enddo
call cpu_time(t_end)
print "(A,f0.3,A)", &
    "Time to index-acess all items in complex array: ", &
    t_end-t_start,"s"

if(allocated(char_array))then
    call cpu_time(t_start)
    do i=1, list%len
        char_ptr => char_array(i)
    enddo
    call cpu_time(t_end)
    print "(A,f0.3,A)", &
        "Time to index-acess all items in character array: ", &
        t_end-t_start,"s"
endif

call cpu_time(t_start)
do i=1, list%len
    ptr_node => indexed_list%item(i)
    !write(*,"(A,I0,1X,A,A)") "item=",i,"value=",list%item(i, "???")
enddo
call cpu_time(t_end)
print "(A,f0.3,A)", &
    "Time to index-acess all items in indexed list: ", &
    t_end-t_start,"s"

call cpu_time(t_start)
do i=1, list%len
    ptr_node => list%item(i)
enddo
call cpu_time(t_end)
print "(A,f0.3,A)", &
    "Time to index-acess all items in regular list: ", &
    t_end-t_start,"s"

end program test_lists
