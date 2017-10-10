!! Copyright (C) 2017 Paulo V. C. Medeiros
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

!==============================================================================
! MODULE: unlimited_polymorphic_lists
!
!> @author
!> Paulo V. C. Medeiros, University of Cambridge <pvm20@cam.ac.uk>
!
! DESCRIPTION: 
!> Doubly-linked lists for unlimited polymorphic objects in Fortran 2008
!> 
!==============================================================================
module unlimited_polymorphic_lists
    use aux_constants_and_types, only : dp, kind_cplx, vector
    use storage_size_wrappers, only : bits_to_MB, array_storage_size_in_MB
    implicit none
    private
    public :: doubly_linked_list, indexed_doubly_linked_list, &
              doubly_linked_list_node, append_to_array
    type doubly_linked_list_node
        ! This defines a doubly-linked list capable of storing vars of any
        ! type. Scalars can be stored out-of-the-box. Non-scalars need to be
        ! encapsulated a scalar var of a suitable user-defined derived type.
        class(*), allocatable :: val
        type(doubly_linked_list_node), pointer :: prev=>null(), next=>null()
    contains
        ! To be able to output values stored in a doubly_linked_list_node
        procedure, private :: write_fmtd => write_LL_nodeval_fmt
        generic :: write(formatted) => write_fmtd
    end type doubly_linked_list_node

    type doubly_linked_list
        type(doubly_linked_list_node), pointer :: head=>null(), tail=>null()
        ! Storing the length as a component variable so that we don't need to
        ! traverse the list every time the lengths is required. Things will
        ! break, however, if the length is not updated once items are added or
        ! removed.
        integer :: len=0
    contains
        ! Methods that return info about items without modifying the list
        !     "item" returns the list node, with value and references to 
        !     "next" and "prev". The head can be easily accessed as either
        !     list%head or list%item(1), whereas the tail can be accessed as
        !     either list%tail or list%item(-1).
        !     The "item" method is also *suitable for output* (eg. write(*,*)).
        procedure :: item => get_ith_node 
        !     "itemval" is convenient if you want direct access to the values
        !     stored in the nodes. But you can use list%item(i)%val too.
        !     You cannot directly use "itemval" for output. Use "item" instead.
        procedure :: itemval => get_node_val 
        procedure :: index => index_of_item
        procedure :: count => count_occurences_of_item
        ! Methods that remove items
        procedure :: insert => insert_item
        procedure :: remove => remove_item_by_value
        procedure :: del => remove_item_by_index
        procedure :: pop => pop_as_node_obj
        ! Methods that add items
        procedure :: append => append_to_list
        procedure :: prepend => prepend_to_list
        procedure :: extend => extend_list 
        ! Method to replace values stored by nodes. The nodes are not removed
        procedure :: replace => replace_node_val_given_index
        ! Method to return a copy of the list 
        ! Changing the copy won't change the original list
        procedure :: copy => list_copy
        ! Method to reverse list in-place
        procedure :: reverse => reverse_list
        ! Memory-related methods
        procedure :: storage_size => list_storage_size
        procedure :: deallocate => deallocate_list
    end type doubly_linked_list

    ! We'll now create an indexed doubly-linked list by inheriting from
    ! the regular doubly-linked list defined above and adding an indexing
    ! array.
    type container_of_ptrs_to_nodes
        type(doubly_linked_list_node), pointer :: pt=>null()
    end type container_of_ptrs_to_nodes

    type :: list_index
        ! The ith element of the "node" array component will store a pointer
        ! to the ith node in the linked list
        type(container_of_ptrs_to_nodes), dimension(:), allocatable :: node
        ! If the index is marked "dirty", then this means that the it needs
        ! to be updated before being used
        logical :: dirty=.TRUE.
    end type list_index

    type, extends(doubly_linked_list) :: indexed_doubly_linked_list
        ! Inherits everything from doubly_linked_list and adds an index and 
        ! related methods
        type(list_index), private :: the_index
    contains
        procedure :: build_index => build_linked_list_index
        procedure :: storage_size_of_index => list_index_storage_size
    end type indexed_doubly_linked_list

    interface append_to_array
        ! The "append" routines will be used for tests and comparison
        module procedure append_to_int_array, append_to_real_array, &
                         append_to_char_array, append_to_complex_array, &
                         append_to_logical_array
    end interface append_to_array

contains

    ! The "append_to_array_*" routines below are defined using the generic code
    ! from the "generic_array_append.inc" file, along with proprocessing.
    ! This reduces code duplication (all routines have basically the same body)
    ! and makes maintenance and extension simpler. This is not really defined
    ! in the Fortran standard, but is supported by most widely used compilers.
    ! Integer
#   define SPECIFIC_ARRAY_APPEND_SUBROUTINE append_to_int_array
#   define ARRAY_TYPE integer
#   include "generic_array_append.inc"
#   undef SPECIFIC_ARRAY_APPEND_SUBROUTINE
#   undef ARRAY_TYPE
    ! Real
#   define SPECIFIC_ARRAY_APPEND_SUBROUTINE append_to_real_array
#   define ARRAY_TYPE real(kind=dp)
#   include "generic_array_append.inc"
#   undef SPECIFIC_ARRAY_APPEND_SUBROUTINE
#   undef ARRAY_TYPE
    ! Complex
#   define SPECIFIC_ARRAY_APPEND_SUBROUTINE append_to_complex_array
#   define ARRAY_TYPE complex(kind=kind_cplx)
#   include "generic_array_append.inc"
#   undef SPECIFIC_ARRAY_APPEND_SUBROUTINE
#   undef ARRAY_TYPE
    ! Character
#   define SPECIFIC_ARRAY_APPEND_SUBROUTINE append_to_char_array
#   define ARRAY_IS_CHAR
#   include "generic_array_append.inc"
#   undef SPECIFIC_ARRAY_APPEND_SUBROUTINE
#   undef ARRAY_IS_CHAR
    ! Logical
#   define SPECIFIC_ARRAY_APPEND_SUBROUTINE append_to_logical_array
#   define ARRAY_TYPE logical
#   include "generic_array_append.inc"
#   undef SPECIFIC_ARRAY_APPEND_SUBROUTINE
#   undef ARRAY_TYPE
    ! End of subroutines to append to regular arrays


    subroutine write_LL_nodeval_fmt(dtv, unit, iotype, v_list, iostat, iomsg)
    ! User-defined derived-type out subroutine for the doubly_linked_list_node
    ! type. See the type definition for the binding. This allows us to write
    !                   write(some_unit, some_format) node
    ! where node is of the doubly_linked_list_node type (or an extension).
    ! In such cases, node%val will be written to unit "some_unit" with the
    ! format specified by "some_format" (both allowed to be the default, *).
    ! Since node%val is unlimited polymorphic, it will be processed using a
    ! "select type" construct.
    implicit none
    class(doubly_linked_list_node), intent(in) :: dtv
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, dimension(:), intent(in) :: v_list
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    ! Local variables
    character(len=:), allocatable :: fmt_string

        if(.not. allocated(dtv%val))then
            iostat = -1
            iomsg = new_line("A")//&
                    'ERROR: Node value cannot be written: Not allocated.'
        else if(size(v_list)>0)then
            iostat = -2
            iomsg = new_line("A")//&
                    'ERROR: Use of array to control format is not supported.'
        else 
            ! Using the preprocessor here helps to avoid code duplication
            ! and makes it easier to extend this routine to support output
            ! of node values with types not currently covered. To do so, 
            ! one "type select" block per new type needs to be added in
            ! the generic_fmtd_unlim_poly_LL_nodeval.inc file. 
            if(iotype=='LISTDIRECTED')then
                ! The "*" format specifier. This is  what you typically want
                ! to use, unless you know exactly the dynamic type of the
                ! value stored at the particular list node.
#               define SPECIFIC_FMT_SPEC *
#               define ASTERISK_FMT_SPEC
#               include "generic_fmtd_unlim_poly_LL_nodeval.inc"
#               undef SPECIFIC_FMT_SPEC
#               undef ASTERISK_FMT_SPEC
            else if(iotype(1:2)=='DT')then 
                ! 'DT' is guaranteed to be uppercase, even if passed otherwise.
                ! Calling write(*, "(dt'f0.8')"), for instance, will instruct 
                ! fortran to format the value to be written as an "f0.8" float.
                ! The write will fail if the descriptor is not compatible with
                ! the dynamic type of the value being written.
                allocate(character(len=len(iotype)) :: fmt_string)
                fmt_string = "("//iotype(3:)//")"
#               define SPECIFIC_FMT_SPEC fmt_string
#               include "generic_fmtd_unlim_poly_LL_nodeval.inc"
#               undef SPECIFIC_FMT_SPEC
            else
                iostat = -4
                iomsg = new_line("A")//&
                        'ERROR: Descriptor not accepted for this type: '//&
                        trim(adjustl(iotype))
            endif
        endif

    end subroutine write_LL_nodeval_fmt

    subroutine build_linked_list_index(this)
    ! Builds the index of the indexed lined list.
    ! The index is a "fortran-style" array of pointers where the ith element
    ! points to the ith node in the linked list.
    !
    ! If I were to call this routine every time the list s updated, then
    ! I'd keep track of an eventual dirty node and reset the index from
    ! there. As, however, I'm only calling this upon retrieval from the
    ! list, I'm resetting the whole index (there may be many dirty nodes
    ! by the point this routine is called, references to which would have
    ! to be updated after every modification).
    implicit none
    class(indexed_doubly_linked_list), intent(inout) :: this
    ! Local variables
    type(doubly_linked_list_node), pointer :: current_node
    integer :: i
 
        if(allocated(this%the_index%node))then
            deallocate(this%the_index%node)
        endif
        allocate(this%the_index%node(1:this%len))
        i=0
        current_node => this%head
        do while(associated(current_node))
            i = i + 1
            this%the_index%node(i)%pt => current_node
            current_node => current_node%next
        enddo
        this%the_index%dirty = .FALSE.

    end subroutine build_linked_list_index

    function list_copy(this) result(copy_this)
    ! Returns a copy of the list. Modifying the copy won't change the list.
    implicit none
    class(doubly_linked_list), allocatable :: copy_this
    class(doubly_linked_list), intent(in) :: this
    ! Local variables
    type(doubly_linked_list_node), pointer :: current_node

        ! Since the indexed_doubly_linked_list type extends doubly_linked_list,
        ! indexed_doubly_linked_list is type-compatible withdoubly_linked_list.
        ! By "copy_this" using "this" as mold, I make sure copy_this has the
        ! same shape and components as "this", no matter whether "this" is
        ! doubly_linked_list or indexed_doubly_linked_list.
        allocate(copy_this, mold=this)
        current_node => this%head
        do while(associated(current_node))
            call copy_this%append(current_node%val)
            current_node => current_node%next
        enddo

        select type(this)
            type is(indexed_doubly_linked_list)
                select type(copy_this)
                    ! We have guaranteed that "this" and "copy_this" have
                    ! the same type, but, in order to modify copy_this, we
                    ! still need the "select type" construct. Not very
                    ! practical, but, oh well...
                    type is(indexed_doubly_linked_list)
                        copy_this%the_index = this%the_index
                end select
        end select

    end function list_copy

    function list_storage_size(this) result(memsize)
    ! Storage size of the list *in MB*
    ! Adopts the 2^n conversion factor convention
    implicit none
    real(kind=dp) :: memsize
    class(doubly_linked_list), intent(in) :: this
    ! Local variables
    type(doubly_linked_list_node), pointer :: current_node
    integer(kind=selected_int_kind(15)) :: memsize_in_bits
    type(doubly_linked_list_node), pointer :: pt_to_node=>null()
    integer, parameter :: size_of_pt_to_node=storage_size(pt_to_node)

        memsize_in_bits = 0
        current_node => this%head
        do while(associated(current_node))
            memsize_in_bits = memsize_in_bits + &
                              storage_size(&
                                  current_node%val, &
                                  kind=kind(memsize_in_bits) &
                              )
            ! Including the "prev" and "next" pointers
            memsize_in_bits = memsize_in_bits + 2*size_of_pt_to_node
            current_node => current_node%next
        enddo
        ! Including the "head" and "tail" pointers
        memsize_in_bits = memsize_in_bits + 2*size_of_pt_to_node

        memsize = bits_to_MB(memsize_in_bits)
        select type(this)
            type is(indexed_doubly_linked_list)
                ! Including size of index -- already in MB
                memsize = memsize + this%storage_size_of_index()
        end select
        
    end function list_storage_size
    function list_index_storage_size(this) result(memsize)
    ! Storage size of the list index *in MB*
    ! Adopts the 2^n conversion factor convention
    implicit none
    real(kind=dp) :: memsize
    class(indexed_doubly_linked_list), intent(in) :: this

        memsize = array_storage_size_in_MB(this%the_index%node(:)) + &
                  bits_to_MB(storage_size(this%the_index%dirty))

    end function list_index_storage_size


    subroutine reverse_list(this)
    ! Reverses the list "in-place".
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    ! Local variables
    type(doubly_linked_list_node), pointer :: current_node, aux_pointer

        current_node => this%head
        do while(associated(current_node))
            aux_pointer => current_node%next
            current_node%next => current_node%prev
            current_node%prev => aux_pointer
            current_node => current_node%prev
        enddo
        aux_pointer => this%head
        this%head => this%tail
        this%tail => aux_pointer

        select type(this)
            type is(indexed_doubly_linked_list)
                this%the_index%dirty = .TRUE.
        end select
        
    end subroutine reverse_list

! Methods that modify the size of the list

    subroutine modify_list_length(this, add)
    ! Changes the length of the list.
    ! If the list is indexed, then the index is marked as "dirty"
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    integer, intent(in) :: add

        this%len = this%len + add
        select type(this)
            type is(indexed_doubly_linked_list)
                this%the_index%dirty = .TRUE.
        end select

    end subroutine modify_list_length

    subroutine deallocate_list(this)
    ! Releases the memory allocated for the data stored in the list nodes.
    ! Pointers are disassociated as well.
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    ! Local variables
    type(doubly_linked_list_node), pointer :: current_node

        if(associated(this%head))then
            current_node => this%head
            do
                deallocate(current_node%val)
                current_node%prev => null()
                current_node => current_node%next
                if(.not. associated(current_node)) exit
                current_node%prev%next => null()
                current_node%prev => null()
            enddo
            this%head => null()
            this%tail => null()

            this%len = 0
            select type(this)
                type is(indexed_doubly_linked_list)
                    deallocate(this%the_index%node)
                    this%the_index%dirty = .TRUE.
            end select

        endif
        
    end subroutine deallocate_list

    subroutine extend_list(this, other)
    ! Extends the list by appending copies of all elements from "other".
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    class(doubly_linked_list), intent(in) :: other
    ! Local variables
    class(doubly_linked_list), allocatable :: copy_other

        ! The ifort version I have does not support intrinsic assignment of
        ! allocatable polymorphic variables (automatic rellocation, F2008 std).
        ! I'd have used "copy_other = other%copy()" if it did.
        allocate(copy_other, source=other%copy()) ! TEST

        this%tail%next => copy_other%head
        copy_other%head%prev => this%tail
        this%tail => copy_other%tail

        call modify_list_length(this, add=copy_other%len)
        
    end subroutine extend_list

    subroutine insert_item(this, inode, item)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    integer, intent(in) :: inode
    class(*), intent(in) :: item
    ! Local variables
    type(doubly_linked_list_node), pointer :: curr_node_at_pos, new_node

        allocate(new_node)
        ! If using mold=item (F2008), then this needs to be followed
        ! by assignment. If using source=item (F2003), then the value of
        ! item is copied to the array being allocated.
        ! Using mold causes NAG Fortran compiler to panic, even though the page
        ! https://www.nag.co.uk/nagware/np/r61_doc/nag_f2008.html
        ! says this is supported.    
        allocate(new_node%val, source=item)

        if(.not. associated(this%head))then
            ! If the list is empty
            this%head => new_node
            this%tail => new_node
        else
            if(inode>this%len)then
                ! Appending
                this%tail%next => new_node
                new_node%prev => this%tail
                ! Resetting tail
                this%tail => new_node
            else
                curr_node_at_pos => get_ith_node(this, inode)
                if(associated(this%head, curr_node_at_pos))then
                    ! Reset head if prepending to non-empty list
                    this%head => new_node
                else
                    curr_node_at_pos%prev%next => new_node
                    new_node%prev => curr_node_at_pos%prev
                endif
                curr_node_at_pos%prev => new_node
                new_node%next => curr_node_at_pos
            endif
        endif
     
        new_node => null()
        curr_node_at_pos => null() 

        call modify_list_length(this, add=1)

    end subroutine insert_item


    subroutine remove_node(this, node_2b_rm)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    type(doubly_linked_list_node), pointer, intent(inout) :: node_2b_rm

        if(associated(this%head, node_2b_rm))then
            this%head => node_2b_rm%next
        else
            node_2b_rm%prev%next => node_2b_rm%next
        endif
        if(associated(this%tail, node_2b_rm))then
            this%tail => node_2b_rm%prev
        else
            node_2b_rm%next%prev => node_2b_rm%prev
        endif

        ! Using deallocate to make sure to free memory
        deallocate(node_2b_rm)
        node_2b_rm => null()

        call modify_list_length(this, add=-1)

    end subroutine remove_node

    subroutine remove_item_by_index(this, inode)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    integer, intent(in) :: inode
    ! Local variables
    type(doubly_linked_list_node), pointer :: node_2b_rm

        node_2b_rm => get_ith_node(this, inode)
        call remove_node(this, node_2b_rm)

    end subroutine remove_item_by_index
    subroutine remove_item_by_value(this, item)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    class(*), intent(in), target :: item

       call remove_item_by_index(this, this%index(item))

    end subroutine remove_item_by_value 

    function pop_as_node_obj(this, inode) result(rtn)
    implicit none
    class(*), allocatable :: rtn
    class(doubly_linked_list), intent(inout) :: this
    integer, intent(in) :: inode
    ! Local variables
    type(doubly_linked_list_node), pointer :: node_2b_rm

        node_2b_rm => get_ith_node(this, inode)
        allocate(rtn, source=node_2b_rm)
        call remove_node(this, node_2b_rm)

    end function pop_as_node_obj

    subroutine append_to_list(this, item)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    class(*), intent(in), target :: item

        call this%insert(this%len+1, item)

    end subroutine append_to_list

    subroutine prepend_to_list(this, item)
    implicit none
    class(doubly_linked_list), intent(inout) :: this
    class(*), intent(in), target :: item

        call this%insert(1, item)

    end subroutine prepend_to_list
! End of methods that modify the size of the list


    function eqv_reduced_inode(this, inode) result(new_inode)
    implicit none
    integer :: new_inode
    class(doubly_linked_list), intent(in) :: this
    integer, intent(in) :: inode

        new_inode = inode
        if(new_inode < 0)then
            new_inode = new_inode + this%len + 1
        elseif(new_inode > this%len)then
            new_inode = this%len + 1
        endif

    end function eqv_reduced_inode


    function get_ith_node_fwd_traverse(this, inode) result(node)
    ! Returns the ith node of the linked list by trversing it
    ! forwards, i.e., from head towards tail. This is obviously
    ! not the intended use for a linked list, and has been included
    ! here just for completeness. If you need indexed/random access,
    ! then use the indexed linked list implemented here or, if you can,
    ! simply use arrays.
    implicit none
    class(doubly_linked_list), intent(in) :: this
    type(doubly_linked_list_node), pointer :: node
    integer, intent(in) :: inode
    ! Local variables
    integer :: i

        node => null()
        if((inode>0) .and. (inode <= this%len))then
            node => this%head
            do i=1, inode-1
                node => node%next
            enddo
        endif

    end function get_ith_node_fwd_traverse
    function get_ith_node_bwd_traverse(this, inode) result(node)
    ! Returns the ith node of the linked list by trversing it
    ! backwards, i.e., from tail towards head. All comments made
    ! in the get_ith_node_fwd_traverse function apply here too.
    implicit none
    class(doubly_linked_list), intent(in) :: this
    type(doubly_linked_list_node), pointer :: node
    integer, intent(in) :: inode
    ! Local variables
    integer :: i

        node => null()
        if((inode>0) .and. (inode <= this%len))then
            node => this%tail
            do i=this%len, inode+1, -1
                node => node%prev
            enddo
        endif

    end function get_ith_node_bwd_traverse
    function get_ith_node_no_index(this, inode) result(node)
    ! Returns the ith node of the linked list by trversing it either
    ! forwards or backwards, depending on the passed node index and 
    ! the size of the list. 
    ! All comments made in the get_ith_node_fwd_traverse function 
    ! apply here too.
    implicit none
    type(doubly_linked_list_node), pointer :: node
    class(doubly_linked_list), intent(in) :: this
    integer, intent(in) :: inode

        if(2*inode > this%len)then
            node => get_ith_node_bwd_traverse(this, inode)
        else
            node => get_ith_node_fwd_traverse(this, inode)
        endif

    end function get_ith_node_no_index
    function get_ith_node(this, inode, reset_index_if_needed) result(node)
    ! Wraps all "get_ith_node_*" functions and decides which one to use.
    ! If the list is indexed, then the index will be used. If the index is
    ! marked "dirty", it will be rebuild before being used. If the list is
    ! not indexed (not the intended use, but, anyway), then the function
    ! "get_ith_node_no_index" will be called.
    implicit none
    type(doubly_linked_list_node), pointer :: node
    ! "this" is "inout" because we may need to reset the index
    class(doubly_linked_list), intent(inout) :: this
    integer, intent(in) :: inode
    logical, intent(in), optional :: reset_index_if_needed
    ! Local variables
    integer :: rdcd_inode
    logical :: reset_index

        node => null()
        rdcd_inode = eqv_reduced_inode(this, inode)
        select type(this)
            type is(indexed_doubly_linked_list)
                reset_index = .FALSE.
                if(present(reset_index_if_needed))then
                    reset_index = reset_index_if_needed
                endif
                if(this%the_index%dirty .and. reset_index)then
                    call this%build_index()
                endif
                if(this%the_index%dirty)then
                    node => get_ith_node_no_index(this, rdcd_inode)
                else
                    node => this%the_index%node(rdcd_inode)%pt
                endif
            class default
                node => get_ith_node_no_index(this, rdcd_inode)
        end select

    end function get_ith_node

    function get_node_val(this, inode) result(rtn)
    ! Returns the value stored in the list node with index "inode".
    ! The returned value most likely will need some processing in order
    ! to be used (eg., via a "select type" construct). If you want to
    ! write the stored value to some output unit, then you can write the
    ! node directly, as the derived type that defines the node also defines
    ! an associated i/o routine.
    implicit none
    class(*), pointer :: rtn
    ! "this" is "inout" because get_ith_node may need to reset the index
    class(doubly_linked_list), target, intent(inout) :: this
    integer, intent(in) :: inode
    ! Local variables
    type(doubly_linked_list_node), pointer :: node

        node => get_ith_node(this, inode, reset_index_if_needed=.TRUE.)
        rtn => node%val

    end function get_node_val

    subroutine replace_node_val_given_index(this, inode, new_val)
    ! If the list is indexed, the index will remain clean if originally so
    ! without any need to rebuild it: The index points to nodes, and we are
    ! not replacing a node -- only the vaue it stores.
    ! If the index is dirty, then get_ith_node will get it rebuilt.
    implicit none
    class(*), intent(in) :: new_val
    ! "this" is "inout" because get_ith_node may need to reset the index
    class(doubly_linked_list), target, intent(inout) :: this
    integer, intent(in) :: inode
    ! Local variables
    type(doubly_linked_list_node), pointer :: node

        node => get_ith_node(this, inode, reset_index_if_needed=.TRUE.)
        ! The ifort version I have does not support intrinsic assignment of
        ! allocatable polymorphic variables (automatic rellocation, F2008 std).
        ! I'd have used "copy_other = other%copy()" if it did.
        deallocate(node%val)
        allocate(node%val, source=new_val)

    end subroutine replace_node_val_given_index

    function index_of_item(this, item) result(rtn)
    implicit none
    integer :: rtn
    class(doubly_linked_list), intent(in) :: this
    class(*), intent(in) :: item
    ! Local variables
    integer :: ind, item_size
    type(doubly_linked_list_node), pointer :: current
    character(len=:), allocatable :: aux_char
    ! Local parameters
    integer, parameter :: char_size = storage_size("A")

        rtn = 0
        item_size = storage_size(item)
        current => this%head
        ind = 0
        do while(associated(current) .and. rtn==0)
            ind = ind + 1
            if(same_type_as(current%val, item))then
                allocate(character(item_size/char_size) :: aux_char)
                if(transfer(current%val, aux_char) == &
                   transfer(item, aux_char))then
                    rtn = ind
                endif
                deallocate(aux_char)
            endif
            current => current%next
        enddo

    end function index_of_item

    function count_occurences_of_item(this, item) result(occ_count)
    implicit none
    integer :: occ_count
    class(doubly_linked_list), intent(in) :: this
    class(*), intent(in) :: item
    ! Local variables
    integer :: item_size
    type(doubly_linked_list_node), pointer :: current
    character(len=:), allocatable :: aux_char
    ! Local parameters
    integer, parameter :: char_size = storage_size("A")

        occ_count = 0
        item_size = storage_size(item)
        current => this%head
        do while(associated(current))
            if(same_type_as(current%val, item))then
                allocate(character(item_size/char_size) :: aux_char)
                if(transfer(current%val, aux_char) == &
                   transfer(item, aux_char))then
                    occ_count = occ_count + 1
                endif
                deallocate(aux_char)
            endif
            current => current%next
        enddo

    end function count_occurences_of_item

end module unlimited_polymorphic_lists
