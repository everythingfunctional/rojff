module rojff_member_linked_list_m
    use rojff_json_member_m, only: json_member_t, move_into_member_unsafe
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: member_linked_list_t

    type :: member_node_t
        character(len=:), allocatable :: key
        class(json_value_t), allocatable :: value_
        type(member_node_t), pointer :: next => null()
    end type

    type :: member_linked_list_t
        private
        type(member_node_t), pointer :: head => null()
        type(member_node_t), pointer :: tail => null()
        integer :: size = 0
    contains
        procedure :: append
        procedure :: move_into_members
    end type
contains
    subroutine append(self, key, value_)
        class(member_linked_list_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        class(json_value_t), allocatable, intent(inout) :: value_

        if (self%size == 0) then
            allocate(self%head)
            self%tail => self%head
            self%tail%key = key
            call move_alloc(value_, self%head%value_)
            self%size = 1
        else
            allocate(self%tail%next)
            self%tail => self%tail%next
            self%tail%key = key
            call move_alloc(value_, self%tail%value_)
            self%size = self%size + 1
        end if
    end subroutine

    subroutine move_into_members(self, members)
        class(member_linked_list_t), intent(inout) :: self
        type(json_member_t), allocatable, intent(out) :: members(:)

        type(member_node_t), pointer :: curr
        integer :: i

        allocate(members(self%size))
        curr => self%head
        do i = 1, self%size
            call move_into_member_unsafe(members(i), curr%key, curr%value_)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end subroutine
end module
