module rojff_member_linked_list_m
    use rojff_json_member_m, only: json_member_t
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

    interface
        module subroutine append(self, key, value_)
            implicit none
            class(member_linked_list_t), intent(inout) :: self
            character(len=*), intent(in) :: key
            class(json_value_t), allocatable, intent(inout) :: value_
        end subroutine

        module subroutine move_into_members(self, members)
            implicit none
            class(member_linked_list_t), intent(inout) :: self
            type(json_member_t), allocatable, intent(out) :: members(:)
        end subroutine
    end interface
end module
