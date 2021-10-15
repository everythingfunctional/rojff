module rojff_string_builder_m
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: string_builder_t

    type :: string_node_t
        character(len=:), allocatable :: part
        type(string_node_t), pointer :: next => null()
    end type

    type, extends(string_sink_t) :: string_builder_t
        private
        type(string_node_t), pointer :: head => null()
        type(string_node_t), pointer :: tail => null()
        integer :: total_length
    contains
        procedure :: append
        procedure :: move_into
    end type
contains
    subroutine append(self, part)
        class(string_builder_t), intent(inout) :: self
        character(len=*), intent(in) :: part

        if (associated(self%head)) then
            allocate(self%tail%next)
            self%tail => self%tail%next
            self%tail%part = part
            self%total_length = self%total_length + len(part)
        else
            allocate(self%head)
            self%tail => self%head
            self%head%part = part
            self%total_length = len(part)
        end if
    end subroutine

    subroutine move_into(self, string)
        class(string_builder_t), intent(inout) :: self
        character(len=:), allocatable, intent(out) :: string

        type(string_node_t), pointer :: curr
        integer :: current_position

        allocate(character(len=self%total_length) :: string)
        current_position = 1
        curr => self%head
        do while (associated(curr))
            string(current_position : current_position + len(curr%part) - 1) = curr%part
            current_position = current_position + len(curr%part)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end subroutine
end module
