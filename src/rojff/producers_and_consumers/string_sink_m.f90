module rojff_string_sink_m
    use rojff_sink_m, only: sink_t

    implicit none
    private
    public :: string_sink_t

    type :: string_node_t
        character(len=:), allocatable :: part
        type(string_node_t), pointer :: next => null()
    end type

    type, extends(sink_t) :: string_sink_t
        private
        type(string_node_t), pointer :: head => null()
        type(string_node_t), pointer :: tail => null()
        integer :: total_length
    contains
        procedure :: append
        procedure :: move_into
    end type

    interface
        module subroutine append(self, part)
            implicit none
            class(string_sink_t), intent(inout) :: self
            character(len=*), intent(in) :: part
        end subroutine

        module subroutine move_into(self, string)
            implicit none
            class(string_sink_t), intent(inout) :: self
            character(len=:), allocatable, intent(out) :: string
        end subroutine
    end interface
end module
