module rojff_json_value_m
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_value_t

    type, abstract :: json_value_t
    contains
        procedure(equals_i), deferred :: equals
        generic :: operator(==) => equals
        procedure(write_to_compactly_i), deferred :: write_to_compactly
        procedure(write_to_expanded_i), deferred :: write_to_expanded
        procedure :: to_compact_string
        procedure :: save_compactly_to
        procedure :: to_expanded_string
        procedure :: save_expanded_to
    end type

    abstract interface
        elemental function equals_i(lhs, rhs) result(equals)
            import :: json_value_t

            implicit none

            class(json_value_t), intent(in) :: lhs, rhs
            logical :: equals
        end function

        subroutine write_to_compactly_i(self, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        subroutine write_to_expanded_i(self, indentation_level, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface

    interface
        module function to_compact_string(self) result(string)
            implicit none
            class(json_value_t), intent(in) :: self
            character(len=:), allocatable :: string
        end function

        module subroutine save_compactly_to(self, file, status, iostat, iomsg)
            implicit none
            class(json_value_t), intent(in) :: self
            character(len=*), intent(in) :: file
            character(len=*), optional, intent(in) :: status
            integer, optional, intent(out) :: iostat
            character(len=:), allocatable, optional, intent(out) :: iomsg
        end subroutine

        module function to_expanded_string(self) result(string)
            implicit none
            class(json_value_t), intent(in) :: self
            character(len=:), allocatable :: string
        end function

        module subroutine save_expanded_to(self, file, status, iostat, iomsg)
            implicit none
            class(json_value_t), intent(in) :: self
            character(len=*), intent(in) :: file
            character(len=*), optional, intent(in) :: status
            integer, optional, intent(out) :: iostat
            character(len=:), allocatable, optional, intent(out) :: iomsg
        end subroutine
    end interface
end module
