module rojff_json_string_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: json_string_t, create_json_string_unsafe, json_string_unsafe

    type, extends(json_value_t) :: json_string_t
        character(len=:), allocatable :: string
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_string_unsafe
        pure module function json_string_unsafe_c(string) result(json_string)
            implicit none
            character(len=*), intent(in) :: string
            type(json_string_t) :: json_string
        end function

        elemental module function json_string_unsafe_s(string) result(json_string)
            implicit none
            type(varying_string), intent(in) :: string
            type(json_string_t) :: json_string
        end function
    end interface

    interface
        module subroutine create_json_string_unsafe(json, string)
            implicit none
            class(json_value_t), allocatable, intent(out) :: json
            character(len=*), intent(in) :: string
        end subroutine

        elemental module function equals(lhs, rhs)
            implicit none
            class(json_string_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_string_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        module subroutine write_to_expanded(self, indentation_level, sink)
            implicit none
            class(json_string_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
