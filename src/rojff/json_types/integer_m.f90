module rojff_json_integer_m
    use rojff_json_value_m, only: json_value_t
    use rojff_sink_m, only: sink_t

    implicit none
    private
    public :: json_integer_t, create_json_integer

    type, extends(json_value_t) :: json_integer_t
        integer :: number
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_integer_t
        elemental module function constructor(number) result(json_integer)
            implicit none
            integer, intent(in) :: number
            type(json_integer_t) :: json_integer
        end function
    end interface

    interface
        module subroutine create_json_integer(json, number)
            implicit none
            type(json_integer_t), allocatable, intent(out) :: json
            integer, intent(in) :: number
        end subroutine

        elemental module function equals(lhs, rhs)
            implicit none
            class(json_integer_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_integer_t), intent(in) :: self
            class(sink_t), intent(inout) :: sink
        end subroutine

        module subroutine write_to_expanded(self, indentation_level, sink)
            implicit none
            class(json_integer_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
