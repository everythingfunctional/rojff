module rojff_json_number_m
    use rojff_json_value_m, only: json_value_t
    use rojff_sink_m, only: sink_t

    implicit none
    private
    public :: json_number_t, create_json_number

    type, extends(json_value_t) :: json_number_t
        double precision :: number
        integer :: precision
        logical :: precision_provided
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_number_t
        elemental module function constructor( &
                number, precision) result(json_number)
            implicit none
            double precision, intent(in) :: number
            integer, optional, intent(in) :: precision
            type(json_number_t) :: json_number
        end function
    end interface

    interface
        module subroutine create_json_number(json, number, precision)
            implicit none
            type(json_number_t), allocatable, intent(out) :: json
            double precision, intent(in) :: number
            integer, optional, intent(in) :: precision
        end subroutine

        elemental module function equals(lhs, rhs)
            implicit none
            class(json_number_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_number_t), intent(in) :: self
            class(sink_t), intent(inout) :: sink
        end subroutine

        module subroutine write_to_expanded(self, indentation_level, sink)
            implicit none
            class(json_number_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
