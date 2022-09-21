module rojff_json_bool_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_bool_t, create_json_bool

    type, extends(json_value_t) :: json_bool_t
        logical :: bool
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_bool_t
        elemental module function constructor(bool) result(json_bool)
            implicit none
            type(json_bool_t) :: json_bool
            logical, intent(in) :: bool
        end function
    end interface

    interface
        module subroutine create_json_bool(json, bool)
            implicit none
            class(json_value_t), allocatable, intent(out) :: json
            logical, intent(in) :: bool
        end subroutine

        elemental module function equals(lhs, rhs)
            implicit none
            class(json_bool_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_bool_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        module subroutine write_to_expanded(self, indentation_level, sink)
            implicit none
            class(json_bool_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
