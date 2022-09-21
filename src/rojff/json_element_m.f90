module rojff_json_element_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_element_t, move_into_element

    type :: json_element_t
        class(json_value_t), allocatable :: json
    contains
        procedure :: equals
        generic :: operator(==) => equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_element_t
        impure elemental module function constructor(json) result(element)
            implicit none
            class(json_value_t), intent(in) :: json
            type(json_element_t) :: element
        end function
    end interface

    interface
        module subroutine move_into_element(element, json)
            implicit none
            type(json_element_t), intent(out) :: element
            class(json_value_t), allocatable, intent(inout) :: json
        end subroutine

        recursive elemental module function equals(lhs, rhs)
            implicit none
            class(json_element_t), intent(in) :: lhs
            type(json_element_t), intent(in) :: rhs
            logical :: equals
        end function

        recursive module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_element_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        recursive module subroutine write_to_expanded( &
                self, indentation_level, sink)
            implicit none
            class(json_element_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
