module rojff_json_array_m
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_element_m, only: json_element_t
    use rojff_json_value_m, only: json_value_t
    use rojff_sink_m, only: sink_t

    implicit none
    private
    public :: json_array_t, move_into_array

    type, extends(json_value_t) :: json_array_t
        type(json_element_t), allocatable :: elements(:)
    contains
        procedure :: equals
        procedure :: get
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_array_t
        module function constructor(elements) result(json_array)
            implicit none
            type(json_element_t), intent(in) :: elements(:)
            type(json_array_t) :: json_array
        end function
    end interface

    interface
        module subroutine move_into_array(json, elements)
            implicit none
            type(json_array_t), allocatable, intent(out) :: json
            type(json_element_t), allocatable, intent(inout) :: elements(:)
        end subroutine

        recursive elemental module function equals(lhs, rhs)
            implicit none
            class(json_array_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        impure elemental module function get(self, position) result(element)
            implicit none
            class(json_array_t), intent(in) :: self
            integer, intent(in) :: position
            type(fallible_json_value_t) :: element
        end function

        recursive module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_array_t), intent(in) :: self
            class(sink_t), intent(inout) :: sink
        end subroutine

        recursive module subroutine write_to_expanded( &
                self, indentation_level, sink)
            implicit none
            class(json_array_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
