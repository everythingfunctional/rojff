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
        module procedure constructor
    end interface
contains
    impure elemental function constructor(json) result(element)
        class(json_value_t), intent(in) :: json
        type(json_element_t) :: element

        element%json = json
    end function

    subroutine move_into_element(element, json)
        type(json_element_t), intent(out) :: element
        class(json_value_t), allocatable, intent(inout) :: json

        call move_alloc(json, element%json)
    end subroutine

    recursive elemental function equals(lhs, rhs)
        class(json_element_t), intent(in) :: lhs
        type(json_element_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%json == rhs%json
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_element_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        call self%json%write_to_compactly(sink)
    end subroutine

    recursive subroutine write_to_expanded(self, indentation_level, sink)
        class(json_element_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        call self%json%write_to_expanded(indentation_level, sink)
    end subroutine
end module
