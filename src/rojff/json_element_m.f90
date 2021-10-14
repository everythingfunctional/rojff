module rojff_json_element_m
    use iso_varying_string, only: varying_string
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_element_t, move_into_element

    type :: json_element_t
        class(json_value_t), allocatable :: json
    contains
        procedure :: equals
        generic :: operator(==) => equals
        procedure :: to_compact_string
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

    elemental function equals(lhs, rhs)
        class(json_element_t), intent(in) :: lhs
        type(json_element_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%json == rhs%json
    end function

    elemental recursive function to_compact_string(self) result(string)
        class(json_element_t), intent(in) :: self
        type(varying_string) :: string

        string = self%json%to_compact_string()
    end function
end module
