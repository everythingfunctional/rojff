module rojff_json_array_m
    use iso_varying_string, only: varying_string, operator(//)
    use rojff_json_element_m, only: json_element_t
    use rojff_json_value_m, only: json_value_t
    use strff, only: join

    implicit none
    private
    public :: json_array_t, move_into_array

    type, extends(json_value_t) :: json_array_t
        type(json_element_t), allocatable :: elements(:)
    contains
        procedure :: equals
        procedure :: to_compact_string
    end type
contains
    function constructor(elements) result(json_array)
        type(json_element_t), intent(in) :: elements(:)
        type(json_array_t) :: json_array

        json_array%elements = elements
    end function

    subroutine move_into_array(json, elements)
        class(json_value_t), allocatable, intent(out) :: json
        type(json_element_t), allocatable, intent(inout) :: elements(:)

        type(json_array_t), allocatable :: local

        allocate(local)
        call move_alloc(elements, local%elements)
        call move_alloc(local, json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_array_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_array_t)
            equals = size(lhs%elements) == size(rhs%elements) .and. all(lhs%elements == rhs%elements)
        class default
            equals = .false.
        end select
    end function

    elemental recursive function to_compact_string(self) result(string)
        class(json_array_t), intent(in) :: self
        type(varying_string) :: string

        string = "[" // join(self%elements%to_compact_string(), ",") // "]"
    end function
end module
