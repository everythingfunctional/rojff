module rojff_fallible_json_element_m
    use erloff, only: error_list_t, module_t, procedure_t
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_element_m, only: json_element_t
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: fallible_json_element_t

    type :: fallible_json_element_t
        type(json_element_t) :: element
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_element_t
        module function from_element(element) result(fallible_element)
            implicit none
            type(json_element_t), intent(in) :: element
            type(fallible_json_element_t) :: fallible_element
        end function

        module function from_errors(errors) result(fallible_element)
            implicit none
            type(error_list_t), intent(in) :: errors
            type(fallible_json_element_t) :: fallible_element
        end function

        module function from_fallible_element( &
                maybe_element, module_, procedure_) result(fallible_element)
            implicit none
            type(fallible_json_element_t), intent(in) :: maybe_element
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_element_t) :: fallible_element
        end function

        module function from_json_value(value_) result(fallible_element)
            implicit none
            class(json_value_t), intent(in) :: value_
            type(fallible_json_element_t) :: fallible_element
        end function

        module function from_fallible_value(maybe_value) result(fallible_element)
            implicit none
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(fallible_json_element_t) :: fallible_element
        end function
    end interface

    interface move_into_fallible_element
        module subroutine move_from_value(fallible_element, value_)
            implicit none
            type(fallible_json_element_t), intent(out) :: fallible_element
            class(json_value_t), allocatable, intent(inout) :: value_
        end subroutine

        module subroutine move_from_fallible_value(fallible_element, maybe_value)
            implicit none
            type(fallible_json_element_t), intent(out) :: fallible_element
            type(fallible_json_value_t), intent(inout) :: maybe_value
        end subroutine
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_element_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module