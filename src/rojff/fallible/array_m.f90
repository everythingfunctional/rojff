module rojff_fallible_json_array_m
    use erloff, only: error_list_t, module_t, procedure_t
    use rojff_fallible_json_element_m, only: fallible_json_element_t
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_array_m, only: json_array_t
    use rojff_json_element_m, only: json_element_t

    implicit none
    private
    public :: fallible_json_array_t, fallible_json_value_t

    type :: fallible_json_array_t
        type(json_array_t) :: array
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_array_t
        module function from_array(array) result(fallible_array)
            implicit none
            type(json_array_t), intent(in) :: array
            type(fallible_json_array_t) :: fallible_array
        end function

        module function from_errors(errors) result(fallible_array)
            implicit none
            type(error_list_t), intent(in) :: errors
            type(fallible_json_array_t) :: fallible_array
        end function

        module function from_fallible_array( &
                maybe_array, module_, procedure_) result(fallible_array)
            implicit none
            type(fallible_json_array_t), intent(in) :: maybe_array
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_array_t) :: fallible_array
        end function

        module function from_elements(elements) result(fallible_array)
            implicit none
            type(json_element_t), intent(in) :: elements(:)
            type(fallible_json_array_t) :: fallible_array
        end function

        module function from_fallible_elements(maybe_elements) result(fallible_array)
            implicit none
            type(fallible_json_element_t), intent(in) :: maybe_elements(:)
            type(fallible_json_array_t) :: fallible_array
        end function
    end interface

    interface fallible_json_value_t
        module function fallible_json_value_from_fallible_array( &
                maybe_array) result(fallible_value)
            implicit none
            type(fallible_json_array_t), intent(in) :: maybe_array
            type(fallible_json_value_t) :: fallible_value
        end function
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_array_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module