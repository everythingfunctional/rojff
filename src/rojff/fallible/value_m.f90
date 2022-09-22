module rojff_fallible_json_value_m
    use erloff, only: error_list_t, module_t, procedure_t
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: fallible_json_value_t, move_into_fallible_json

    type :: fallible_json_value_t
        type(error_list_t) :: errors
        class(json_value_t), allocatable :: json
    contains
        procedure :: failed
    end type

    interface fallible_json_value_t
        module function from_value(json) result(fallible_json)
            implicit none
            class(json_value_t), intent(in) :: json
            type(fallible_json_value_t) :: fallible_json
        end function

        module function from_errors(errors) result(fallible_json)
            implicit none
            type(error_list_t), intent(in) :: errors
            type(fallible_json_value_t) :: fallible_json
        end function

        module function from_fallible_json( &
                original, module_, procedure_) result(new)
            implicit none
            type(fallible_json_value_t), intent(in) :: original
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_value_t) :: new
        end function
    end interface

    interface
        module subroutine move_into_fallible_json(fallible_json, json)
            implicit none
            type(fallible_json_value_t), intent(out) :: fallible_json
            class(json_value_t), allocatable, intent(inout) :: json
        end subroutine

        elemental module function failed(self)
            implicit none
            class(fallible_json_value_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module
