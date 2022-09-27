module rojff_fallible_json_value_m
    use erloff, only: error_list_t, module_t, procedure_t
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: fallible_json_value_t, move_into_fallible_value

    type :: fallible_json_value_t
        type(error_list_t) :: errors
        class(json_value_t), allocatable :: value_
    contains
        procedure :: failed
    end type

    interface fallible_json_value_t
        module function from_value(value_) result(fallible_value)
            implicit none
            class(json_value_t), intent(in) :: value_
            type(fallible_json_value_t) :: fallible_value
        end function

        module function from_errors(errors) result(fallible_value)
            implicit none
            type(error_list_t), intent(in) :: errors
            type(fallible_json_value_t) :: fallible_value
        end function

        module function from_fallible_value( &
                maybe_value, module_, procedure_) result(fallible_value)
            implicit none
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_value_t) :: fallible_value
        end function
    end interface

    interface move_into_fallible_value
        module subroutine move_from_value(fallible_value, value_)
            implicit none
            type(fallible_json_value_t), intent(out) :: fallible_value
            class(json_value_t), allocatable, intent(inout) :: value_
        end subroutine

        module subroutine move_from_fallible_value( &
                fallible_value, maybe_value, module_, procedure_)
            implicit none
            type(fallible_json_value_t), intent(out) :: fallible_value
            type(fallible_json_value_t), intent(inout) :: maybe_value
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
        end subroutine
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_value_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module
