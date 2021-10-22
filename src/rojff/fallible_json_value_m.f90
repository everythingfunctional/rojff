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
        module procedure from_value
        module procedure from_errors
        module procedure from_fallible_json
    end interface
contains
    function from_value(json) result(fallible_json)
        class(json_value_t), intent(in) :: json
        type(fallible_json_value_t) :: fallible_json

        fallible_json%json = json
    end function

    function from_errors(errors) result(fallible_json)
        type(error_list_t), intent(in) :: errors
        type(fallible_json_value_t) :: fallible_json

        fallible_json%errors = errors
    end function

    function from_fallible_json(original, module_, procedure_) result(new)
        type(fallible_json_value_t), intent(in) :: original
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_value_t) :: new

        if (original%failed()) then
            new%errors = error_list_t(original%errors, module_, procedure_)
        else
            new%json = original%json
        end if
    end function

    subroutine move_into_fallible_json(fallible_json, json)
        type(fallible_json_value_t), intent(out) :: fallible_json
        class(json_value_t), allocatable, intent(inout) :: json

        call move_alloc(json, fallible_json%json)
    end subroutine

    elemental function failed(self)
        class(fallible_json_value_t), intent(in) :: self
        logical :: failed

        failed = self%errors%has_any()
    end function
end module
