module rojff_fallible_json_string_m
    use erloff, only: error_list_t, module_t, procedure_t
    use iso_varying_string, only: varying_string
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_string_m, only: json_string_t

    implicit none
    private
    public :: fallible_json_string_t, fallible_json_value_t

    type :: fallible_json_string_t
        type(json_string_t) :: string
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_string_t
        module function from_character(string) result(fallible_string)
            implicit none
            character(len=*), intent(in) :: string
            type(fallible_json_string_t) :: fallible_string
        end function

        impure elemental module function from_string( &
                string) result(fallible_string)
            implicit none
            type(varying_string), intent(in) :: string
            type(fallible_json_string_t) :: fallible_string
        end function

        impure elemental module function from_json_string( &
                string) result(fallible_string)
            implicit none
            type(json_string_t), intent(in) :: string
            type(fallible_json_string_t) :: fallible_string
        end function

        module function from_errors(errors) result(fallible_string)
            implicit none
            type(error_list_t), intent(in) :: errors
            type(fallible_json_string_t) :: fallible_string
        end function

        impure elemental module function from_fallible_string( &
                maybe_string, module_, procedure_) result(fallible_string)
            implicit none
            type(fallible_json_string_t), intent(in) :: maybe_string
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_string_t) :: fallible_string
        end function
    end interface

    interface fallible_json_value_t
        module function fallible_json_value_from_fallible_array( &
                maybe_string) result(fallible_value)
            implicit none
            type(fallible_json_string_t), intent(in) :: maybe_string
            type(fallible_json_value_t) :: fallible_value
        end function
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_string_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module
