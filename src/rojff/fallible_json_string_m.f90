module rojff_fallible_json_string_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use rojff_json_string_m, only: json_string_t, json_string_unsafe
    use rojff_json_value_m, only: json_value_t
    use rojff_parser_m, only: parse_json_string, INVALID_INPUT
    use rojff_string_cursor_m, only: string_cursor_t

    implicit none
    private
    public :: fallible_json_string_t

    type :: fallible_json_string_t
        type(json_string_t) :: string
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_string_t
        module procedure constructor
    end interface

    character (len=*), parameter :: MODULE_NAME = "rojff_fallible_json_string_m"
contains
    function constructor(string) result(fallible_string)
        character(len=*), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        character(len=*), parameter :: PROCEDURE_NAME = "constructor"
        type(string_cursor_t) :: cursor
        class(json_value_t), allocatable :: json

        cursor = string_cursor_t(string // '"')
        call parse_json_string(cursor, json, fallible_string%errors)
        if (fallible_string%errors%has_any()) then
            fallible_string%errors = error_list_t( &
                    fallible_string%errors, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else if (.not.cursor%finished()) then
            fallible_string%errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "Unescaped quote in string: '" // string // "'"))
        else
            fallible_string%string = json_string_unsafe(string)
        end if
    end function

    elemental function failed(self)
        class(fallible_json_string_t), intent(in) :: self
        logical :: failed

        failed = self%errors%has_any()
    end function
end module
