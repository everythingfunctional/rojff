module rojff_fallible_json_string_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: varying_string, char
    use rojff_json_string_m, only: json_string_t, json_string_unsafe
    use rojff_json_value_m, only: json_value_t
    use rojff_parser_m, only: parse_json_string
    use rojff_string_cursor_m, only: string_cursor_t
    use rojff_utils_m, only: INVALID_INPUT

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
        module procedure from_character
        module procedure from_string
        module procedure from_json_string
        module procedure from_fallible_string
    end interface

    character (len=*), parameter :: MODULE_NAME = "rojff_fallible_json_string_m"
contains
    function from_character(string) result(fallible_string)
        character(len=*), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        character(len=*), parameter :: PROCEDURE_NAME = "from_character"
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

    function from_string(string) result(fallible_string)
        type(varying_string), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        fallible_string = fallible_json_string_t( &
                fallible_json_string_t(char(string)), &
                module_t(MODULE_NAME), &
                procedure_t("from_string"))
    end function

    function from_json_string(string) result(fallible_string)
        type(json_string_t), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        fallible_string%string = string
    end function

    function from_fallible_string( &
            maybe_string, module_, procedure_) result(fallible_string)
        type(fallible_json_string_t), intent(in) :: maybe_string
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_string_t) :: fallible_string

        if (maybe_string%failed()) then
            fallible_string%errors = error_list_t( &
                    maybe_string%errors, module_, procedure_)
        else
            fallible_string%string = maybe_string%string
        end if
    end function

    elemental function failed(self)
        class(fallible_json_string_t), intent(in) :: self
        logical :: failed

        failed = self%errors%has_any()
    end function
end module
