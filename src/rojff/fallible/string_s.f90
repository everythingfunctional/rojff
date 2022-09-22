submodule(rojff_fallible_json_string_m) rojff_fallible_json_string_s
    use erloff, only: fatal_t
    use iso_varying_string, only: char
    use rojff_constants_m, only: INVALID_INPUT
    use rojff_json_string_m, only: json_string_unsafe
    use rojff_parser_m, only: parse_json_string
    use rojff_string_cursor_m, only: string_cursor_t

    implicit none

    character (len=*), parameter :: MODULE_NAME = "rojff_fallible_json_string_m"
contains
    module procedure from_character
        character(len=*), parameter :: PROCEDURE_NAME = "from_character"
        type(string_cursor_t) :: cursor
        type(json_string_t), allocatable :: json

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
    end procedure

    module procedure from_string
        fallible_string = fallible_json_string_t( &
                fallible_json_string_t(char(string)), &
                module_t(MODULE_NAME), &
                procedure_t("from_string"))
    end procedure

    module procedure from_json_string
        fallible_string%string = string
    end procedure

    module procedure from_fallible_string
        if (maybe_string%failed()) then
            fallible_string%errors = error_list_t( &
                    maybe_string%errors, module_, procedure_)
        else
            fallible_string%string = maybe_string%string
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule