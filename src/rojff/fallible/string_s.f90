submodule(rojff_fallible_json_string_m) rojff_fallible_json_string_s
    use erloff, only: fatal_t
    use iso_varying_string, only: char
    use rojff_constants_m, only: INVALID_INPUT
    use rojff_fallible_json_value_m, only: move_into_fallible_value_ => move_into_fallible_value
    use rojff_json_value_m, only: json_value_t
    use rojff_parser_m, only: parse_json_string
    use rojff_string_cursor_m, only: string_cursor_t

    implicit none

    character (len=*), parameter :: MODULE_NAME = "rojff_fallible_json_string_m"
contains
    module procedure from_character
        type(fallible_json_string_t) :: local

        call create_fallible_json_string(local, string)
        fallible_string = fallible_json_string_t( &
                local, module_t(MODULE_NAME), procedure_t("from_character"))
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

    module procedure from_errors
        fallible_string%errors = errors
    end procedure

    module procedure from_fallible_string
        if (maybe_string%failed()) then
            fallible_string%errors = error_list_t( &
                    maybe_string%errors, module_, procedure_)
        else
            fallible_string%string = maybe_string%string
        end if
    end procedure

    module procedure create_from_character
        character(len=:), allocatable :: local_string
        type(fallible_json_string_t) :: maybe_string

        local_string = string
        call move_into_fallible_string(maybe_string, local_string)
        call move_into_fallible_string(fallible_string, maybe_string, module_t(MODULE_NAME), procedure_t("create_from_character"))
    end procedure

    module procedure create_from_string
        type(fallible_json_string_t) :: local_string

        call create_fallible_json_string(local_string, char(string))
        call move_into_fallible_string( &
                fallible_string, &
                local_string, &
                module_t(MODULE_NAME), &
                procedure_t("create_from_string"))
    end procedure

    module procedure move_from_character
        character(len=*), parameter :: PROCEDURE_NAME = "move_from_character"
        type(string_cursor_t) :: cursor

        cursor = string_cursor_t(string // '"')
        call parse_json_string(cursor, fallible_string%string, fallible_string%errors)
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
        end if
        deallocate(string)
    end procedure

    module procedure move_from_json_string
        call move_alloc(string, fallible_string%string)
    end procedure

    module procedure move_from_fallible_string
        if (maybe_string%failed()) then
            fallible_string%errors = error_list_t( &
                    maybe_string%errors, module_, procedure_)
        else
            call move_alloc(maybe_string%string, fallible_string%string)
        end if
    end procedure

    module procedure fallible_json_value_from_fallible_string
        if (maybe_string%failed()) then
            fallible_value = fallible_json_value_t(maybe_string%errors)
        else
            fallible_value = fallible_json_value_t(maybe_string%string)
        end if
    end procedure

    module procedure move_fallible_string_into_fallible_value
        class(json_value_t), allocatable :: tmp_val

        if (fallible_string%failed()) then
            fallible_value = fallible_json_value_t(fallible_string%errors)
        else
            call move_alloc(fallible_string%string, tmp_val)
            call move_into_fallible_value_(fallible_value, tmp_val)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule