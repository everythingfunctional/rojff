module rojff_parser_m
    use erloff, only: error_list_t, fatal_t, message_type_t, module_t, procedure_t
    use rojff_cursor_m, only: cursor_t
    use rojff_fallible_json_value_m, only: &
            fallible_json_value_t, move_into_fallible_json
    use rojff_json_value_m, only: json_value_t
    use rojff_string_cursor_m, only: string_cursor_t

    implicit none
    private
    public :: parse_json_from_string

    character(len=1), parameter :: TAB = char(9)
    character(len=1), parameter :: NEWLINE = char(10)
    character(len=1), parameter :: CARRIAGE_RETURN = char(13)
    character(len=1), parameter :: SPACE = char(32)
    character(len=*), parameter :: WHITESPACE = &
                        TAB // NEWLINE // CARRIAGE_RETURN // SPACE
    type(message_type_t), parameter :: INVALID_INPUT = message_type_t( &
            "Invalid Input")
    character(len=*), parameter :: MODULE_NAME = "rojff_parser_m"
contains
    subroutine parse_json(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        do while ((.not.cursor%finished()) .and. index(WHITESPACE, cursor%peek()) /= 0)
            call cursor%next()
        end do
        if (cursor%finished()) then
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json"), &
                    "No json found"))
            return
        end if
    end subroutine

    function parse_json_from_string(string) result(fallible_json)
        character(len=*), intent(in) :: string
        type(fallible_json_value_t) :: fallible_json

        type(string_cursor_t) :: cursor
        class(json_value_t), allocatable :: json
        type(error_list_t) :: errors

        cursor = string_cursor_t(string)
        call parse_json(cursor, json, errors)
        if (errors%has_any()) then
            fallible_json = fallible_json_value_t(error_list_t( &
                    errors, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_from_string")))
        else
            call move_into_fallible_json(fallible_json, json)
        end if
    end function
end module
