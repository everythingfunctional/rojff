module rojff_parser_m
    use erloff, only: error_list_t, fatal_t, message_type_t, module_t, procedure_t
    use iso_varying_string, only: operator(//)
    use rojff_cursor_m, only: cursor_t
    use rojff_fallible_json_value_m, only: &
            fallible_json_value_t, move_into_fallible_json
    use rojff_json_bool_m, only: create_json_bool
    use rojff_json_integer_m, only: create_json_integer
    use rojff_json_null_m, only: create_json_null
    use rojff_json_number_m, only: create_json_number
    use rojff_json_value_m, only: json_value_t
    use rojff_string_cursor_m, only: string_cursor_t
    use strff, only: to_string

    implicit none
    private
    public :: parse_json_from_string

    type(message_type_t), parameter :: INVALID_INPUT = message_type_t( &
            "Invalid Input")
    character(len=*), parameter :: MODULE_NAME = "rojff_parser_m"
contains
    subroutine parse_json(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=*), parameter :: PROCEDURE_NAME = "parse_json"

        call skip_whitespace(cursor)
        if (cursor%finished()) then
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "No json found"))
            return
        end if
        call parse_json_value(cursor, json, errors)
        if (errors%has_any()) then
            errors = error_list_t(errors, module_t(MODULE_NAME), procedure_t(PROCEDURE_NAME))
            return
        end if
        if (.not.cursor%finished()) then
            block
                character(len=:), allocatable :: trailing_content
                trailing_content = ""
                do while (.not. cursor%finished())
                    trailing_content = trailing_content // cursor%peek()
                    call cursor%next()
                end do
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected trailing content: " // trailing_content))
            end block
        end if
    end subroutine

    recursive subroutine parse_json_value(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_value"
        character(len=1) :: next_character

        next_character = cursor%peek()

        select case (next_character)
        case ("n")
            call parse_json_null(cursor, json, errors)
        case ("t")
            call parse_json_true(cursor, json, errors)
        case ("f")
            call parse_json_false(cursor, json, errors)
        case ("+", "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
            call parse_json_number(cursor, json, errors)
        case default
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "At line " // to_string(cursor%current_line()) &
                    // " and column " // to_string(cursor%current_column()) &
                    // " found " // next_character &
                    // ', but expected one of ", {, [, +, -, 0-9, true, false, or null'))
            return
        end select
        if (errors%has_any()) then
            errors = error_list_t(errors, module_t(MODULE_NAME), procedure_t(PROCEDURE_NAME))
        else
            call skip_whitespace(cursor)
        end if
    end subroutine

    subroutine parse_json_null(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=4) :: null_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        do i = 1, 4
            null_string(i:i) = cursor%peek()
            call cursor%next()
            if (cursor%finished()) exit
        end do
        if (null_string == "null") then
            call create_json_null(json)
        else
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_null"), &
                    "At line " // to_string(starting_line) &
                    // " and column " // to_string(starting_column) &
                    // " found " // null_string(1:min(i, 4)) &
                    // ', but expected null'))
        end if
    end subroutine

    subroutine parse_json_true(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=4) :: true_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        do i = 1, 4
            true_string(i:i) = cursor%peek()
            call cursor%next()
            if (cursor%finished()) exit
        end do
        if (true_string == "true") then
            call create_json_bool(json, .true.)
        else
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_true"), &
                    "At line " // to_string(starting_line) &
                    // " and column " // to_string(starting_column) &
                    // " found " // true_string(1:min(i, 4)) &
                    // ', but expected true'))
        end if
    end subroutine

    subroutine parse_json_false(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=5) :: false_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        do i = 1, 5
            false_string(i:i) = cursor%peek()
            call cursor%next()
            if (cursor%finished()) exit
        end do
        if (false_string == "false") then
            call create_json_bool(json, .false.)
        else
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_false"), &
                    "At line " // to_string(starting_line) &
                    // " and column " // to_string(starting_column) &
                    // " found " // false_string(1:min(i, 4)) &
                    // ', but expected false'))
        end if
    end subroutine

    subroutine parse_json_number(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=*), parameter :: NUMBER_SYMBOLS = "0123456789+-.eE"
        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_number"
        character(len=1) :: next_character
        character(len=:), allocatable :: number_string
        integer :: starting_line, starting_column, stat, number_i, precision, exponent_location
        double precision :: number_d

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        number_string = ""
        do while (.not. cursor%finished())
            next_character = cursor%peek()
            if (index(NUMBER_SYMBOLS, next_character) /= 0) then
                number_string = number_string // next_character
                call cursor%next()
            else
                exit
            end if
        end do
        if ( &
                index(number_string, "e") == 0 &
                .and. index(number_string, "E") == 0 &
                .and. index(number_string, ".") == 0) then
            read(number_string, *, iostat=stat) number_i
            if (stat == 0) then
                call create_json_integer(json, number_i)
            else
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(starting_line) &
                        // " and column " // to_string(starting_column) &
                        // " unable to parse " // number_string &
                        // " as an integer"))
            end if
        else
            read(number_string, *, iostat=stat) number_d
            if (stat == 0) then
                exponent_location = index(number_string, "e")
                if (exponent_location == 0) exponent_location = index(number_string, "E")
                if (exponent_location == 0) then
                    precision = len_trim(number_string) - 1
                else
                    precision = exponent_location - merge(1, 2, index(number_string, ".") == 0)
                end if
                call create_json_number(json, number_d, precision)
            else
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(starting_line) &
                        // " and column " // to_string(starting_column) &
                        // " unable to parse " // number_string &
                        // " as a number"))
            end if
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

    subroutine skip_whitespace(cursor)
        class(cursor_t), intent(inout) :: cursor

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE = char(10)
        character(len=1), parameter :: CARRIAGE_RETURN = char(13)
        character(len=1), parameter :: SPACE = char(32)
        character(len=*), parameter :: WHITESPACE = &
                            TAB // NEWLINE // CARRIAGE_RETURN // SPACE

        do while ((.not.cursor%finished()) .and. index(WHITESPACE, cursor%peek()) /= 0)
            call cursor%next()
        end do
    end subroutine
end module
