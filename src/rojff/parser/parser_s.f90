submodule(rojff_parser_m) rojff_parser_s
    use erloff, only: fatal_t, module_t, procedure_t
    ! use iso_c_binding, only: c_char, c_double, c_ptr, c_null_char, c_null_ptr
    use rojff_constants_m, only: INVALID_INPUT
    use rojff_fallible_json_value_m, only: move_into_fallible_value
    use rojff_fallible_json_object_m, only: &
            fallible_json_object_t, move_into_fallible_object
    use rojff_file_cursor_m, only: file_cursor_t
    use rojff_json_array_m, only: json_array_t, move_into_array
    use rojff_json_bool_m, only: json_bool_t, create_json_bool
    use rojff_json_element_m, only: json_element_t
    use rojff_json_integer_m, only: json_integer_t, create_json_integer
    use rojff_json_linked_list_m, only: json_linked_list_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_null_m, only: json_null_t, create_json_null
    use rojff_json_number_m, only: json_number_t, create_json_number
    use rojff_json_object_m, only: json_object_t, move_into_object_unsafe
    use rojff_json_string_m, only: move_into_json_string_unsafe
    use rojff_json_value_m, only: json_value_t
    use rojff_member_linked_list_m, only: member_linked_list_t
    use rojff_string_cursor_m, only: string_cursor_t
    use rojff_utils_m, only: to_string

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_parser_m"
contains
    module procedure parse_json_from_file
        type(file_cursor_t) :: cursor
        class(json_value_t), allocatable :: json
        type(error_list_t) :: errors
        integer :: unit

        open( &
                newunit = unit, &
                file = filename, &
                status="OLD", &
                action="READ", &
                access="STREAM", &
                form="FORMATTED")
        cursor = file_cursor_t(unit)
        call parse_json(cursor, json, errors)
        close(unit)
        if (errors%has_any()) then
            fallible_json = fallible_json_value_t(error_list_t( &
                    errors, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_from_file")))
        else
            call move_into_fallible_value(fallible_json, json)
        end if
    end procedure

    module procedure parse_json_from_string
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
            call move_into_fallible_value(fallible_json, json)
        end if
    end procedure

    module procedure parse_json_string
        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_string"
        character(len=:), allocatable :: the_string
        character(len=1) :: next_character
        integer :: i

        allocate(character(len=0) :: the_string)
        do
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing string " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            next_character = cursor%peek()
            select case (next_character)
            case ('\')
                the_string = the_string // next_character
                call cursor%next()
                if (cursor%finished()) then
                    errors = error_list_t(fatal_t( &
                            INVALID_INPUT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "Unexpected end of input after escape character " &
                            // "At line " // to_string(cursor%current_line()) &
                            // " and column " // to_string(cursor%current_column())))
                    return
                end if
                next_character = cursor%peek()
                select case (next_character)
                case ('"', '\', '/', 'b', 'f', 'n', 'r', 't')
                    the_string = the_string // next_character
                case ('u')
                    the_string = the_string // next_character
                    do i = 1, 4
                        call cursor%next()
                        if (cursor%finished()) then
                            errors = error_list_t(fatal_t( &
                                    INVALID_INPUT, &
                                    module_t(MODULE_NAME), &
                                    procedure_t(PROCEDURE_NAME), &
                                    "Unexpected end of input while parsing escaped unicode string " &
                                    // "At line " // to_string(cursor%current_line()) &
                                    // " and column " // to_string(cursor%current_column())))
                            return
                        end if
                        next_character = cursor%peek()
                        if (index("0123456789abcdefABCDEF", next_character) == 0) then
                            errors = error_list_t(fatal_t( &
                                    INVALID_INPUT, &
                                    module_t(MODULE_NAME), &
                                    procedure_t(PROCEDURE_NAME), &
                                    "At line " // to_string(cursor%current_line()) &
                                    // " and column " // to_string(cursor%current_column()) &
                                    // " unexpected character in escaped unicode: found " // next_character &
                                    // ', but expected one of 0-9, a-f, or A-F'))
                            return
                        end if
                        the_string = the_string // next_character
                    end do
                case default
                    errors = error_list_t(fatal_t( &
                            INVALID_INPUT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "At line " // to_string(cursor%current_line()) &
                            // " and column " // to_string(cursor%current_column()) &
                            // " unexpected escaped character in string: found " // next_character &
                            // ', but expected one of ", \, /, b, f, n, r, t, or u'))
                    return
                end select
            case ('"')
                call cursor%next()
                exit
            case default
                the_string = the_string // next_character
            end select
            call cursor%next()
        end do
        call move_into_json_string_unsafe(json, the_string)
    end procedure

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
                allocate(character(len=0) :: trailing_content)
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
        type(json_array_t), allocatable :: array_val
        type(json_null_t), allocatable :: null_val
        type(json_bool_t), allocatable :: bool_val
        type(json_object_t), allocatable :: obj_val
        type(json_string_t), allocatable :: string_val

        next_character = cursor%peek()

        select case (next_character)
        case ("n")
            call parse_json_null(cursor, null_val, errors)
            call move_alloc(null_val, json)
        case ("t")
            call parse_json_true(cursor, bool_val, errors)
            call move_alloc(bool_val, json)
        case ("f")
            call parse_json_false(cursor, bool_val, errors)
            call move_alloc(bool_val, json)
        case ("+", "-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "N", "I")
            call parse_json_number(cursor, json, errors)
        case ('"')
            call cursor%next()
            call parse_json_string(cursor, string_val, errors)
            call move_alloc(string_val, json)
        case ('[')
            call cursor%next()
            call parse_json_array(cursor, array_val, errors)
            call move_alloc(array_val, json)
        case ('{')
            call cursor%next()
            call parse_json_object(cursor, obj_val, errors)
            call move_alloc(obj_val, json)
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
        type(json_null_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=4) :: null_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        null_string = "    "
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
                    // ', did you mean "' // null_string(1:min(i, 4)) //'..."'))
        end if
    end subroutine

    subroutine parse_json_true(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        type(json_bool_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=4) :: true_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        true_string = "    "
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
                    // ', did you mean "' // true_string(1:min(i, 4)) //'..."'))
        end if
    end subroutine

    subroutine parse_json_false(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        type(json_bool_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=5) :: false_string
        integer :: starting_line, starting_column, i

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        false_string = "     "
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
                    // ', did you mean "' // false_string(1:min(i, 4)) //'..."'))
        end if
    end subroutine

    subroutine parse_json_number(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        class(json_value_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        ! interface
        !     function strtod( str, endptr ) result(d) bind(C, name="strtod" )
        !         ! <stdlib.h> :: double strtod(const char *str, char **endptr)
        !         import
        !         character(kind=c_char,len=1),dimension(*),intent(in) :: str
        !         type(c_ptr), intent(inout) :: endptr
        !         real(c_double) :: d
        !     end function strtod
        ! end interface

        character(len=*), parameter :: NUMBER_SYMBOLS = "0123456789+-.eENaInf"
        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_number"
        character(len=1) :: next_character
        character(len=:), allocatable :: number_string
        integer :: starting_line, starting_column, stat, number_i, precision, exponent_location
        double precision :: number_d
        ! type(c_ptr) :: endptr
        type(json_integer_t), allocatable :: int_val
        type(json_number_t), allocatable :: num_val

        starting_line = cursor%current_line()
        starting_column = cursor%current_column()

        allocate(character(len=0) :: number_string)
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
                number_string == "NaN" &
                .or. number_string == "+NaN" &
                .or. number_string == "-NaN" &
                .or. number_string == "Inf" &
                .or. number_string == "+Inf" &
                .or. number_string == "-Inf") then
            read(number_string, *, iostat=stat) number_d
            if (stat == 0) then
                call create_json_number(num_val, number_d)
                call move_alloc(num_val, json)
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
        else if ( &
                index(number_string, "e") == 0 &
                .and. index(number_string, "E") == 0 &
                .and. index(number_string, ".") == 0) then
            read(number_string, *, iostat=stat) number_i
            if (stat == 0) then
                call create_json_integer(int_val, number_i)
                call move_alloc(int_val, json)
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
            ! This would have higher performance, but it's not catching errors sufficiently
            ! endptr = c_null_ptr
            ! number_d = strtod(number_string // c_null_char, endptr)
            ! if ((.not. number_d < 0.0d0) .and. (.not. number_d > 0.0d0)) then
            !     read(number_string, *, iostat=stat) number_d  ! not efficient - might really be 0.0
            ! else
            !     stat = 0
            ! end if
            read(number_string, *, iostat=stat) number_d  ! not efficient
            if (stat == 0) then
                exponent_location = index(number_string, "e")
                if (exponent_location == 0) exponent_location = index(number_string, "E")
                if (exponent_location == 0) then
                    precision = len_trim(number_string) - 1
                else
                    precision = exponent_location - merge(1, 2, index(number_string, ".") == 0)
                end if
                call create_json_number(num_val, number_d, precision)
                call move_alloc(num_val, json)
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

    recursive subroutine parse_json_array(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        type(json_array_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_array"
        type(json_element_t), allocatable :: elements(:)
        type(json_linked_list_t) :: parsed
        class(json_value_t), allocatable :: next_value

        call skip_whitespace(cursor)
        if (cursor%finished()) then
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "Unexpected end of input while parsing array " &
                    // "At line " // to_string(cursor%current_line()) &
                    // " and column " // to_string(cursor%current_column())))
            return
        end if
        if (cursor%peek() == ']') then
            allocate(elements(0))
            call move_into_array(json, elements)
            call cursor%next()
            return
        end if
        do
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing array " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            call parse_json_value(cursor, next_value, errors)
            if (errors%has_any()) then
                errors = error_list_t(errors, module_t(MODULE_NAME), procedure_t(PROCEDURE_NAME))
                return
            end if
            call parsed%append(next_value)
            call skip_whitespace(cursor)
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing array " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            select case (cursor%peek())
            case (',')
                call cursor%next()
                call skip_whitespace(cursor)
            case (']')
                call cursor%next()
                exit
            case default
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column()) &
                        // " unexpected character in array: found " // cursor%peek() &
                        // ', but expected one of ",", or "]"'))
                return
            end select
        end do
        call parsed%move_into_elements(elements)
        call move_into_array(json, elements)
    end subroutine

    recursive subroutine parse_json_object(cursor, json, errors)
        class(cursor_t), intent(inout) :: cursor
        type(json_object_t), allocatable, intent(out) :: json
        type(error_list_t), intent(out) :: errors

        character(len=*), parameter :: PROCEDURE_NAME = "parse_json_object"
        type(json_member_t), allocatable :: members(:)
        type(member_linked_list_t) :: parsed
        type(json_string_t), allocatable :: key
        class(json_value_t), allocatable :: val
        type(fallible_json_object_t) :: maybe_object

        call skip_whitespace(cursor)
        if (cursor%finished()) then
            errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "Unexpected end of input while parsing object " &
                    // "At line " // to_string(cursor%current_line()) &
                    // " and column " // to_string(cursor%current_column())))
            return
        end if
        if (cursor%peek() == '}') then
            allocate(members(0))
            call move_into_object_unsafe(json, members)
            call cursor%next()
            return
        end if
        do
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing object " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            if (cursor%peek() /= '"') then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column()) &
                        // " unexpected character in object: found " // cursor%peek() &
                        // ", but expected '""'"))
                return
            else
                call cursor%next()
            end if
            call parse_json_string(cursor, key, errors)
            if (errors%has_any()) then
                errors = error_list_t(errors, module_t(MODULE_NAME), procedure_t(PROCEDURE_NAME))
                return
            end if
            call skip_whitespace(cursor)
            if (cursor%peek() /= ":") then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column()) &
                        // " unexpected character in object: found " // cursor%peek() &
                        // ', but expected ":"'))
                return
            else
                call cursor%next()
            end if
            call skip_whitespace(cursor)
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing object " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            call parse_json_value(cursor, val, errors)
            if (errors%has_any()) then
                errors = error_list_t(errors, module_t(MODULE_NAME), procedure_t(PROCEDURE_NAME))
                return
            end if
            call parsed%append(key%string, val)
            if (cursor%finished()) then
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unexpected end of input while parsing object " &
                        // "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column())))
                return
            end if
            call skip_whitespace(cursor)
            select case (cursor%peek())
            case (',')
                call cursor%next()
                call skip_whitespace(cursor)
            case ('}')
                call cursor%next()
                exit
            case default
                errors = error_list_t(fatal_t( &
                        INVALID_INPUT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "At line " // to_string(cursor%current_line()) &
                        // " and column " // to_string(cursor%current_column()) &
                        // " unexpected character in object: found " // cursor%peek() &
                        // ', but expected one of ",", or "}"'))
                return
            end select
        end do
        call parsed%move_into_members(members)
        call move_into_fallible_object(maybe_object, members)
        if (maybe_object%failed()) then
            errors = error_list_t( &
                    maybe_object%errors, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            call move_alloc(maybe_object%object, json)
        end if
    end subroutine

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
end submodule