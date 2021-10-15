module parse_json_test
    use integer_input_m, only: integer_input_t
    use iso_varying_string, only: operator(//), put_line
    use json_assertion, only: assert_equals
    use number_input_m, only: number_input_t
    use rojff, only: &
            fallible_json_value_t, &
            json_array_t, &
            json_bool_t, &
            json_element_t, &
            json_integer_t, &
            json_member_t, &
            json_null_t, &
            json_number_t, &
            json_object_t, &
            json_value_t, &
            create_json_integer, &
            create_json_number, &
            create_json_string_unsafe, &
            json_member_unsafe, &
            json_string_unsafe, &
            move_into_array, &
            move_into_element, &
            move_into_member_unsafe, &
            move_into_object, &
            parse_json_from_file, &
            parse_json_from_string
    use strff, only: NEWLINE
    use vegetables, only: &
            example_t, &
            input_t, &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_parse_json
contains
    function test_parse_json() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "parse_json", &
                [ it("parsing an empty string returns an error", check_parse_empty) &
                , it("can parse null", check_parse_null) &
                , it("can parse true", check_parse_true) &
                , it("can parse false", check_parse_false) &
                , it( &
                        "can parse a variety of numbers", &
                        [ example_t(number_input_t("0.0", 0.0d0)) &
                        , example_t(number_input_t("-0.0", 0.0d0)) &
                        , example_t(number_input_t("2.0", 2.0d0)) &
                        , example_t(number_input_t("-2.0", -2.0d0)) &
                        , example_t(number_input_t("0.2", 0.2d0)) &
                        , example_t(number_input_t("1.2e3", 1.2d3)) &
                        , example_t(number_input_t("1.2E+3", 1.2d3)) &
                        , example_t(number_input_t("1.2e-3", 1.2d-3)) &
                        , example_t(number_input_t("20e1", 20.0d1)) &
                        ], &
                        check_parse_number) &
                , it( &
                        "when parsing a number, tracks how many digits of precision there were", &
                        check_number_significant_digits) &
                , it( &
                        "can parse a variety of integers", &
                        [ example_t(integer_input_t("0", 0)) &
                        , example_t(integer_input_t("-0", 0)) &
                        , example_t(integer_input_t("3", 3)) &
                        ], &
                        check_parse_integer) &
                , it("can parse a string", check_parse_string) &
                , it("can parse an empty array", check_parse_empty_array) &
                , it("can parse an array with a single element", check_parse_single_array) &
                , it("can parse an array with multiple elements", check_parse_multi_array) &
                , it("can parse an empty object", check_parse_empty_object) &
                , it("can parse an object with a single member", check_parse_single_object) &
                , it("can parse an object with multiple members", check_parse_multi_object) &
                , it("can parse data from a file", check_parse_from_file) &
                ])
    end function

    function check_parse_empty() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("")

        result_ = assert_that(json%errors%has_any(), json%errors%to_string())
    end function

    function check_parse_null() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string(" null ")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_null_t(), json%json)
        end if
    end function

    function check_parse_true() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string(" true ")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_bool_t(.true.), json%json)
        end if
    end function

    function check_parse_false() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string(" false ")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_bool_t(.false.), json%json)
        end if
    end function

    function check_parse_number(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        select type (input)
        type is (number_input_t)
            json = parse_json_from_string(input%string())
            result_ = assert_not(json%errors%has_any(), json%errors%to_string())
            if (result_%passed()) then
                result_ = assert_equals( &
                        json_number_t(input%value_()), &
                        json%json, &
                        "Original string: " // input%string())
            end if
        class default
            result_ = fail("Expected to get a number_input_t")
        end select
    end function

    function check_number_significant_digits() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("1.23e4")
        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            select type (number => json%json)
            type is (json_number_t)
                result_ = assert_equals(3, number%precision)
            class default
                result_ = fail("expected a json_number_t, but got: " // number%to_compact_string())
            end select
        end if
    end function

    function check_parse_integer(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        select type (input)
        type is (integer_input_t)
            json = parse_json_from_string(input%string())
            result_ = assert_not(json%errors%has_any(), json%errors%to_string())
            if (result_%passed()) then
                result_ = assert_equals( &
                        json_integer_t(input%value_()), &
                        json%json, &
                        "Original string: " // input%string())
            end if
        class default
            result_ = fail("Expected to get a integer_input_t")
        end select
    end function

    function check_parse_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: THE_STRING = &
                '"AB\"\\\/\b\n\r\t\u1a2f"'
        type(fallible_json_value_t) :: json

        json = parse_json_from_string(THE_STRING)

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_string_unsafe(THE_STRING(2:len(THE_STRING)-1)), json%json)
        end if
    end function

    function check_parse_empty_array() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("[ ]")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_array_t([json_element_t::]), json%json)
        end if
    end function

    function check_parse_single_array() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("[20e1]")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals( &
                    json_array_t([json_element_t(json_number_t(20d1))]), &
                    json%json)
        end if
    end function

    function check_parse_multi_array() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("[ null, null ,null ]")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals( &
                    json_array_t( &
                            [ json_element_t(json_null_t()) &
                            , json_element_t(json_null_t()) &
                            , json_element_t(json_null_t()) &
                            ]), &
                    json%json)
        end if
    end function

    function check_parse_empty_object() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("{ }")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_object_t([json_member_t::]), json%json)
        end if
    end function

    function check_parse_single_object() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string('{"first" : null}')

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals( &
                    json_object_t([json_member_unsafe("first", json_null_t())]), &
                    json%json)
        end if
    end function

    function check_parse_multi_object() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string('{"first" : null, "second" : null, "third" : null}')

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals( &
                    json_object_t( &
                            [ json_member_unsafe("first", json_null_t()) &
                            , json_member_unsafe("second", json_null_t()) &
                            , json_member_unsafe("third", json_null_t()) &
                            ]), &
                    json%json)
        end if
    end function

    function check_parse_from_file() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: TEMP_FILE_NAME = "temp_file.json"
        class(json_value_t), allocatable :: example
        integer :: file_unit
        type(fallible_json_value_t) :: parsed

        example = complex_example()
        call example%save_compactly_to(TEMP_FILE_NAME, status = "REPLACE")

        parsed = parse_json_from_file(TEMP_FILE_NAME)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")

        result_ = assert_not(parsed%errors%has_any(), parsed%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(example, parsed%json)
        end if
    end function

    function complex_example()
        class(json_value_t), allocatable :: complex_example
! {
!     "glossary" : {
!         "title" : "example glossary",
!         "GlossDiv" : {
!             "title" : "S",
!             "GlossList" : {
!                 "GlossEntry" : {
!                     "ID" : 101,
!                     "SortAs" : "SGML",
!                     "GlossTerm" : "Standard Generalized Markup Language",
!                     "Acronym" : "SGML",
!                     "Abbrev" : "ISO 8879:1986",
!                     "GlossDef" : {
!                         "para" : "A meta-markup language, used to create markup languages such as DocBook.",
!                         "GlossSeeAlso" : [
!                             "GML",
!                             "XML"
!                         ]
!                     },
!                     "GlossSee" : 123.456
!                 }
!             }
!         }
!     }
! }
!
! It should really be possible to construct this in the functional style
! (i.e. in a single expression), but for some reason gfortran crashes.
! So for now, we doing it the long and complicated way
        type(json_member_t), allocatable :: members(:)
        type(json_element_t), allocatable :: elements(:)

        allocate(elements(2))
        call create_json_string_unsafe(complex_example, "GML")
        call move_into_element(elements(1), complex_example)
        call create_json_string_unsafe(complex_example, "XML")
        call move_into_element(elements(2), complex_example)
        call move_into_array(complex_example, elements)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossSeeAlso", complex_example)
        call create_json_string_unsafe( &
                complex_example, &
                "A meta-markup language, used to create markup languages such as DocBook.")
        call move_into_member_unsafe(members(1), "para", complex_example)
        call move_into_object(complex_example, members)
        allocate(members(7))
        call move_into_member_unsafe(members(6), "GlossDef", complex_example)
        call create_json_integer(complex_example, 101)
        call move_into_member_unsafe(members(1), "ID", complex_example)
        call create_json_string_unsafe(complex_example, "SGML")
        call move_into_member_unsafe(members(2), "SortAs", complex_example)
        call create_json_string_unsafe(complex_example, "Standard Generalized Markup Language")
        call move_into_member_unsafe(members(3), "GlossTerm", complex_example)
        call create_json_string_unsafe(complex_example, "SGML")
        call move_into_member_unsafe(members(4), "Acronym", complex_example)
        call create_json_string_unsafe(complex_example, "ISO 8879:1986")
        call move_into_member_unsafe(members(5), "Abbrev", complex_example)
        call create_json_number(complex_example, 123.456d0, 6)
        call move_into_member_unsafe(members(7), "GlossSee", complex_example)
        call move_into_object(complex_example, members)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "GlossEntry", complex_example)
        call move_into_object(complex_example, members)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossList", complex_example)
        call create_json_string_unsafe(complex_example, "S")
        call move_into_member_unsafe(members(1), "title", complex_example)
        call move_into_object(complex_example, members)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossDiv", complex_example)
        call create_json_string_unsafe(complex_example, "example glossary")
        call move_into_member_unsafe(members(1), "title", complex_example)
        call move_into_object(complex_example, members)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "glossary", complex_example)
        call move_into_object(complex_example, members)
    end function
end module
