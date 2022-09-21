module parse_json_test
    use integer_input_m, only: integer_input_t
    use iso_varying_string, only: operator(//)
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
            parse_json_from_string, &
            INVALID_INPUT
    use veggies, only: &
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

        character(len=:), allocatable :: nan_str
        character(len=:), allocatable :: neg_nan_str
        character(len=:), allocatable :: pos_nan_str
        character(len=:), allocatable :: inf_str
        character(len=:), allocatable :: neg_inf_str
        character(len=:), allocatable :: pos_inf_str
        double precision :: nan, neg_nan, pos_nan, inf, neg_inf, pos_inf

        nan_str = "NaN"
        neg_nan_str = "-NaN"
        pos_nan_str = "+NaN"
        inf_str = "Inf"
        neg_inf_str = "-Inf"
        pos_inf_str = "+Inf"
        read(nan_str, *) nan
        read(neg_nan_str, *) neg_nan
        read(pos_nan_str, *) pos_nan
        read(inf_str, *) inf
        read(neg_inf_str, *) neg_inf
        read(pos_inf_str, *) pos_inf

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
                        , example_t(number_input_t(nan_str, nan)) &
                        , example_t(number_input_t(neg_nan_str, neg_nan)) &
                        , example_t(number_input_t(pos_nan_str, pos_nan)) &
                        , example_t(number_input_t(inf_str, inf)) &
                        , example_t(number_input_t(neg_inf_str, neg_inf)) &
                        , example_t(number_input_t(pos_inf_str, pos_inf)) &
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
                , it("fails if there is trailing content", check_trailing_content) &
                , it("fails if there are duplicate keys", check_duplicate_keys) &
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
        type(json_element_t) :: empty_elements(0)

        json = parse_json_from_string("[ ]")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_array_t(empty_elements), json%json)
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
        type(json_member_t) :: empty_members(0)

        json = parse_json_from_string("{ }")

        result_ = assert_not(json%errors%has_any(), json%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_object_t(empty_members), json%json)
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

    function check_trailing_content() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("null # with trailing content")

        result_ = assert_that(json%errors.hasType.INVALID_INPUT, json%errors%to_string())
    end function

    function check_duplicate_keys() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string('{"1" : 1, "1" : 2}')

        result_ = assert_that(json%errors.hasType.INVALID_INPUT, json%errors%to_string())
    end function

    function complex_example() result(example)
        class(json_value_t), allocatable :: example
        class(json_value_t), allocatable :: complex_example_
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
        call create_json_string_unsafe(complex_example_, "GML")
        call move_into_element(elements(1), complex_example_)
        call create_json_string_unsafe(complex_example_, "XML")
        call move_into_element(elements(2), complex_example_)
        call move_into_array(complex_example_, elements)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossSeeAlso", complex_example_)
        call create_json_string_unsafe( &
                complex_example_, &
                "A meta-markup language, used to create markup languages such as DocBook.")
        call move_into_member_unsafe(members(1), "para", complex_example_)
        call move_into_object(complex_example_, members)
        allocate(members(7))
        call move_into_member_unsafe(members(6), "GlossDef", complex_example_)
        call create_json_integer(complex_example_, 101)
        call move_into_member_unsafe(members(1), "ID", complex_example_)
        call create_json_string_unsafe(complex_example_, "SGML")
        call move_into_member_unsafe(members(2), "SortAs", complex_example_)
        call create_json_string_unsafe(complex_example_, "Standard Generalized Markup Language")
        call move_into_member_unsafe(members(3), "GlossTerm", complex_example_)
        call create_json_string_unsafe(complex_example_, "SGML")
        call move_into_member_unsafe(members(4), "Acronym", complex_example_)
        call create_json_string_unsafe(complex_example_, "ISO 8879:1986")
        call move_into_member_unsafe(members(5), "Abbrev", complex_example_)
        call create_json_number(complex_example_, 123.456d0, 6)
        call move_into_member_unsafe(members(7), "GlossSee", complex_example_)
        call move_into_object(complex_example_, members)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "GlossEntry", complex_example_)
        call move_into_object(complex_example_, members)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossList", complex_example_)
        call create_json_string_unsafe(complex_example_, "S")
        call move_into_member_unsafe(members(1), "title", complex_example_)
        call move_into_object(complex_example_, members)
        allocate(members(2))
        call move_into_member_unsafe(members(2), "GlossDiv", complex_example_)
        call create_json_string_unsafe(complex_example_, "example glossary")
        call move_into_member_unsafe(members(1), "title", complex_example_)
        call move_into_object(complex_example_, members)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "glossary", complex_example_)
        call move_into_object(complex_example_, members)
        call move_alloc(complex_example_, example)
    end function
end module
