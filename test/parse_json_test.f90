module parse_json_test
    use integer_input_m, only: integer_input_t
    use iso_varying_string, only: operator(//)
    use json_assertion, only: assert_equals
    use number_input_m, only: number_input_t
    use rojff, only: &
            fallible_json_value_t, &
            json_bool_t, &
            json_integer_t, &
            json_null_t, &
            json_number_t, &
            parse_json_from_string
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
end module
