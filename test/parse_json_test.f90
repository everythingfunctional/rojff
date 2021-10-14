module parse_json_test
    use json_assertion, only: assert_equals
    use rojff, only: &
            fallible_json_value_t, &
            json_bool_t, &
            json_null_t, &
            parse_json_from_string
    use vegetables, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

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
end module
