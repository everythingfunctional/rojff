module parse_json_test
    use rojff, only: fallible_json_value_t, parse_json_from_string
    use vegetables, only: result_t, test_item_t, assert_that, describe, it

    implicit none
    private
    public :: test_parse_json
contains
    function test_parse_json() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "parse_json", &
                [ it("parsing an empty string returns an error", check_parse_empty) &
                ])
    end function

    function check_parse_empty() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: json

        json = parse_json_from_string("")

        result_ = assert_that(json%errors%has_any(), json%errors%to_string())
    end function
end module
