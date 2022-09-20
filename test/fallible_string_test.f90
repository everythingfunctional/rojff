module fallible_string_test
    use erloff, only: module_t
    use json_assertion, only: assert_equals
    use rojff, only: fallible_json_string_t, json_string_unsafe
    use veggies, only: result_t, test_item_t, assert_that, describe, fail, it

    implicit none
    private
    public :: test_fallible_string
contains
    function test_fallible_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "constructing a json string", &
                [ it("produces the proper string if its valid", check_valid) &
                , it("produces an error if the string is invalid", check_invalid) &
                ])
    end function

    function check_valid() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: valid_string = "Hello"
        type(fallible_json_string_t) :: maybe_string

        maybe_string = fallible_json_string_t(valid_string)
        if (maybe_string%failed()) then
            result_ = fail(maybe_string%errors%to_string())
        else
            result_ = assert_equals(json_string_unsafe(valid_string), maybe_string%string)
        end if
    end function

    function check_invalid() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: invalid_string = 'thi"ng'
        type(fallible_json_string_t) :: maybe_string

        maybe_string = fallible_json_string_t(invalid_string)
        result_ = assert_that( &
                maybe_string%errors.hasAnyFrom.module_t("rojff_fallible_json_string_m"), &
                maybe_string%errors%to_string())
    end function
end module