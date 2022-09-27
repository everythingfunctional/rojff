module fallible_element_test
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use json_assertion, only: assert_equals
    use rojff, only: fallible_json_element_t, fallible_json_value_t, json_null_t
    use veggies, only: result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_fallible_element
contains
    function test_fallible_element() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "constructing an element", &
                [ it("contains the given value", check_direct) &
                , it("gets the value from a valid fallible", check_valid) &
                , it("forwards an error from a failed value", check_failed) &
                ])
    end function

    function check_direct() result(result_)
        type(result_t) :: result_

        type(fallible_json_element_t) :: maybe_element

        maybe_element = fallible_json_element_t(json_null_t())
        if (maybe_element%failed()) then
            result_ = fail(maybe_element%errors%to_string())
        else
            result_ = assert_equals( &
                    json_null_t(), &
                    maybe_element%element%json)
        end if
    end function

    function check_valid() result(result_)
        type(result_t) :: result_

        type(fallible_json_element_t) :: maybe_element

        maybe_element = fallible_json_element_t(fallible_json_value_t(json_null_t()))
        if (maybe_element%failed()) then
            result_ = fail(maybe_element%errors%to_string())
        else
            result_ = assert_equals( &
                    json_null_t(), &
                    maybe_element%element%json)
        end if
    end function

    function check_failed() result(result_)
        type(result_t) :: result_

        type(fallible_json_element_t) :: maybe_element
        type(fallible_json_value_t) :: maybe_value

        maybe_value%errors = error_list_t(fatal_t( &
                module_t("fallible_member_test"), &
                procedure_t("check_failed_value"), &
                "Intentional failure"))
        maybe_element = fallible_json_element_t(maybe_value)
        result_ = assert_equals( &
                maybe_value%errors%to_string(), &
                maybe_element%errors%to_string())
    end function
end module