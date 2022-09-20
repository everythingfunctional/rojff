module fallible_member_test
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: var_str
    use json_assertion, only: assert_equals
    use rojff, only: &
            fallible_json_member_t, &
            fallible_json_string_t, &
            fallible_json_value_t, &
            json_null_t, &
            json_member_unsafe, &
            json_string_unsafe
    use veggies, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_fallible_member
contains
    function test_fallible_member() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "constructing a json member", &
                [ it("produces the expected member if it is valid", check_valid) &
                , it("produces an error if the key string is invalid", check_invalid_key) &
                , it("forwards an error from a failed key", check_failed_key) &
                , it("forwards an error from a failed value", check_failed_value) &
                ])
    end function

    function check_valid() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: valid_string = "Hello"
        type(fallible_json_member_t) :: maybe_member

        maybe_member = fallible_json_member_t(valid_string, json_null_t())
        if (maybe_member%failed()) then
            result_ = fail(maybe_member%errors%to_string())
        else
            result_ = assert_equals( &
                    json_member_unsafe(valid_string, json_null_t()), &
                    maybe_member%member)
        end if
    end function

    function check_invalid_key() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: invalid_string = 'thi"ng'
        type(fallible_json_member_t) :: maybe_from_character
        type(fallible_json_member_t) :: maybe_from_string

        maybe_from_character = fallible_json_member_t(invalid_string, json_null_t())
        maybe_from_string = fallible_json_member_t(var_str(invalid_string), json_null_t())
        result_ = &
            assert_that( &
                maybe_from_character%errors.hasAnyFrom.module_t("rojff_fallible_json_member_m"), &
                maybe_from_character%errors%to_string()) &
            .and.assert_that( &
                maybe_from_string%errors.hasAnyFrom.module_t("rojff_fallible_json_member_m"), &
                maybe_from_string%errors%to_string())
    end function

    function check_failed_key() result(result_)
        type(result_t) :: result_

        type(fallible_json_string_t) :: maybe_string
        type(fallible_json_member_t) :: maybe_member

        maybe_string%errors = error_list_t(fatal_t( &
                module_t("fallible_member_test"), &
                procedure_t("check_failed_key"), &
                "Intentional failure"))
        maybe_member = fallible_json_member_t(maybe_string, json_null_t())
        result_ = assert_equals( &
                maybe_string%errors%to_string(), &
                maybe_member%errors%to_string())
    end function

    function check_failed_value() result(result_)
        type(result_t) :: result_

        type(fallible_json_value_t) :: maybe_value
        type(fallible_json_member_t) :: maybe_member

        maybe_value%errors = error_list_t(fatal_t( &
                module_t("fallible_member_test"), &
                procedure_t("check_failed_value"), &
                "Intentional failure"))
        maybe_member = fallible_json_member_t("Hello", maybe_value)
        result_ = assert_equals( &
                maybe_value%errors%to_string(), &
                maybe_member%errors%to_string())
    end function
end module