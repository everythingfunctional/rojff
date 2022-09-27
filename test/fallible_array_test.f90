module fallible_array_test
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use json_assertion, only: assert_equals
    use rojff, only: &
            fallible_json_array_t, &
            fallible_json_element_t, &
            json_array_t, &
            json_element_t, &
            json_null_t
    use veggies, only: result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_fallible_array
contains
    function test_fallible_array() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "constructing a json array", &
                [ it("produces the expected array if the values are valid", check_valid) &
                , it("forwards an error from a failed element", check_failed_element) &
                ])
    end function

    function check_valid() result(result_)
        type(result_t) :: result_

        type(fallible_json_array_t) :: maybe_array

        maybe_array = fallible_json_array_t( &
                [ fallible_json_element_t(json_null_t()) &
                , fallible_json_element_t(json_null_t()) &
                ])
        if (maybe_array%failed()) then
            result_ = fail(maybe_array%errors%to_string())
        else
            result_ = assert_equals( &
                    json_array_t( &
                            [ json_element_t(json_null_t()) &
                            , json_element_t(json_null_t()) &
                            ]), &
                    maybe_array%array)
        end if
    end function

    function check_failed_element() result(result_)
        type(result_t) :: result_

        type(fallible_json_array_t) :: maybe_array
        type(fallible_json_element_t) :: maybe_element

        maybe_element%errors = error_list_t(fatal_t( &
                module_t("fallible_object_test"), &
                procedure_t("check_failed_member"), &
                "Intentional failure"))
        maybe_array = fallible_json_array_t( &
                [ fallible_json_element_t(json_null_t()) &
                , maybe_element &
                ])
        result_ = assert_equals( &
                maybe_element%errors%to_string(), &
                maybe_array%errors%to_string())
    end function
end module