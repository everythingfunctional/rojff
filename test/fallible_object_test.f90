module fallible_object_test
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use json_assertion, only: assert_equals
    use rojff, only: &
            fallible_json_member_t, &
            fallible_json_object_t, &
            json_integer_t, &
            json_member_t, &
            json_member_unsafe, &
            json_object_unsafe
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
    public :: test_fallible_object
contains
    function test_fallible_object() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "constructing a json object", &
                [ it("produces the expected object if the keys are unique", check_valid) &
                , it("produces an error if there is a duplicate key", check_duplicate) &
                , it("forwards an error from a failed member", check_failed_member) &
                ])
    end function

    function check_valid() result(result_)
        type(result_t) :: result_

        type(fallible_json_object_t) :: maybe_object
        type(json_member_t), allocatable :: members(:)

        allocate(members, source = &
                [ json_member_unsafe("1", json_integer_t(1)) &
                , json_member_unsafe("2", json_integer_t(2)) &
                ])
        maybe_object = fallible_json_object_t(members)
        if (maybe_object%failed()) then
            result_ = fail(maybe_object%errors%to_string())
        else
            result_ = assert_equals( &
                    json_object_unsafe(members), &
                    maybe_object%object)
        end if
    end function

    function check_duplicate() result(result_)
        type(result_t) :: result_

        type(fallible_json_object_t) :: maybe_object
        type(json_member_t), allocatable :: members(:)

        allocate(members, source = &
                [ json_member_unsafe("1", json_integer_t(1)) &
                , json_member_unsafe("1", json_integer_t(2)) &
                ])
        maybe_object = fallible_json_object_t(members)
        result_ = assert_that(&
                maybe_object%errors.hasAnyFrom.module_t("rojff_fallible_json_object_m"), &
                maybe_object%errors%to_string())
    end function

    function check_failed_member() result(result_)
        type(result_t) :: result_

        type(fallible_json_member_t) :: maybe_member
        type(fallible_json_object_t) :: maybe_object

        maybe_member%errors = error_list_t(fatal_t( &
                module_t("fallible_object_test"), &
                procedure_t("check_failed_member"), &
                "Intentional failure"))
        maybe_object = fallible_json_object_t([maybe_member])
        result_ = assert_equals( &
                maybe_member%errors%to_string(), &
                maybe_object%errors%to_string())
    end function
end module