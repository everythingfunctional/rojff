module json_test
    use rojff, only: json_null_t
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_includes, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_json
contains
    function test_json() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "JSON", &
                [ it( &
                        "null has the correct string representation", &
                        check_null_to_string) &
                ])
    end function

    function check_null_to_string() result(result_)
        type(result_t) :: result_

        type(json_null_t) :: json_null

        result_ = assert_equals("null", json_null%to_compact_string())
    end function
end module
