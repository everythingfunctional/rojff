module json_test
    use rojff, only: &
            json_value_t, &
            json_bool_t, &
            json_null_t, &
            create_json_bool, &
            create_json_null
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
                , it( &
                        "true has the correct string representation", &
                        check_true_to_string) &
                , it( &
                        "false has the correct string representation", &
                        check_false_to_string) &
                ])
    end function

    function check_null_to_string() result(result_)
        type(result_t) :: result_

        type(json_null_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_null_t()
        call create_json_null(created)

        result_ = &
                assert_equals("null", copied%to_compact_string(), "copied") &
                .and.assert_equals("null", created%to_compact_string(), "created")
    end function

    function check_true_to_string() result(result_)
        type(result_t) :: result_

        type(json_bool_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_bool_t(.true.)
        call create_json_bool(created, .true.)

        result_ = &
                assert_equals("true", copied%to_compact_string(), "copied") &
                .and.assert_equals("true", created%to_compact_string(), "created")
    end function

    function check_false_to_string() result(result_)
        type(result_t) :: result_

        type(json_bool_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_bool_t(.false.)
        call create_json_bool(created, .false.)

        result_ = &
                assert_equals("false", copied%to_compact_string(), "copied") &
                .and.assert_equals("false", created%to_compact_string(), "created")
    end function
end module
