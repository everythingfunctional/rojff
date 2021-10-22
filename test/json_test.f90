module json_test
    use rojff, only: &
            json_array_t, &
            json_bool_t, &
            json_element_t, &
            json_integer_t, &
            json_member_t, &
            json_null_t, &
            json_number_t, &
            json_object_t, &
            json_string_t, &
            json_value_t, &
            create_json_bool, &
            create_json_integer, &
            create_json_null, &
            create_json_number, &
            create_json_string_unsafe, &
            json_member_unsafe, &
            json_string_unsafe, &
            move_into_array, &
            move_into_element, &
            move_into_member_unsafe, &
            move_into_object
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
                , it("a string can be converted back", check_string_to_string) &
                , it("a character can be converted back to a string", check_character_to_string) &
                , it( &
                        "a number has the correct string representation", &
                        check_number_to_string) &
                , it( &
                        "a number gets converted to a string with the specified precision", &
                        check_number_with_precision) &
                , it( &
                        "an integer has the correct string representation", &
                        check_integer_to_string) &
                , it( &
                        "an array has the correct string representation", &
                        check_array_to_string) &
                , it( &
                        "an object has the correct string representation", &
                        check_object_to_string) &
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

    function check_string_to_string() result(result_)
        type(result_t) :: result_

        type(json_string_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_string_unsafe("Hello")
        call create_json_string_unsafe(created, "Hello")

        result_ = &
                assert_equals('"Hello"', copied%to_compact_string(), "copied") &
                .and.assert_equals('"Hello"', created%to_compact_string(), "created")
    end function

    function check_character_to_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: test_string = "Hello world"
        type(json_string_t) :: created

        created = json_string_t(test_string)
        result_ = assert_equals('"Hello world"', created%to_compact_string())
    end function

    function check_number_to_string() result(result_)
        type(result_t) :: result_

        type(json_number_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_number_t(1.0d0)
        call create_json_number(created, 1.0d0)

        result_ = &
                assert_equals('1.0', copied%to_compact_string(), "copied") &
                .and.assert_equals('1.0', created%to_compact_string(), "created")
    end function

    function check_number_with_precision() result(result_)
        type(result_t) :: result_

        type(json_number_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_number_t(1.234d0, 3)
        call create_json_number(created, 1.234d0, 3)

        result_ = &
                assert_equals('1.23', copied%to_compact_string(), "copied") &
                .and.assert_equals('1.23', created%to_compact_string(), "created")
    end function

    function check_integer_to_string() result(result_)
        type(result_t) :: result_

        type(json_integer_t) :: copied
        class(json_value_t), allocatable :: created

        copied = json_integer_t(1)
        call create_json_integer(created, 1)

        result_ = &
                assert_equals('1', copied%to_compact_string(), "copied") &
                .and.assert_equals('1', created%to_compact_string(), "created")
    end function

    function check_array_to_string() result(result_)
        type(result_t) :: result_

        type(json_array_t) :: copied
        class(json_value_t), allocatable :: created
        type(json_element_t), allocatable :: elements(:)

        copied = json_array_t( &
                [ json_element_t(json_null_t()) &
                , json_element_t(json_string_unsafe("Hello")) &
                , json_element_t(json_number_t(2.0d0)) &
                ])

        allocate(elements(3))
        call create_json_null(created)
        call move_into_element(elements(1), created)
        call create_json_string_unsafe(created, "Hello")
        call move_into_element(elements(2), created)
        call create_json_number(created, 2.0d0)
        call move_into_element(elements(3), created)
        call move_into_array(created, elements)

        result_ = &
                assert_equals('[null,"Hello",2.0]', copied%to_compact_string(), "copied") &
                .and.assert_equals('[null,"Hello",2.0]', created%to_compact_string(), "created")
    end function

    function check_object_to_string() result(result_)
        type(result_t) :: result_

        type(json_object_t) :: copied
        character(len=:), allocatable :: copied_string
        class(json_value_t), allocatable :: created
        character(len=:), allocatable :: created_string
        type(json_member_t), allocatable :: members(:)

        copied = json_object_t( &
                [ json_member_unsafe("sayHello", json_bool_t(.true.)) &
                , json_member_unsafe("aNumber", json_number_t(3.0d0)) &
                ])

        allocate(members(2))
        call create_json_bool(created, .true.)
        call move_into_member_unsafe(members(1), "sayHello", created)
        call create_json_number(created, 3.0d0)
        call move_into_member_unsafe(members(2), "aNumber", created)
        call move_into_object(created, members)

        copied_string = copied%to_compact_string()
        created_string = created%to_compact_string()

        result_ = &
                assert_includes('"sayHello":true', copied_string, "copied") &
                .and.assert_includes('"sayHello":true', created_string, "created") &
                .and.assert_includes('"aNumber":3.0', copied_string, "copied") &
                .and.assert_includes('"aNumber":3.0', created_string, "created") &
                .and.assert_includes("{", copied_string, "copied") &
                .and.assert_includes("{", created_string, "created") &
                .and.assert_includes("}", copied_string, "copied") &
                .and.assert_includes("}", created_string, "created") &
                .and.assert_includes(",", copied_string, "copied") &
                .and.assert_includes(",", created_string, "created")
    end function
end module
