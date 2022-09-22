module json_test
    use erloff, only: NOT_FOUND, OUT_OF_BOUNDS
    use rojff, only: &
            fallible_json_string_t, &
            fallible_json_value_t, &
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
            json_object_unsafe, &
            json_string_unsafe, &
            move_into_array, &
            move_into_element, &
            move_into_member_unsafe, &
            move_into_object, &
            INVALID_INPUT
    use rojff_constants_m, only: NEWLINE
    use veggies, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_includes, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it
    use json_assertion, only: assert_equals
    use iso_varying_string, only: varying_string, assignment(=)
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
                , it("a string constructed with varying string is the same as one constructed with character", &
                        check_character_string_constructors) &
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
                , it( &
                        "a complex object has the correct string representation", &
                        check_complex_object_to_string) &
                , it( &
                        "can be generated in an expanded, easier to read form", &
                        check_pretty_printing) &
                , it("can extract a value from an array", get_value_from_array) &
                , it( &
                        "the elements of an array accessed via get match accessing them directly", &
                        get_array_elements) &
                , it( &
                        "extracting a value from an array with a position greater" &
                        // " than the number of elements in the array is an error", &
                        get_value_from_array_failure) &
                , it("can extract a value from an object", get_value_from_object) &
                , it( &
                        "the keys and values of an object are returned in a consistent order", &
                        get_keys_and_values) &
                , it( &
                        "extracting a value from an object with a key it doesn't have is an error", &
                        get_value_from_object_failure) &
                , it( &
                        "trying to create an invalid string is an error", &
                        check_string_error) &
                ])
    end function

    function check_null_to_string() result(result_)
        type(result_t) :: result_

        type(json_null_t) :: copied
        type(json_null_t), allocatable :: created

        copied = json_null_t()
        call create_json_null(created)

        result_ = &
                assert_equals("null", copied%to_compact_string(), "copied") &
                .and.assert_equals("null", created%to_compact_string(), "created")
    end function

    function check_true_to_string() result(result_)
        type(result_t) :: result_

        type(json_bool_t) :: copied
        type(json_bool_t), allocatable :: created

        copied = json_bool_t(.true.)
        call create_json_bool(created, .true.)

        result_ = &
                assert_equals("true", copied%to_compact_string(), "copied") &
                .and.assert_equals("true", created%to_compact_string(), "created")
    end function

    function check_false_to_string() result(result_)
        type(result_t) :: result_

        type(json_bool_t) :: copied
        type(json_bool_t), allocatable :: created

        copied = json_bool_t(.false.)
        call create_json_bool(created, .false.)

        result_ = &
                assert_equals("false", copied%to_compact_string(), "copied") &
                .and.assert_equals("false", created%to_compact_string(), "created")
    end function

    function check_string_to_string() result(result_)
        type(result_t) :: result_

        type(json_string_t) :: copied
        type(json_string_t), allocatable :: created

        copied = json_string_unsafe("Hello")
        call create_json_string_unsafe(created, "Hello")

        result_ = &
                assert_equals('"Hello"', copied%to_compact_string(), "copied") &
                .and.assert_equals('"Hello"', created%to_compact_string(), "created")
    end function

    function check_character_string_constructors() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: test_string = "Hello world"
        type(json_string_t) :: string_char, string_var
        type(varying_string) :: var_string

        var_string = test_string
        string_char = json_string_unsafe(test_string)
        string_var = json_string_unsafe(var_string)
        result_ = &
                assert_equals(string_char,string_var)
    end function

    function check_number_to_string() result(result_)
        type(result_t) :: result_

        type(json_number_t) :: copied
        type(json_number_t), allocatable :: created

        copied = json_number_t(1.0d0)
        call create_json_number(created, 1.0d0)

        result_ = &
                assert_equals('1.0', copied%to_compact_string(), "copied") &
                .and.assert_equals('1.0', created%to_compact_string(), "created")
    end function

    function check_number_with_precision() result(result_)
        type(result_t) :: result_

        type(json_number_t) :: copied
        type(json_number_t), allocatable :: created

        copied = json_number_t(1.234d0, 3)
        call create_json_number(created, 1.234d0, 3)

        result_ = &
                assert_equals('1.23', copied%to_compact_string(), "copied") &
                .and.assert_equals('1.23', created%to_compact_string(), "created")
    end function

    function check_integer_to_string() result(result_)
        type(result_t) :: result_

        type(json_integer_t) :: copied
        type(json_integer_t), allocatable :: created

        copied = json_integer_t(1)
        call create_json_integer(created, 1)

        result_ = &
                assert_equals('1', copied%to_compact_string(), "copied") &
                .and.assert_equals('1', created%to_compact_string(), "created")
    end function

    function check_array_to_string() result(result_)
        type(result_t) :: result_

        type(json_array_t) :: copied
        type(json_array_t), allocatable :: created
        class(json_value_t), allocatable :: next_val
        type(json_element_t), allocatable :: elements(:)
        type(json_null_t), allocatable :: null_val
        type(json_number_t), allocatable :: num_val
        type(json_string_t), allocatable :: string_val

        copied = json_array_t( &
                [ json_element_t(json_null_t()) &
                , json_element_t(json_string_unsafe("Hello")) &
                , json_element_t(json_number_t(2.0d0)) &
                ])

        allocate(elements(3))
        call create_json_null(null_val)
        call move_alloc(null_val, next_val)
        call move_into_element(elements(1), next_val)
        call create_json_string_unsafe(string_val, "Hello")
        call move_alloc(string_val, next_val)
        call move_into_element(elements(2), next_val)
        call create_json_number(num_val, 2.0d0)
        call move_alloc(num_val, next_val)
        call move_into_element(elements(3), next_val)
        call move_into_array(created, elements)

        result_ = &
                assert_equals('[null,"Hello",2.0]', copied%to_compact_string(), "copied") &
                .and.assert_equals('[null,"Hello",2.0]', created%to_compact_string(), "created")
    end function

    function check_object_to_string() result(result_)
        type(result_t) :: result_

        type(json_object_t) :: copied
        type(json_bool_t), allocatable :: bool_val
        character(len=:), allocatable :: copied_string
        class(json_value_t), allocatable :: created
        character(len=:), allocatable :: created_string
        type(json_member_t), allocatable :: members(:)
        type(json_number_t), allocatable :: num_val

        copied = json_object_unsafe( &
                [ json_member_unsafe("sayHello", json_bool_t(.true.)) &
                , json_member_unsafe("aNumber", json_number_t(3.0d0)) &
                ])

        allocate(members(2))
        call create_json_bool(bool_val, .true.)
        call move_alloc(bool_val, created)
        call move_into_member_unsafe(members(1), "sayHello", created)
        call create_json_number(num_val, 3.0d0)
        call move_alloc(num_val, created)
        call move_into_member_unsafe(members(2), "aNumber", created)
        call move_into_object(created, members)

        allocate(copied_string, source = copied%to_compact_string())
        allocate(created_string, source = created%to_compact_string())

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

    function check_complex_object_to_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
                '{"Hello":[null,{"World":1.0},true]}'
        type(json_array_t), allocatable :: array_val
        type(json_object_t) :: copied
        type(json_bool_t), allocatable :: bool_val
        class(json_value_t), allocatable :: created
        type(json_element_t), allocatable :: elements(:)
        type(json_member_t), allocatable :: members(:)
        type(json_null_t), allocatable :: null_val
        type(json_number_t), allocatable :: num_val

        copied = json_object_unsafe( &
                [ json_member_unsafe("Hello", json_array_t( &
                        [ json_element_t(json_null_t()) &
                        , json_element_t(json_object_unsafe([json_member_unsafe("World", json_number_t(1.0d0))])) &
                        , json_element_t(json_bool_t(.true.)) &
                        ])) &
                ])

        allocate(elements(3))
        call create_json_null(null_val)
        call move_alloc(null_val, created)
        call move_into_element(elements(1), created)
        allocate(members(1))
        call create_json_number(num_val, 1.0d0, 2)
        call move_alloc(num_val, created)
        call move_into_member_unsafe(members(1), "World", created)
        call move_into_object(created, members)
        call move_into_element(elements(2), created)
        call create_json_bool(bool_val, .true.)
        call move_alloc(bool_val, created)
        call move_into_element(elements(3), created)
        call move_into_array(array_val, elements)
        call move_alloc(array_val, created)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "Hello", created)
        call move_into_object(created, members)

        result_ = &
                assert_equals(EXPECTED, copied%to_compact_string(), "copied") &
                .and.assert_equals(EXPECTED, created%to_compact_string(), "created")
    end function

    function check_pretty_printing() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "Hello" : [' // NEWLINE &
// '        null,' // NEWLINE &
// '        {' // NEWLINE &
// '            "World" : 1.0' // NEWLINE &
// '        },' // NEWLINE &
// '        true' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(json_object_t) :: copied
        type(json_array_t), allocatable :: array_val
        type(json_bool_t), allocatable :: bool_val
        class(json_value_t), allocatable :: created
        type(json_element_t), allocatable :: elements(:)
        type(json_member_t), allocatable :: members(:)
        type(json_null_t), allocatable :: null_val
        type(json_number_t), allocatable :: num_val

        copied = json_object_unsafe( &
                [ json_member_unsafe("Hello", json_array_t( &
                        [ json_element_t(json_null_t()) &
                        , json_element_t(json_object_unsafe([json_member_unsafe("World", json_number_t(1.0d0))])) &
                        , json_element_t(json_bool_t(.true.)) &
                        ])) &
                ])

        allocate(elements(3))
        call create_json_null(null_val)
        call move_alloc(null_val, created)
        call move_into_element(elements(1), created)
        allocate(members(1))
        call create_json_number(num_val, 1.0d0, 2)
        call move_alloc(num_val, created)
        call move_into_member_unsafe(members(1), "World", created)
        call move_into_object(created, members)
        call move_into_element(elements(2), created)
        call create_json_bool(bool_val, .true.)
        call move_alloc(bool_val, created)
        call move_into_element(elements(3), created)
        call move_into_array(array_val, elements)
        call move_alloc(array_val, created)
        allocate(members(1))
        call move_into_member_unsafe(members(1), "Hello", created)
        call move_into_object(created, members)

        result_ = &
                assert_equals(EXPECTED, copied%to_expanded_string(), "copied") &
                .and.assert_equals(EXPECTED, created%to_expanded_string(), "created")
    end function

    function get_value_from_array() result(result_)
        type(result_t) :: result_

        type(json_array_t) :: array
        type(fallible_json_value_t) :: retrieved

        array = json_array_t(json_element_t( &
                [ json_string_unsafe("first") &
                , json_string_unsafe("second") &
                , json_string_unsafe("third") &
                ]))

        retrieved = array%get(3)

        result_ = assert_not(retrieved%errors%has_any(), retrieved%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_string_unsafe("third"), retrieved%json)
        end if
    end function

    function get_array_elements() result(result_)
        type(result_t) :: result_

        type(json_array_t) :: array
        integer :: i
        type(fallible_json_value_t) :: maybe_item

        array = json_array_t(json_element_t( &
                [ json_string_unsafe("first") &
                , json_string_unsafe("second") &
                , json_string_unsafe("third") &
                ]))

        do i = 1, size(array%elements)
            maybe_item = array%get(i)
            if (maybe_item%failed()) then
                result_ = result_.and.fail(maybe_item%errors%to_string())
            else
                result_ = result_.and.assert_equals(array%elements(i)%json, maybe_item%json)
            end if
        end do
    end function

    function get_value_from_array_failure() result(result_)
        type(result_t) :: result_

        type(json_array_t) :: array
        type(fallible_json_value_t) :: retrieved

        array = json_array_t(json_element_t( &
                [ json_string_unsafe("first") &
                , json_string_unsafe("second") &
                , json_string_unsafe("third") &
                ]))

        retrieved = array%get(4)

        result_ = assert_that( &
                retrieved%errors.hasType.OUT_OF_BOUNDS, retrieved%errors%to_string())
    end function

    function get_value_from_object() result(result_)
        type(result_t) :: result_

        type(json_object_t) :: object
        type(fallible_json_value_t) :: retrieved

        object = json_object_unsafe( &
                [ json_member_unsafe("first", json_string_unsafe("hello")) &
                , json_member_unsafe("second", json_string_unsafe("goodbye")) &
                ])

        retrieved = object%get("first")

        result_ = assert_not(retrieved%errors%has_any(), retrieved%errors%to_string())
        if (result_%passed()) then
            result_ = assert_equals(json_string_unsafe("hello"), retrieved%json)
        end if
    end function

    function get_keys_and_values() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: keys(:)
        integer :: i
        type(fallible_json_value_t) :: maybe_value
        type(json_object_t) :: object
        type(json_element_t), allocatable :: values(:)

        object = json_object_unsafe( &
                [ json_member_unsafe("first", json_string_unsafe("hello")) &
                , json_member_unsafe("second", json_string_unsafe("world")) &
                , json_member_unsafe("third", json_string_unsafe("goodbye")) &
                ])
        keys = object%keys()
        values = object%values()
        result_ = assert_equals(size(keys), size(values), "number of members")
        do i = 1, size(keys)
            maybe_value = object%get(keys(i))
            if (maybe_value%failed()) then
                result_ = result_.and.fail(maybe_value%errors%to_string())
            else
                result_ = result_.and.assert_equals(values(i)%json, maybe_value%json)
            end if
        end do
    end function

    function get_value_from_object_failure() result(result_)
        type(result_t) :: result_

        type(json_object_t) :: object
        type(fallible_json_value_t) :: retrieved

        object = json_object_unsafe( &
                [ json_member_unsafe("first", json_string_unsafe("hello")) &
                , json_member_unsafe("second", json_string_unsafe("goodbye")) &
                ])

        retrieved = object%get("third")

        result_ = assert_that(retrieved%errors.hasType.NOT_FOUND, retrieved%errors%to_string())
    end function

    function check_string_error() result(result_)
        type(result_t) :: result_

        type(fallible_json_string_t) :: string

        string = fallible_json_string_t("\invalid")

        result_ = assert_that(string%errors.hasType.INVALID_INPUT, string%errors%to_string())
    end function
end module
