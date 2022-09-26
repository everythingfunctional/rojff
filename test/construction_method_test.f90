module construction_method_test
    use iso_varying_string, only: var_str
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
            json_object_unsafe, &
            json_string_unsafe, &
            move_into_array, &
            move_into_element, &
            move_into_json_string_unsafe, &
            move_into_member, &
            move_into_member_unsafe, &
            move_into_object_unsafe
    use json_assertion, only: assert_equals
    use veggies, only: test_item_t, result_t, describe, it

    implicit none
    private
    public :: test_construction_method
contains
    function test_construction_method() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "It is possible to construct JSON using any combination of copy vs move and error checking vs not", &
            [ it("and have all values be the same", check_construction_methods) &
            ])
    end function

    function check_construction_methods() result(result_)
        type(result_t) :: result_

        type(json_object_t) :: copied_without_errors
        type(json_object_t), allocatable :: moved_without_errors

        copied_without_errors = copy_construct_without_errors()
        call move_construct_without_errors(moved_without_errors)

        result_ = assert_equals( &
                copied_without_errors, &
                moved_without_errors)
    end function

    function copy_construct_without_errors() result(object)
        type(json_object_t) :: object

        object = json_object_unsafe( &
            [ json_member_unsafe("1", json_null_t()) &
            , json_member_unsafe(var_str("2"), json_bool_t(.true.)) &
            , json_member_t(json_string_unsafe("3"), json_bool_t(.false.)) &
            , json_member_t(json_string_unsafe(var_str("4")), json_number_t(3.14d0)) &
            , json_member_unsafe("5", json_integer_t(42)) &
            , json_member_unsafe("6", json_string_unsafe("hello")) &
            , json_member_unsafe("7", json_object_unsafe([json_member_t::])) &
            , json_member_unsafe("8", json_array_t( &
                [ json_element_t(json_null_t()) &
                , json_element_t(json_bool_t(.true.)) &
                , json_element_t(json_bool_t(.false.)) &
                , json_element_t(json_number_t(3.14d0)) &
                , json_element_t(json_integer_t(42)) &
                , json_element_t(json_string_unsafe("world")) &
                , json_element_t(json_object_unsafe([json_member_t::])) &
                , json_element_t(json_array_t( &
                    [ json_element_t(json_null_t()) &
                    , json_element_t(json_bool_t(.true.)) &
                    , json_element_t(json_bool_t(.false.)) &
                    , json_element_t(json_number_t(3.14d0)) &
                    , json_element_t(json_integer_t(42)) &
                    , json_element_t(json_array_t([json_element_t::])) &
                    ])) &
                ])) &
            ])
    end function

    subroutine move_construct_without_errors(object)
        type(json_object_t), allocatable, intent(out) :: object

        type(json_member_t), allocatable :: top_members(:)
        type(json_string_t), allocatable :: key1
        type(json_null_t), allocatable :: top_null
        type(json_string_t), allocatable :: key2
        type(json_bool_t), allocatable :: top_true
        type(json_bool_t), allocatable :: top_false
        character(len=:), allocatable :: key3_char
        type(json_string_t), allocatable :: key3
        type(json_number_t), allocatable :: top_number
        type(json_integer_t), allocatable :: top_integer
        type(json_string_t), allocatable :: top_string
        type(json_object_t), allocatable :: top_empty_object
        type(json_member_t), allocatable :: top_empty_members(:)
        type(json_array_t), allocatable :: top_array
        type(json_element_t), allocatable :: mid_elements(:)
        type(json_null_t), allocatable :: mid_null
        type(json_bool_t), allocatable :: mid_true
        type(json_bool_t), allocatable :: mid_false
        type(json_number_t), allocatable :: mid_number
        type(json_integer_t), allocatable :: mid_integer
        type(json_string_t), allocatable :: mid_string
        type(json_object_t), allocatable :: mid_empty_object
        type(json_member_t), allocatable :: mid_empty_members(:)
        type(json_array_t), allocatable :: mid_array
        type(json_element_t), allocatable :: bottom_elements(:)
        type(json_null_t), allocatable :: bottom_null
        type(json_bool_t), allocatable :: bottom_true
        type(json_bool_t), allocatable :: bottom_false
        type(json_number_t), allocatable :: bottom_number
        type(json_integer_t), allocatable :: bottom_integer
        type(json_array_t), allocatable :: empty_array
        type(json_element_t), allocatable :: empty_elements(:)
        class(json_value_t), allocatable :: tmp_val

        allocate(top_members(8))

        call create_json_string_unsafe(key1, "1")
        call create_json_null(top_null)
        call move_alloc(top_null, tmp_val)
        call move_into_member(top_members(1), key1, tmp_val)

        call create_json_string_unsafe(key2, var_str("2"))
        call create_json_bool(top_true, .true.)
        call move_alloc(top_true, tmp_val)
        call move_into_member(top_members(2), key2, tmp_val)

        key3_char = "3"
        call move_into_json_string_unsafe(key3, key3_char)
        call create_json_bool(top_false, .false.)
        call move_alloc(top_false, tmp_val)
        call move_into_member(top_members(3), key3, tmp_val)

        call create_json_number(top_number, 3.14d0)
        call move_alloc(top_number, tmp_val)
        call move_into_member_unsafe(top_members(4), "4", tmp_val)

        call create_json_integer(top_integer, 42)
        call move_alloc(top_integer, tmp_val)
        call move_into_member_unsafe(top_members(5), var_str("5"), tmp_val)

        call create_json_string_unsafe(top_string, "hello")
        call move_alloc(top_string, tmp_val)
        call move_into_member_unsafe(top_members(6), "6", tmp_val)

        allocate(top_empty_members(0))
        call move_into_object_unsafe(top_empty_object, top_empty_members)
        call move_alloc(top_empty_object, tmp_val)
        call move_into_member_unsafe(top_members(7), "7", tmp_val)

        allocate(mid_elements(8))

        call create_json_null(mid_null)
        call move_alloc(mid_null, tmp_val)
        call move_into_element(mid_elements(1), tmp_val)

        call create_json_bool(mid_true, .true.)
        call move_alloc(mid_true, tmp_val)
        call move_into_element(mid_elements(2), tmp_val)

        call create_json_bool(mid_false, .false.)
        call move_alloc(mid_false, tmp_val)
        call move_into_element(mid_elements(3), tmp_val)

        call create_json_number(mid_number, 3.14d0)
        call move_alloc(mid_number, tmp_val)
        call move_into_element(mid_elements(4), tmp_val)

        call create_json_integer(mid_integer, 42)
        call move_alloc(mid_integer, tmp_val)
        call move_into_element(mid_elements(5), tmp_val)

        call create_json_string_unsafe(mid_string, "world")
        call move_alloc(mid_string, tmp_val)
        call move_into_element(mid_elements(6), tmp_val)

        allocate(mid_empty_members(0))
        call move_into_object_unsafe(mid_empty_object, mid_empty_members)
        call move_alloc(mid_empty_object, tmp_val)
        call move_into_element(mid_elements(7), tmp_val)

        allocate(bottom_elements(6))

        call create_json_null(bottom_null)
        call move_alloc(bottom_null, tmp_val)
        call move_into_element(bottom_elements(1), tmp_val)

        call create_json_bool(bottom_true, .true.)
        call move_alloc(bottom_true, tmp_val)
        call move_into_element(bottom_elements(2), tmp_val)

        call create_json_bool(bottom_false, .false.)
        call move_alloc(bottom_false, tmp_val)
        call move_into_element(bottom_elements(3), tmp_val)

        call create_json_number(bottom_number, 3.14d0)
        call move_alloc(bottom_number, tmp_val)
        call move_into_element(bottom_elements(4), tmp_val)

        call create_json_integer(bottom_integer, 42)
        call move_alloc(bottom_integer, tmp_val)
        call move_into_element(bottom_elements(5), tmp_val)

        allocate(empty_elements(0))
        call move_into_array(empty_array, empty_elements)
        call move_alloc(empty_array, tmp_val)
        call move_into_element(bottom_elements(6), tmp_val)

        call move_into_array(mid_array, bottom_elements)
        call move_alloc(mid_array, tmp_val)
        call move_into_element(mid_elements(8), tmp_val)

        call move_into_array(top_array, mid_elements)
        call move_alloc(top_array, tmp_val)
        call move_into_member_unsafe(top_members(8), "8", tmp_val)

        call move_into_object_unsafe(object, top_members)
    end subroutine
end module