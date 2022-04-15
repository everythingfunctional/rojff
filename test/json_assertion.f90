module json_assertion
    use rojff, only: json_value_t
    use veggies, only: &
            result_t, &
            fail, &
            make_equals_failure_message, &
            make_equals_success_message, &
            succeed, &
            with_user_message

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_json
        module procedure assert_equals_json_with_message
    end interface
contains
    function assert_equals_json(expected, actual) result(result_)
        class(json_value_t), intent(in) :: expected
        class(json_value_t), intent(in) :: actual
        type(result_t) :: result_

        result_ = assert_equals(expected, actual, "")
    end function

    function assert_equals_json_with_message(expected, actual, message) result(result_)
        class(json_value_t), intent(in) :: expected
        class(json_value_t), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result_

        if (expected == actual) then
            result_ = succeed(with_user_message(make_equals_success_message( &
                    expected%to_compact_string()), &
                    message))
        else
            result_ = fail(with_user_message(make_equals_failure_message( &
                    expected%to_compact_string(), actual%to_compact_string()), &
                    message))
        end if
    end function
end module
