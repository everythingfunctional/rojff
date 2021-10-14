module json_assertion
    use rojff, only: json_value_t
    use vegetables, only: &
            result_t, &
            fail, &
            make_equals_failure_message, &
            make_equals_success_message, &
            succeed

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_json
    end interface
contains
    function assert_equals_json(expected, actual) result(result_)
        class(json_value_t), intent(in) :: expected
        class(json_value_t), intent(in) :: actual
        type(result_t) :: result_

        if (expected == actual) then
            result_ = succeed(make_equals_success_message( &
                    expected%to_compact_string()))
        else
            result_ = fail(make_equals_failure_message( &
                    expected%to_compact_string(), actual%to_compact_string()))
        end if
    end function
end module
