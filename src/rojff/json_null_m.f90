module rojff_json_null_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_null_t, create_json_null

    type, extends(json_value_t) :: json_null_t
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type
contains
    subroutine create_json_null(json)
        class(json_value_t), allocatable, intent(out) :: json

        allocate(json_null_t :: json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_null_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        associate(unused => lhs); end associate
        select type (rhs)
        type is (json_null_t)
            equals = .true.
        class default
            equals = .false.
        end select
    end function

    subroutine write_to_compactly(self, sink)
        class(json_null_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        associate(unused => self); end associate
        call sink%append("null")
    end subroutine

    subroutine write_to_expanded(self, indentation_level, sink)
        class(json_null_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        associate(unused => indentation_level); end associate
        associate(unused => self); end associate
        call sink%append("null")
    end subroutine
end module
