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
    end type

    interface json_null_t
        module procedure constructor
    end interface
contains
    function constructor() result(json_null)
        type(json_null_t) :: json_null
    end function

    subroutine create_json_null(json)
        class(json_value_t), allocatable, intent(out) :: json

        allocate(json_null_t :: json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_null_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

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

        call sink%append("null")
    end subroutine
end module
