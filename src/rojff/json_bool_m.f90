module rojff_json_bool_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_bool_t, create_json_bool

    type, extends(json_value_t) :: json_bool_t
        logical :: bool
    contains
        procedure :: equals
        procedure :: write_to_compactly
    end type

    interface json_bool_t
        module procedure constructor
    end interface
contains
    elemental function constructor(bool) result(json_bool)
        type(json_bool_t) :: json_bool
        logical, intent(in) :: bool

        json_bool%bool = bool
    end function

    subroutine create_json_bool(json, bool)
        class(json_value_t), allocatable, intent(out) :: json
        logical, intent(in) :: bool

        type(json_bool_t), allocatable :: local

        allocate(local)
        local%bool = bool
        call move_alloc(local, json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_bool_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_bool_t)
            equals = lhs%bool .eqv. rhs%bool
        class default
            equals = .false.
        end select
    end function

    subroutine write_to_compactly(self, sink)
        class(json_bool_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        if (self%bool) then
            call sink%append("true")
        else
            call sink%append("false")
        end if
    end subroutine
end module
