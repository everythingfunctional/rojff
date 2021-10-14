module rojff_json_bool_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_bool_t, create_json_bool

    type, extends(json_value_t) :: json_bool_t
        logical :: bool
    contains
        procedure :: equals
        procedure :: to_compact_string => to_string
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

    elemental function to_string(self) result(string)
        class(json_bool_t), intent(in) :: self
        type(varying_string) :: string

        if (self%bool) then
            string = "true"
        else
            string = "false"
        end if
    end function
end module
