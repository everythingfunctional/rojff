module rojff_json_member_m
    use iso_varying_string, only: varying_string, operator(//)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_member_t, json_member_unsafe, move_into_member_unsafe

    type :: json_member_t
        character(len=:), allocatable :: key
        class(json_value_t), allocatable :: value_
    contains
        procedure :: equals
        generic :: operator(==) => equals
        procedure :: to_compact_string
    end type
contains
    impure elemental function json_member_unsafe(key, value_) result(json_member)
        character(len=*), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(json_member_t) :: json_member

        json_member%key = key
        json_member%value_ = value_
    end function

    subroutine move_into_member_unsafe(member, key, value_)
        type(json_member_t), intent(out) :: member
        character(len=*), intent(in) :: key
        class(json_value_t), allocatable, intent(inout) :: value_

        member%key = key
        call move_alloc(value_, member%value_)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_member_t), intent(in) :: lhs
        type(json_member_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%key == rhs%key .and. lhs%value_ == rhs%value_
    end function

    elemental recursive function to_compact_string(self) result(string)
        class(json_member_t), intent(in) :: self
        type(varying_string) :: string

        string = '"' // self%key // '":' // self%value_%to_compact_string()
    end function
end module
