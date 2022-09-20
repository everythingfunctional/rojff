module rojff_json_member_m
    use iso_varying_string, only: varying_string, char
    use rojff_json_element_m, only: json_element_t
    use rojff_json_string_m, only: json_string_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: &
            json_member_t, &
            json_member_unsafe, &
            move_into_member, &
            move_into_member_unsafe

    type :: json_member_t
        character(len=:), allocatable :: key
        class(json_value_t), allocatable :: value_
    contains
        procedure :: equals
        generic :: operator(==) => equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_member_t
        module procedure construct_v
        module procedure construct_e
    end interface

    interface json_member_unsafe
        module procedure json_member_unsafe_cv
        module procedure json_member_unsafe_ce
        module procedure json_member_unsafe_sv
        module procedure json_member_unsafe_se
    end interface
contains
    impure elemental function json_member_unsafe_cv(key, value_) result(json_member)
        character(len=*), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(json_member_t) :: json_member

        json_member%key = key
        json_member%value_ = value_
    end function

    impure elemental function json_member_unsafe_ce(key, element) result(json_member)
        character(len=*), intent(in) :: key
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member%key = key
        json_member%value_ = element%json
    end function

    impure elemental function json_member_unsafe_sv(key, value_) result(json_member)
        type(varying_string), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(json_member_t) :: json_member

        json_member%key = char(key)
        json_member%value_ = value_
    end function

    impure elemental function json_member_unsafe_se(key, element) result(json_member)
        type(varying_string), intent(in) :: key
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member%key = char(key)
        json_member%value_ = element%json
    end function

    impure elemental function construct_v(key, value_) result(json_member)
        type(json_string_t), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(json_member_t) :: json_member

        json_member%key = key%string
        json_member%value_ = value_
    end function

    impure elemental function construct_e(key, element) result(json_member)
        type(json_string_t), intent(in) :: key
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member%key = key%string
        json_member%value_ = element%json
    end function

    subroutine move_into_member(member, key, value_)
        type(json_member_t), intent(out) :: member
        type(json_string_t), intent(in) :: key
        class(json_value_t), allocatable, intent(inout) :: value_

        call move_alloc(key%string, member%key)
        call move_alloc(value_, member%value_)
    end subroutine

    subroutine move_into_member_unsafe(member, key, value_)
        type(json_member_t), intent(out) :: member
        character(len=*), intent(in) :: key
        class(json_value_t), allocatable, intent(inout) :: value_

        member%key = key
        call move_alloc(value_, member%value_)
    end subroutine

    recursive elemental function equals(lhs, rhs)
        class(json_member_t), intent(in) :: lhs
        type(json_member_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%key == rhs%key .and. lhs%value_ == rhs%value_
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_member_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        call sink%append('"')
        call sink%append(self%key)
        call sink%append('":')
        call self%value_%write_to_compactly(sink)
    end subroutine

    recursive subroutine write_to_expanded(self, indentation_level, sink)
        class(json_member_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        call sink%append('"' // self%key // '" : ')
        call self%value_%write_to_expanded(indentation_level, sink)
    end subroutine
end module
