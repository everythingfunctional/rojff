module rojff_json_member_m
    use iso_varying_string, only: varying_string
    use rojff_json_element_m, only: json_element_t
    use rojff_json_string_m, only: json_string_t
    use rojff_json_value_m, only: json_value_t
    use rojff_sink_m, only: sink_t

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
        module function deleted(key, value_)
            implicit none
            character(len=*), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(json_member_t) :: deleted
        end function

        impure elemental module function construct_v( &
                key, value_) result(json_member)
            implicit none
            type(json_string_t), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(json_member_t) :: json_member
        end function

        impure elemental module function construct_e( &
                key, element) result(json_member)
            implicit none
            type(json_string_t), intent(in) :: key
            type(json_element_t), intent(in) :: element
            type(json_member_t) :: json_member
        end function
    end interface

    interface json_member_unsafe
        impure elemental module function json_member_unsafe_cv( &
                key, value_) result(json_member)
            implicit none
            character(len=*), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(json_member_t) :: json_member
        end function

        impure elemental module function json_member_unsafe_ce( &
                key, element) result(json_member)
            implicit none
            character(len=*), intent(in) :: key
            type(json_element_t), intent(in) :: element
            type(json_member_t) :: json_member
        end function

        impure elemental module function json_member_unsafe_sv( &
                key, value_) result(json_member)
            implicit none
            type(varying_string), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(json_member_t) :: json_member
        end function

        impure elemental module function json_member_unsafe_se( &
                key, element) result(json_member)
            implicit none
            type(varying_string), intent(in) :: key
            type(json_element_t), intent(in) :: element
            type(json_member_t) :: json_member
        end function
    end interface

    interface
        module subroutine move_into_member(member, key, value_)
            implicit none
            type(json_member_t), intent(out) :: member
            type(json_string_t), intent(inout) :: key
            class(json_value_t), allocatable, intent(inout) :: value_
        end subroutine

        module subroutine move_into_member_unsafe(member, key, value_)
            implicit none
            type(json_member_t), intent(out) :: member
            character(len=*), intent(in) :: key
            class(json_value_t), allocatable, intent(inout) :: value_
        end subroutine

        recursive elemental module function equals(lhs, rhs)
            implicit none
            class(json_member_t), intent(in) :: lhs
            type(json_member_t), intent(in) :: rhs
            logical :: equals
        end function

        recursive module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_member_t), intent(in) :: self
            class(sink_t), intent(inout) :: sink
        end subroutine

        recursive module subroutine write_to_expanded( &
                self, indentation_level, sink)
            implicit none
            class(json_member_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
