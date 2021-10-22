module rojff_json_object_m
    use rojff_constants_m, only: INDENTATION, NEWLINE
    use rojff_json_member_m, only: json_member_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t
    use strff, only: join

    implicit none
    private
    public :: json_object_t, move_into_object

    type, extends(json_value_t) :: json_object_t
        type(json_member_t), allocatable :: members(:)
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_object_t
        module procedure constructor
    end interface
contains
    function constructor(members) result(json_object)
        type(json_member_t), intent(in) :: members(:)
        type(json_object_t) :: json_object

        json_object%members = members
    end function

    subroutine move_into_object(json, members)
        class(json_value_t), allocatable, intent(out) :: json
        type(json_member_t), allocatable, intent(inout) :: members(:)

        type(json_object_t), allocatable :: local

        allocate(local)
        call move_alloc(members, local%members)
        call move_alloc(local, json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_object_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        integer :: i

        select type (rhs)
        type is (json_object_t)
            ! looping over both sides avoids inadvertent equality on the off chance
            ! that one side has a duplicate entry that the other side has only one of
            equals = &
                    size(lhs%members) == size(rhs%members) &
                    .and. all([(any(lhs%members(i) == rhs%members), i = 1, size(lhs%members))]) &
                    .and. all([(any(rhs%members(i) == lhs%members), i = 1, size(rhs%members))])
        class default
            equals = .false.
        end select
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_object_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        integer :: i

        call sink%append("{")
        do i = 1, size(self%members) - 1
            call self%members(i)%write_to_compactly(sink)
            call sink%append(",")
        end do
        if (size(self%members) > 0) then
            call self%members(size(self%members))%write_to_compactly(sink)
        end if
        call sink%append("}")
    end subroutine

    recursive subroutine write_to_expanded(self, indentation_level, sink)
        class(json_object_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        integer :: i
        integer :: my_indentation_level

        if (size(self%members) > 0) then
            call sink%append("{" // NEWLINE)
            my_indentation_level = indentation_level + 1
            do i = 1, size(self%members) - 1
                call sink%append(repeat(" ", my_indentation_level * INDENTATION))
                call self%members(i)%write_to_expanded(my_indentation_level, sink)
                call sink%append("," // NEWLINE)
            end do
            call sink%append(repeat(" ", my_indentation_level * INDENTATION))
            call self%members(i)%write_to_expanded(my_indentation_level, sink)
            call sink%append(NEWLINE)
            call sink%append(repeat(" ", indentation_level * INDENTATION) // "}")
        else
            call sink%append("{}")
        end if
    end subroutine
end module
