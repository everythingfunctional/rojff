module rojff_json_object_m
    use iso_varying_string, only: varying_string, operator(//)
    use rojff_json_member_m, only: json_member_t
    use rojff_json_value_m, only: json_value_t
    use strff, only: join

    implicit none
    private
    public :: json_object_t, move_into_object

    type, extends(json_value_t) :: json_object_t
        type(json_member_t), allocatable :: members(:)
    contains
        procedure :: equals
        procedure :: to_compact_string
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

    elemental recursive function to_compact_string(self) result(string)
        class(json_object_t), intent(in) :: self
        type(varying_string) :: string

        string = "{" // join(self%members%to_compact_string(), ",") // "}"
    end function
end module
