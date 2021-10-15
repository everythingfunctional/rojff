module rojff_json_array_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_element_m, only: json_element_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_builder_m, only: string_builder_t
    use rojff_string_sink_m, only: string_sink_t
    use strff, only: join

    implicit none
    private
    public :: json_array_t, move_into_array

    type, extends(json_value_t) :: json_array_t
        type(json_element_t), allocatable :: elements(:)
    contains
        procedure :: equals
        procedure :: to_compact_string
        procedure :: write_to_compactly
    end type
contains
    function constructor(elements) result(json_array)
        type(json_element_t), intent(in) :: elements(:)
        type(json_array_t) :: json_array

        json_array%elements = elements
    end function

    subroutine move_into_array(json, elements)
        class(json_value_t), allocatable, intent(out) :: json
        type(json_element_t), allocatable, intent(inout) :: elements(:)

        type(json_array_t), allocatable :: local

        allocate(local)
        call move_alloc(elements, local%elements)
        call move_alloc(local, json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_array_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_array_t)
            equals = size(lhs%elements) == size(rhs%elements) .and. all(lhs%elements == rhs%elements)
        class default
            equals = .false.
        end select
    end function

    recursive function to_compact_string(self) result(string)
        class(json_array_t), intent(in) :: self
        type(varying_string) :: string

        type(string_builder_t) :: sink
        character(len=:), allocatable :: plain_string

        call self%write_to_compactly(sink)
        call sink%move_into(plain_string)
        string = plain_string
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_array_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        integer :: i

        call sink%append("[")
        do i = 1, size(self%elements) - 1
            call self%elements(i)%write_to_compactly(sink)
            call sink%append(",")
        end do
        if (size(self%elements) > 0) then
            call self%elements(size(self%elements))%write_to_compactly(sink)
        end if
        call sink%append("]")
    end subroutine
end module
