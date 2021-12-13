module rojff_json_object_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, NOT_FOUND
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), operator(==), len
    use rojff_constants_m, only: INDENTATION, NEWLINE
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_element_m, only: json_element_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_object_t, move_into_object

    type, extends(json_value_t) :: json_object_t
        type(json_member_t), allocatable :: members(:)
    contains
        procedure :: equals
        procedure, private :: get_c, get_s
        generic :: get => get_c, get_s
        procedure :: keys
        procedure :: values
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_object_t
        module procedure constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "rojff_json_object_m"
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

    recursive elemental function equals(lhs, rhs)
        class(json_object_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        integer :: i

        select type (rhs)
        type is (json_object_t)
            if (allocated(lhs%members)) then
                if (allocated(rhs%members)) then
                    ! looping over both sides avoids inadvertent equality on the off chance
                    ! that one side has a duplicate entry that the other side has only one of
                    equals = &
                            size(lhs%members) == size(rhs%members) &
                            .and. all([(any(lhs%members(i) == rhs%members), i = 1, size(lhs%members))]) &
                            .and. all([(any(rhs%members(i) == lhs%members), i = 1, size(rhs%members))])
                else
                    equals = size(lhs%members) == 0
                end if
            else
                if (allocated(rhs%members)) then
                    equals = size(rhs%members) == 0
                else
                    equals = .true.
                end if
            end if
        class default
            equals = .false.
        end select
    end function

    function get_c(self, key) result(element)
        class(json_object_t), intent(in) :: self
        character(len=*), intent(in) :: key
        type(fallible_json_value_t) :: element

        integer :: i

        if (allocated(self%members)) then
            do i = 1, size(self%members)
                if (key == self%members(i)%key .and. len(key) == len(self%members(i)%key)) then
                    element = fallible_json_value_t(self%members(i)%value_)
                    return
                end if
            end do
        end if
        element = fallible_json_value_t(error_list_t(fatal_t( &
                NOT_FOUND, &
                module_t(MODULE_NAME), &
                procedure_t("get_c"), &
                '"' // key // '" not found in ' // self%to_compact_string())))
    end function

    impure elemental function get_s(self, key) result(element)
        class(json_object_t), intent(in) :: self
        type(varying_string), intent(in) :: key
        type(fallible_json_value_t) :: element

        integer :: i

        if (allocated(self%members)) then
            do i = 1, size(self%members)
                if (key == self%members(i)%key .and. len(key) == len(self%members(i)%key)) then
                    element = fallible_json_value_t(self%members(i)%value_)
                    return
                end if
            end do
        end if
        element = fallible_json_value_t(error_list_t(fatal_t( &
                NOT_FOUND, &
                module_t(MODULE_NAME), &
                procedure_t("get_s"), &
                '"' // key // '" not found in ' // self%to_compact_string())))
    end function

    function keys(self)
        class(json_object_t), intent(in) :: self
        type(varying_string), allocatable :: keys(:)

        integer :: i

        if (allocated(self%members)) then
            allocate(keys(size(self%members)))
            do i = 1, size(self%members)
                keys(i) = self%members(i)%key
            end do
        else
            allocate(keys(0))
        end if
    end function

    function values(self)
        class(json_object_t), intent(in) :: self
        type(json_element_t), allocatable :: values(:)

        integer :: i

        if (allocated(self%members)) then
            values = [(json_element_t(self%members(i)%value_), i = 1, size(self%members))]
        else
            allocate(values(0))
        end if
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_object_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        integer :: i

        call sink%append("{")
        if (allocated(self%members)) then
            do i = 1, size(self%members) - 1
                call self%members(i)%write_to_compactly(sink)
                call sink%append(",")
            end do
            if (size(self%members) > 0) then
                call self%members(size(self%members))%write_to_compactly(sink)
            end if
        end if
        call sink%append("}")
    end subroutine

    recursive subroutine write_to_expanded(self, indentation_level, sink)
        class(json_object_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        integer :: i
        integer :: my_indentation_level

        if (allocated(self%members)) then
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
        else
            call sink%append("{}")
        end if
    end subroutine
end module
