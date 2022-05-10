module rojff_json_array_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, OUT_OF_BOUNDS
    use rojff_constants_m, only: INDENTATION, NEWLINE
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_element_m, only: json_element_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t
    use rojff_utils_m, only: to_string

    implicit none
    private
    public :: json_array_t, move_into_array

    type, extends(json_value_t) :: json_array_t
        type(json_element_t), allocatable :: elements(:)
    contains
        procedure :: equals
        procedure :: get
        procedure :: write_to_compactly
        procedure :: write_to_expanded
    end type

    interface json_array_t
        module procedure constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "rojff_json_array_m"
contains
    function constructor(elements) result(json_array)
        type(json_element_t), intent(in) :: elements(:)
        type(json_array_t) :: json_array

        allocate(json_array%elements, source = elements)
    end function

    subroutine move_into_array(json, elements)
        class(json_value_t), allocatable, intent(out) :: json
        type(json_element_t), allocatable, intent(inout) :: elements(:)

        type(json_array_t), allocatable :: local

        allocate(local)
        call move_alloc(elements, local%elements)
        call move_alloc(local, json)
    end subroutine

    recursive elemental function equals(lhs, rhs)
        class(json_array_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_array_t)
            if (allocated(lhs%elements)) then
                if (allocated(rhs%elements)) then
                    equals = &
                            size(lhs%elements) == size(rhs%elements) &
                            .and. all(lhs%elements == rhs%elements)
                else
                    equals = size(lhs%elements) == 0
                end if
            else
                if (allocated(rhs%elements)) then
                    equals = size(rhs%elements) == 0
                else
                    equals = .true.
                end if
            end if
        class default
            equals = .false.
        end select
    end function

    impure elemental function get(self, position) result(element)
        class(json_array_t), intent(in) :: self
        integer, intent(in) :: position
        type(fallible_json_value_t) :: element

        if (allocated(self%elements)) then
            if (position <= 0 .or. position > size(self%elements)) then
                element = fallible_json_value_t(error_list_t(fatal_t( &
                        OUT_OF_BOUNDS, &
                        module_t(MODULE_NAME), &
                        procedure_t("get"), &
                        "Attempted to access element " // to_string(position) &
                            // " from an array with only " // to_string(size(self%elements)) &
                            // " elements")))
            else
                element = fallible_json_value_t(self%elements(position)%json)
            end if
        else
            element = fallible_json_value_t(error_list_t(fatal_t( &
                    OUT_OF_BOUNDS, &
                    module_t(MODULE_NAME), &
                    procedure_t("get"), &
                    "Attempted to access element " // to_string(position) &
                        // " from an unallocated array.")))
        end if
    end function

    recursive subroutine write_to_compactly(self, sink)
        class(json_array_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        integer :: i

        call sink%append("[")
        if (allocated(self%elements)) then
            do i = 1, size(self%elements) - 1
                call self%elements(i)%write_to_compactly(sink)
                call sink%append(",")
            end do
            if (size(self%elements) > 0) then
                call self%elements(size(self%elements))%write_to_compactly(sink)
            end if
        end if
        call sink%append("]")
    end subroutine

    recursive subroutine write_to_expanded(self, indentation_level, sink)
        class(json_array_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        integer :: i
        integer :: my_indentation_level

        if (allocated(self%elements)) then
            if (size(self%elements) > 0) then
                call sink%append("[" // NEWLINE)
                my_indentation_level = indentation_level + 1
                do i = 1, size(self%elements) - 1
                    call sink%append(repeat(" ", my_indentation_level * INDENTATION))
                    call self%elements(i)%write_to_expanded(my_indentation_level, sink)
                    call sink%append("," // NEWLINE)
                end do
                call sink%append(repeat(" ", my_indentation_level * INDENTATION))
                call self%elements(i)%write_to_expanded(my_indentation_level, sink)
                call sink%append(NEWLINE)
                call sink%append(repeat(" ", indentation_level * INDENTATION) // "]")
            else
                call sink%append("[]")
            end if
        else
            call sink%append("[]")
        end if
    end subroutine
end module
