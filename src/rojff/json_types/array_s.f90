submodule(rojff_json_array_m) rojff_json_array_s
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, OUT_OF_BOUNDS
    use rojff_constants_m, only: INDENTATION, NEWLINE
    use rojff_utils_m, only: to_string

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_json_array_m"
contains
    module procedure constructor
        allocate(json_array%elements, source = elements)
    end procedure

    module procedure move_into_array
        type(json_array_t), allocatable :: local

        allocate(local)
        call move_alloc(elements, local%elements)
        call move_alloc(local, json)
    end procedure

    module procedure equals
        select type (rhs)
        type is (json_array_t)
            if (allocated(lhs%elements)) then
                if (allocated(rhs%elements)) then
                    if (size(lhs%elements) == size(rhs%elements)) then
                        equals = all(lhs%elements == rhs%elements)
                    else
                        equals = .false.
                    end if
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
    end procedure

    module procedure get
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
    end procedure

    module procedure write_to_compactly
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
    end procedure

    module procedure write_to_expanded
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
    end procedure
end submodule