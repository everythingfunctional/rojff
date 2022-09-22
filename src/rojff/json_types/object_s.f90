submodule(rojff_json_object_m) rojff_json_object_s
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, NOT_FOUND
    use iso_varying_string, only: assignment(=), char
    use rojff_constants_m, only: INDENTATION, NEWLINE

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_json_object_m"
contains
    module procedure deleted
        associate(unused => members); end associate
        error stop "Do not use the intrinsic structure constructor!"
    end procedure

    module procedure constructor
        allocate(json_object%members, source = members)
    end procedure

    module procedure move_into_object
        type(json_object_t), allocatable :: local

        allocate(local)
        call move_alloc(members, local%members)
        call move_alloc(local, json)
    end procedure

    module procedure move_into_object_unsafe
        allocate(object)
        call move_alloc(members, object%members)
    end procedure

    module procedure equals
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
    end procedure

    module procedure get_c
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
    end procedure

    module procedure get_s
        element = fallible_json_value_t( &
                self%get(char(key)), &
                module_t(MODULE_NAME), &
                procedure_t("get_s"))
    end procedure

    module procedure keys
        integer :: i

        if (allocated(self%members)) then
            allocate(keys(size(self%members)))
            do i = 1, size(self%members)
                keys(i) = self%members(i)%key
            end do
        else
            allocate(keys(0))
        end if
    end procedure

    module procedure values
        integer :: i

        if (allocated(self%members)) then
            values = [(json_element_t(self%members(i)%value_), i = 1, size(self%members))]
        else
            allocate(values(0))
        end if
    end procedure

    module procedure write_to_compactly
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
    end procedure

    module procedure write_to_expanded
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
    end procedure
end submodule