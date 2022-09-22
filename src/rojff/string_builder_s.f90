submodule(rojff_string_builder_m) rojff_string_builder_s
    implicit none
contains
    module procedure append
        if (associated(self%head)) then
            allocate(self%tail%next)
            self%tail => self%tail%next
            self%tail%part = part
            self%total_length = self%total_length + len(part)
        else
            allocate(self%head)
            self%tail => self%head
            self%head%part = part
            self%total_length = len(part)
        end if
    end procedure

    module procedure move_into
        type(string_node_t), pointer :: curr
        integer :: current_position

        allocate(character(len=self%total_length) :: string)
        current_position = 1
        curr => self%head
        do while (associated(curr))
            string(current_position : current_position + len(curr%part) - 1) = curr%part
            current_position = current_position + len(curr%part)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end procedure
end submodule