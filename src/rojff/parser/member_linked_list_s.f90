submodule(rojff_member_linked_list_m) rojff_member_linked_list_s
    use rojff_json_member_m, only: move_into_member_unsafe

    implicit none
contains
    module procedure append
        if (self%size == 0) then
            allocate(self%head)
            self%tail => self%head
            self%tail%key = key
            call move_alloc(value_, self%head%value_)
            self%size = 1
        else
            allocate(self%tail%next)
            self%tail => self%tail%next
            self%tail%key = key
            call move_alloc(value_, self%tail%value_)
            self%size = self%size + 1
        end if
    end procedure

    module procedure move_into_members
        type(member_node_t), pointer :: curr
        integer :: i

        allocate(members(self%size))
        curr => self%head
        do i = 1, self%size
            call move_into_member_unsafe(members(i), curr%key, curr%value_)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end procedure
end submodule