submodule(rojff_json_linked_list_m) rojff_json_linked_list_s
    use rojff_json_element_m, only: move_into_element

    implicit none
contains
    module procedure append
        if (self%size == 0) then
            allocate(self%head)
            self%tail => self%head
            call move_alloc(json, self%head%json)
            self%size = 1
        else
            allocate(self%tail%next)
            self%tail => self%tail%next
            call move_alloc(json, self%tail%json)
            self%size = self%size + 1
        end if
    end procedure

    module procedure move_into_elements
        type(json_node_t), pointer :: curr
        integer :: i

        allocate(elements(self%size))
        curr => self%head
        do i = 1, self%size
            call move_into_element(elements(i), curr%json)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end procedure
end submodule