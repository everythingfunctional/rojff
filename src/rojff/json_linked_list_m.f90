module rojff_json_linked_list_m
    use rojff_json_element_m, only: json_element_t, move_into_element
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_linked_list_t

    type :: json_node_t
        class(json_value_t), allocatable :: json
        type(json_node_t), pointer :: next => null()
    end type

    type :: json_linked_list_t
        private
        type(json_node_t), pointer :: head => null()
        type(json_node_t), pointer :: tail => null()
        integer :: size = 0
    contains
        procedure :: append
        procedure :: move_into_elements
    end type
contains
    subroutine append(self, json)
        class(json_linked_list_t), intent(inout) :: self
        class(json_value_t), allocatable, intent(inout) :: json

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
    end subroutine

    subroutine move_into_elements(self, elements)
        class(json_linked_list_t), intent(inout) :: self
        type(json_element_t), allocatable, intent(out) :: elements(:)

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
    end subroutine
end module
