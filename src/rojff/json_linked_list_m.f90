module rojff_json_linked_list_m
    use rojff_json_element_m, only: json_element_t
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

    interface
        module subroutine append(self, json)
            implicit none
            class(json_linked_list_t), intent(inout) :: self
            class(json_value_t), allocatable, intent(inout) :: json
        end subroutine

        module subroutine move_into_elements(self, elements)
            implicit none
            class(json_linked_list_t), intent(inout) :: self
            type(json_element_t), allocatable, intent(out) :: elements(:)
        end subroutine
    end interface
end module
