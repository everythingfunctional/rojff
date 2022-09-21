module rojff_fallible_json_object_m
    use erloff, only: error_list_t, module_t, procedure_t
    use rojff_fallible_json_member_m, only: fallible_json_member_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_object_m, only: json_object_t

    implicit none
    private
    public :: fallible_json_object_t, move_into_fallible_object

    type :: fallible_json_object_t
        type(json_object_t), allocatable :: object
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_object_t
        module function from_members(members) result(fallible_object)
            implicit none
            type(json_member_t), intent(in) :: members(:)
            type(fallible_json_object_t) :: fallible_object
        end function

        module function from_fallible_members( &
                maybe_members) result(fallible_object)
            implicit none
            type(fallible_json_member_t), intent(in) :: maybe_members(:)
            type(fallible_json_object_t) :: fallible_object
        end function

        module function from_fallible_object( &
                maybe_object, module_, procedure_) result(fallible_object)
            implicit none
            type(fallible_json_object_t), intent(in) :: maybe_object
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_object_t) :: fallible_object
        end function
    end interface

    interface move_into_fallible_object
        module subroutine move_from_members_into_fallible_object( &
                fallible_object, members)
            implicit none
            type(fallible_json_object_t), intent(out) :: fallible_object
            type(json_member_t), allocatable, intent(inout) :: members(:)
        end subroutine
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_object_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module