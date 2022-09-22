module rojff_json_object_m
    use iso_varying_string, only: varying_string
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_element_m, only: json_element_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_object_t, json_object_unsafe, move_into_object, move_into_object_unsafe

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
        module function constructor(members) result(json_object)
            implicit none
            type(json_member_t), intent(in) :: members(:)
            type(json_object_t) :: json_object
        end function
    end interface

    interface json_object_unsafe
        module procedure constructor
    end interface

    interface
        module subroutine move_into_object(json, members)
            implicit none
            class(json_value_t), allocatable, intent(out) :: json
            type(json_member_t), allocatable, intent(inout) :: members(:)
        end subroutine

        module subroutine move_into_object_unsafe(object, members)
            implicit none
            type(json_object_t), allocatable, intent(out) :: object
            type(json_member_t), allocatable, intent(inout) :: members(:)
        end subroutine

        recursive elemental module function equals(lhs, rhs)
            implicit none
            class(json_object_t), intent(in) :: lhs
            class(json_value_t), intent(in) :: rhs
            logical :: equals
        end function

        module function get_c(self, key) result(element)
            implicit none
            class(json_object_t), intent(in) :: self
            character(len=*), intent(in) :: key
            type(fallible_json_value_t) :: element
        end function

        impure elemental module function get_s(self, key) result(element)
            implicit none
            class(json_object_t), intent(in) :: self
            type(varying_string), intent(in) :: key
            type(fallible_json_value_t) :: element
        end function

        pure module function keys(self)
            implicit none
            class(json_object_t), intent(in) :: self
            type(varying_string), allocatable :: keys(:)
        end function

        module function values(self)
            implicit none
            class(json_object_t), intent(in) :: self
            type(json_element_t), allocatable :: values(:)
        end function

        recursive module subroutine write_to_compactly(self, sink)
            implicit none
            class(json_object_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        recursive module subroutine write_to_expanded( &
                self, indentation_level, sink)
            implicit none
            class(json_object_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
