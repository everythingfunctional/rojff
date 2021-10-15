module rojff_json_value_m
    use rojff_string_builder_m, only: string_builder_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_value_t

    type, abstract :: json_value_t
    contains
        procedure(equals_i), deferred :: equals
        generic :: operator(==) => equals
        procedure :: to_compact_string
        procedure(write_to_compactly_i), deferred :: write_to_compactly
    end type

    abstract interface
        elemental function equals_i(lhs, rhs) result(equals)
            import :: json_value_t

            implicit none

            class(json_value_t), intent(in) :: lhs, rhs
            logical :: equals
        end function

        subroutine write_to_compactly_i(self, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
contains
    function to_compact_string(self) result(string)
        class(json_value_t), intent(in) :: self
        character(len=:), allocatable :: string

        type(string_builder_t) :: sink

        call self%write_to_compactly(sink)
        call sink%move_into(string)
    end function
end module
