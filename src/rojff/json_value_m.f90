module rojff_json_value_m
    use iso_varying_string, only: varying_string
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_value_t

    type, abstract :: json_value_t
    contains
        procedure(equals_i), deferred :: equals
        generic :: operator(==) => equals
        procedure(to_string_i), deferred :: to_compact_string
        procedure(write_to_compactly_i), deferred :: write_to_compactly
    end type

    abstract interface
        elemental function equals_i(lhs, rhs) result(equals)
            import :: json_value_t

            implicit none

            class(json_value_t), intent(in) :: lhs, rhs
            logical :: equals
        end function

        function to_string_i(self) result(string)
            import :: json_value_t, varying_string

            implicit none

            class(json_value_t), intent(in) :: self
            type(varying_string) :: string
        end function

        subroutine write_to_compactly_i(self, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
end module
