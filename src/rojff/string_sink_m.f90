module rojff_string_sink_m
    implicit none
    private
    public :: string_sink_t

    type, abstract :: string_sink_t
    contains
        procedure(append_i), deferred :: append
    end type

    abstract interface
        subroutine append_i(self, part)
            import :: string_sink_t

            implicit none

            class(string_sink_t), intent(inout) :: self
            character(len=*), intent(in) :: part
        end subroutine
    end interface
end module
