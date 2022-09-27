module rojff_sink_m
    implicit none
    private
    public :: sink_t

    type, abstract :: sink_t
    contains
        procedure(append_i), deferred :: append
    end type

    abstract interface
        subroutine append_i(self, part)
            import :: sink_t

            implicit none

            class(sink_t), intent(inout) :: self
            character(len=*), intent(in) :: part
        end subroutine
    end interface
end module
