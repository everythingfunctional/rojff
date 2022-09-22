module rojff_file_sink_m
    use rojff_sink_m, only: sink_t

    implicit none
    private
    public :: file_sink_t

    type, extends(sink_t) :: file_sink_t
        private
        integer :: unit
    contains
        procedure :: append
    end type

    interface file_sink_t
        module function constructor(unit) result(file_sink)
            implicit none
            integer, intent(in) :: unit !! This must be (and remain) associated with an open file
            type(file_sink_t) :: file_sink
        end function
    end interface

    interface
        module subroutine append(self, part)
            implicit none
            class(file_sink_t), intent(inout) :: self
            character(len=*), intent(in) :: part
        end subroutine
    end interface
end module
