module rojff_constants_m
    implicit none
    private
    public :: INDENTATION, NEWLINE

    integer, parameter :: INDENTATION = 4
    character(len=*), parameter :: NEWLINE = new_line("a")
end module
