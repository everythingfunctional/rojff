module rojff_constants_m
    use erloff, only: message_type_t

    implicit none
    private
    public :: INDENTATION, INVALID_INPUT, NEWLINE

    integer, parameter :: INDENTATION = 4
    character(len=*), parameter :: NEWLINE = new_line("a")
    type(message_type_t), parameter :: INVALID_INPUT = message_type_t( &
            "Invalid Input")
end module
