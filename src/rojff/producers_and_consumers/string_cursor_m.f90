module rojff_string_cursor_m
    use rojff_cursor_m, only: cursor_t

    implicit none
    private
    public :: string_cursor_t

    type, extends(cursor_t) :: string_cursor_t
        private
        character(len=:), allocatable :: whole_string
        integer :: current_character = 1
        integer :: current_line_ = 1
        integer :: current_column_ = 1
    contains
        procedure :: peek
        procedure :: next
        procedure :: finished
        procedure :: current_line
        procedure :: current_column
    end type

    interface string_cursor_t
        pure module function constructor(string) result(cursor)
            implicit none
            character(len=*), intent(in) :: string
            type(string_cursor_t) :: cursor
        end function
    end interface

    interface
        pure module function peek(self) result(next_char)
            implicit none
            class(string_cursor_t), intent(in) :: self
            character(len=1) :: next_char
        end function

        module subroutine next(self)
            implicit none
            class(string_cursor_t), intent(inout) :: self
        end subroutine

        pure module function finished(self)
            implicit none
            class(string_cursor_t), intent(in) :: self
            logical :: finished
        end function

        pure module function current_line(self)
            implicit none
            class(string_cursor_t), intent(in) :: self
            integer :: current_line
        end function

        pure module function current_column(self)
            implicit none
            class(string_cursor_t), intent(in) :: self
            integer :: current_column
        end function
    end interface
end module
