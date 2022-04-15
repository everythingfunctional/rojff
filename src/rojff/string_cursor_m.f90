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
        module procedure constructor
    end interface
contains
    pure function constructor(string) result(cursor)
        character(len=*), intent(in) :: string
        type(string_cursor_t) :: cursor

        cursor%whole_string = string
    end function

    pure function peek(self) result(next_char)
        class(string_cursor_t), intent(in) :: self
        character(len=1) :: next_char

        if (.not.self%finished()) then
            next_char = self%whole_string(self%current_character:self%current_character)
        else
            next_char = " "
        end if
    end function

    subroutine next(self)
        class(string_cursor_t), intent(inout) :: self

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE = char(10)
        character(len=1) :: current_char

        if (.not. self%finished()) then
            current_char = self%peek()

            if (current_char == NEWLINE) then
                self%current_line_ = self%current_line_ + 1
                self%current_column_ = 1
            else if (current_char == TAB) then
                self%current_column_ = self%current_column_ + 8 - mod(self%current_column_ - 1, 8)
            else
                self%current_column_ = self%current_column_ + 1
            end if
            self%current_character = self%current_character + 1
        end if
    end subroutine

    pure function finished(self)
        class(string_cursor_t), intent(in) :: self
        logical :: finished

        finished = self%current_character > len(self%whole_string)
    end function

    pure function current_line(self)
        class(string_cursor_t), intent(in) :: self
        integer :: current_line

        current_line = self%current_line_
    end function

    pure function current_column(self)
        class(string_cursor_t), intent(in) :: self
        integer :: current_column

        current_column = self%current_column_
    end function
end module
