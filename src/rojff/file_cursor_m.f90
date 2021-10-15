module rojff_file_cursor_m
    use iso_fortran_env, only: iostat_end, iostat_eor
    use rojff_cursor_m, only: cursor_t

    implicit none
    private
    public :: file_cursor_t

    type, extends(cursor_t) :: file_cursor_t
        private
        integer :: unit
        character(len=1) :: buffer
        logical :: reached_end = .false.
        integer :: current_line_ = 1
        integer :: current_column_ = 1
    contains
        procedure :: peek
        procedure :: next
        procedure :: finished
        procedure :: current_line
        procedure :: current_column
    end type

    interface file_cursor_t
        module procedure constructor
    end interface
contains
    function constructor(unit) result(cursor)
        integer, intent(in) :: unit !! This must be (and remain) associated with an open file
        type(file_cursor_t) :: cursor

        integer :: stat

        cursor%unit = unit
        read(cursor%unit, '(A)', advance="NO", iostat=stat) cursor%buffer
        if (stat == iostat_end) then
            cursor%reached_end = .true.
        else if (stat == iostat_eor) then
            ! read(cursor%unit, *)
            cursor%buffer = new_line('a')
        else if (stat /= 0) then
            read(cursor%unit, *)
        end if
    end function

    pure function peek(self) result(next_char)
        class(file_cursor_t), intent(in) :: self
        character(len=1) :: next_char

        if (.not.self%finished()) then
            next_char = self%buffer
        end if
    end function

    subroutine next(self)
        class(file_cursor_t), intent(inout) :: self

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE = char(10)
        character(len=1) :: current_char
        integer :: stat

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
            read(self%unit, '(A)', advance="NO", iostat=stat) self%buffer
            if (stat == iostat_end) then
                self%reached_end = .true.
            else if (stat == iostat_eor) then
                ! read(self%unit, *)
                self%buffer = new_line('a')
            else if (stat /= 0) then
                read(self%unit, *)
            end if
        end if
    end subroutine

    pure function finished(self)
        class(file_cursor_t), intent(in) :: self
        logical :: finished

        finished = self%reached_end
    end function

    pure function current_line(self)
        class(file_cursor_t), intent(in) :: self
        integer :: current_line

        current_line = self%current_line_
    end function

    pure function current_column(self)
        class(file_cursor_t), intent(in) :: self
        integer :: current_column

        current_column = self%current_column_
    end function
end module
