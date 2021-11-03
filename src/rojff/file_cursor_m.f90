module rojff_file_cursor_m
    use iso_fortran_env, only: iostat_end, iostat_eor
    use rojff_cursor_m, only: cursor_t

    implicit none
    private
    public :: file_cursor_t

    integer, parameter :: buffer_length = 1024

    type, extends(cursor_t) :: file_cursor_t
        private
        integer :: unit
        character(len=buffer_length) :: buffer
        integer :: actual_buffer_length = buffer_length
        integer :: buffer_position = 1
        logical :: reached_end = .false.
        integer :: current_line_ = 1
        integer :: current_column_ = 1
    contains
        procedure :: peek
        procedure :: next
        procedure :: finished
        procedure :: current_line
        procedure :: current_column
        procedure, private :: increment_position
        procedure, private :: read_into_buffer
    end type

    interface file_cursor_t
        module procedure constructor
    end interface
contains
    function constructor(unit) result(cursor)
        integer, intent(in) :: unit !! This must be (and remain) associated with an open file
        type(file_cursor_t) :: cursor

        cursor%unit = unit
        call cursor%read_into_buffer()
    end function

    pure function peek(self) result(next_char)
        class(file_cursor_t), intent(in) :: self
        character(len=1) :: next_char

        if (.not.self%finished()) then
            next_char = self%buffer(self%buffer_position:self%buffer_position)
        end if
    end function

    subroutine next(self)
        class(file_cursor_t), intent(inout) :: self

        if (self%buffer_position < self%actual_buffer_length) then
            call self%increment_position(self%peek())
            self%buffer_position = self%buffer_position + 1
        else
            call self%increment_position(self%peek())
            if (self%reached_end) then
                self%buffer_position = self%buffer_position + 1
            else
                call self%read_into_buffer()
                self%buffer_position = 1
            end if
        end if
    end subroutine

    pure function finished(self)
        class(file_cursor_t), intent(in) :: self
        logical :: finished

        finished = self%reached_end .and. self%buffer_position > self%actual_buffer_length
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

    subroutine increment_position(self, current_char)
        class(file_cursor_t), intent(inout) :: self
        character(len=1), intent(in) :: current_char

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE = char(10)

        if (current_char == NEWLINE) then
            self%current_line_ = self%current_line_ + 1
            self%current_column_ = 1
        else if (current_char == TAB) then
            self%current_column_ = self%current_column_ + 8 - mod(self%current_column_ - 1, 8)
        else
            self%current_column_ = self%current_column_ + 1
        end if
    end subroutine

    subroutine read_into_buffer(self)
        class(file_cursor_t), intent(inout) :: self

        integer :: stat

        read(self%unit, fmt='(A)', advance="NO", iostat=stat, size=self%actual_buffer_length) self%buffer(1:buffer_length-1)
        if (stat == iostat_end) then
            self%reached_end = .true.
        else if (stat == iostat_eor) then
            self%buffer = self%buffer(1:self%actual_buffer_length) // new_line('a')
            self%actual_buffer_length = self%actual_buffer_length + 1
        else if (stat /= 0) then
            read(self%unit, *)
        end if
    end subroutine
end module
