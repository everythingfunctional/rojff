module rojff_file_cursor_m
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
        module function constructor(unit) result(cursor)
            implicit none
            integer, intent(in) :: unit !! This must be (and remain) associated with an open file
            type(file_cursor_t) :: cursor
        end function
    end interface

    interface
        pure module function peek(self) result(next_char)
            implicit none
            class(file_cursor_t), intent(in) :: self
            character(len=1) :: next_char
        end function

        module subroutine next(self)
            implicit none
            class(file_cursor_t), intent(inout) :: self
        end subroutine

        pure module function finished(self)
            implicit none
            class(file_cursor_t), intent(in) :: self
            logical :: finished
        end function

        pure module function current_line(self)
            implicit none
            class(file_cursor_t), intent(in) :: self
            integer :: current_line
        end function

        pure module function current_column(self)
            implicit none
            class(file_cursor_t), intent(in) :: self
            integer :: current_column
        end function

        module subroutine increment_position(self, current_char)
            implicit none
            class(file_cursor_t), intent(inout) :: self
            character(len=1), intent(in) :: current_char
        end subroutine

        module subroutine read_into_buffer(self)
            implicit none
            class(file_cursor_t), intent(inout) :: self
        end subroutine
    end interface
end module
