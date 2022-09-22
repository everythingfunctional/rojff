module rojff_cursor_m
    implicit none
    private
    public :: cursor_t

    type, abstract :: cursor_t
    contains
        procedure(peek_i), deferred :: peek
        procedure(next_i), deferred :: next
        procedure(finished_i), deferred :: finished
        procedure(position_i), deferred :: current_line
        procedure(position_i), deferred :: current_column
    end type

    abstract interface
        pure function peek_i(self) result(next_char)
            import :: cursor_t

            implicit none

            class(cursor_t), intent(in) :: self
            character(len=1) :: next_char
        end function

        subroutine next_i(self)
            import :: cursor_t

            implicit none

            class(cursor_t), intent(inout) :: self
        end subroutine

        pure function finished_i(self) result(finished)
            import :: cursor_t

            implicit none

            class(cursor_t), intent(in) :: self
            logical :: finished
        end function

        pure function position_i(self) result(position)
            import :: cursor_t

            implicit none

            class(cursor_t), intent(in) :: self
            integer :: position
        end function
    end interface
end module
