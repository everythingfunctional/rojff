submodule(rojff_file_cursor_m) rojff_file_cursor_s
    implicit none
contains
    module procedure constructor
        cursor%unit = unit
        call cursor%read_into_buffer()
    end procedure

    module procedure peek
        if (.not.self%finished()) then
            next_char = self%buffer(self%buffer_position:self%buffer_position)
        else
            next_char = " "
        end if
    end procedure

    module procedure next
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
    end procedure

    module procedure finished
        finished = self%reached_end .and. self%buffer_position > self%actual_buffer_length
    end procedure

    module procedure current_line
        current_line = self%current_line_
    end procedure

    module procedure current_column
        current_column = self%current_column_
    end procedure

    module procedure increment_position
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
    end procedure

    module procedure read_into_buffer
        integer :: stat
        character(len=100) :: msg

        read( &
                self%unit, &
                fmt='(A)', &
                advance="NO", &
                iostat=stat, &
                iomsg=msg, &
                size=self%actual_buffer_length) &
                        self%buffer(1:buffer_length-1)
        if (is_iostat_end(stat)) then
            self%reached_end = .true.
        else if (is_iostat_eor(stat)) then
            self%buffer = self%buffer(1:self%actual_buffer_length) // new_line('a')
            self%actual_buffer_length = self%actual_buffer_length + 1
        else if (stat /= 0) then
            error stop trim(msg)
        end if
    end procedure
end submodule