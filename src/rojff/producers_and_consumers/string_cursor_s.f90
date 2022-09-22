submodule(rojff_string_cursor_m) rojff_string_cursor_s
    implicit none
contains
    module procedure constructor
        cursor%whole_string = string
    end procedure

    module procedure peek
        if (.not.self%finished()) then
            next_char = self%whole_string(self%current_character:self%current_character)
        else
            next_char = " "
        end if
    end procedure

    module procedure next
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
    end procedure

    module procedure finished
        finished = self%current_character > len(self%whole_string)
    end procedure

    module procedure current_line
        current_line = self%current_line_
    end procedure

    module procedure current_column
        current_column = self%current_column_
    end procedure
end submodule