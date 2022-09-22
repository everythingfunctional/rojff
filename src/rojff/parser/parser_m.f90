module rojff_parser_m
    use erloff, only: error_list_t
    use rojff_cursor_m, only: cursor_t
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_string_m, only: json_string_t

    implicit none
    private
    public :: parse_json_from_file, parse_json_from_string, parse_json_string

    interface
        module function parse_json_from_file(filename) result(fallible_json)
            implicit none
            character(len=*), intent(in) :: filename
            type(fallible_json_value_t) :: fallible_json
        end function

        module function parse_json_from_string(string) result(fallible_json)
            implicit none
            character(len=*), intent(in) :: string
            type(fallible_json_value_t) :: fallible_json
        end function

        module subroutine parse_json_string(cursor, json, errors)
            implicit none
            class(cursor_t), intent(inout) :: cursor
            type(json_string_t), allocatable, intent(out) :: json
            type(error_list_t), intent(out) :: errors
        end subroutine
    end interface
end module
