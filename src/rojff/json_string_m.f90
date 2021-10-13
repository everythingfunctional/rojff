module rojff_json_string_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_string_t, create_json_string_unsafe, json_string_unsafe

    type, extends(json_value_t) :: json_string_t
        character(len=:), allocatable :: string
    contains
        procedure :: to_compact_string => to_string
    end type
contains
    function json_string_unsafe(string) result(json_string)
        character(len=*), intent(in) :: string
        type(json_string_t) :: json_string

        json_string%string = string
    end function

    subroutine create_json_string_unsafe(json, string)
        class(json_value_t), allocatable, intent(out) :: json
        character(len=*), intent(in) :: string

        type(json_string_t), allocatable :: local

        allocate(local)
        local%string = string
        call move_alloc(local, json)
    end subroutine

    elemental function to_string(self) result(string)
        class(json_string_t), intent(in) :: self
        type(varying_string) :: string

        string = '"' // self%string // '"'
    end function
end module
