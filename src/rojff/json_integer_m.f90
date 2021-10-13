module rojff_json_integer_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t
    use strff, only: to_string

    implicit none
    private
    public :: json_integer_t, create_json_integer

    type, extends(json_value_t) :: json_integer_t
        integer :: number
    contains
        procedure :: to_compact_string => integer_to_string
    end type

    interface json_integer_t
        module procedure constructor
    end interface
contains
    elemental function constructor(number) result(json_integer)
        type(json_integer_t) :: json_integer
        integer, intent(in) :: number

        json_integer%number = number
    end function

    subroutine create_json_integer(json, number)
        class(json_value_t), allocatable, intent(out) :: json
        integer, intent(in) :: number

        type(json_integer_t), allocatable :: local

        allocate(local)
        local%number = number
        call move_alloc(local, json)
    end subroutine

    elemental function integer_to_string(self) result(string)
        class(json_integer_t), intent(in) :: self
        type(varying_string) :: string

        string = to_string(self%number)
    end function
end module
