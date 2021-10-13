module rojff_json_null_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_null_t, create_json_null

    type, extends(json_value_t) :: json_null_t
    contains
        procedure :: to_compact_string => to_string
    end type

    interface json_null_t
        module procedure constructor
    end interface
contains
    function constructor() result(json_null)
        type(json_null_t) :: json_null
    end function

    subroutine create_json_null(json)
        class(json_value_t), allocatable, intent(out) :: json

        allocate(json_null_t :: json)
    end subroutine

    elemental function to_string(self) result(string)
        class(json_null_t), intent(in) :: self
        type(varying_string) :: string

        string = "null"
    end function
end module
