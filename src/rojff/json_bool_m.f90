module rojff_json_bool_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_bool_t, create_json_bool

    type, extends(json_value_t) :: json_bool_t
        logical :: bool
    contains
        procedure :: to_compact_string => to_string
    end type

    interface json_bool_t
        module procedure constructor
    end interface
contains
    function constructor(bool) result(json_bool)
        type(json_bool_t) :: json_bool
        logical, intent(in) :: bool

        json_bool%bool = bool
    end function

    subroutine create_json_bool(json, bool)
        class(json_value_t), allocatable, intent(out) :: json
        logical, intent(in) :: bool

        type(json_bool_t), allocatable :: local

        allocate(local)
        local%bool = bool
        call move_alloc(local, json)
    end subroutine

    elemental function to_string(self) result(string)
        class(json_bool_t), intent(in) :: self
        type(varying_string) :: string

        if (self%bool) then
            string = "true"
        else
            string = "false"
        end if
    end function
end module
