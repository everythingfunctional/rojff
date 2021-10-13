module rojff_json_null_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_null_t

    type, extends(json_value_t) :: json_null_t
    contains
        procedure :: to_compact_string => to_string
    end type
contains
    elemental function to_string(self) result(string)
        class(json_null_t), intent(in) :: self
        type(varying_string) :: string

        string = "null"
    end function
end module
