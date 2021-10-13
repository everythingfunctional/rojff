module rojff_json_number_m
    use iso_varying_string, only: varying_string, assignment(=)
    use rojff_json_value_m, only: json_value_t
    use strff, only: to_string

    implicit none
    private
    public :: json_number_t, create_json_number

    type, extends(json_value_t) :: json_number_t
        double precision :: number
        integer :: precision
        logical :: precision_provided
    contains
        procedure :: to_compact_string => number_to_string
    end type

    interface json_number_t
        module procedure constructor
    end interface
contains
    elemental function constructor(number, precision) result(json_number)
        type(json_number_t) :: json_number
        double precision, intent(in) :: number
        integer, optional, intent(in) :: precision

        json_number%number = number
        if (present(precision)) then
            json_number%precision = precision
            json_number%precision_provided = .true.
        else
            json_number%precision_provided = .false.
        end if
    end function

    subroutine create_json_number(json, number, precision)
        class(json_value_t), allocatable, intent(out) :: json
        double precision, intent(in) :: number
        integer, optional, intent(in) :: precision

        type(json_number_t), allocatable :: local

        allocate(local)
        local%number = number
        if (present(precision)) then
            local%precision = precision
            local%precision_provided = .true.
        else
            local%precision_provided = .false.
        end if
        call move_alloc(local, json)
    end subroutine

    elemental function number_to_string(self) result(string)
        class(json_number_t), intent(in) :: self
        type(varying_string) :: string

        if (self%precision_provided) then
            string = to_string(self%number, self%precision)
        else
            string = to_string(self%number)
        end if
    end function
end module
