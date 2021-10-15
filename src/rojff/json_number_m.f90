module rojff_json_number_m
    use iso_varying_string, only: varying_string, assignment(=), char
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t
    use strff, only: to_string

    implicit none
    private
    public :: json_number_t, create_json_number

    type, extends(json_value_t) :: json_number_t
        double precision :: number
        integer :: precision
        logical :: precision_provided
    contains
        procedure :: equals
        procedure :: to_compact_string => number_to_string
        procedure :: write_to_compactly => write_to
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

    elemental function equals(lhs, rhs)
        class(json_number_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_number_t)
            ! avoids warnings about comparing floating point numbers
            equals = (.not. (lhs%number < rhs%number)) .and. (.not. (lhs%number > rhs%number))
        class default
            equals = .false.
        end select
    end function

    pure function number_to_string(self) result(string)
        class(json_number_t), intent(in) :: self
        type(varying_string) :: string

        if (self%precision_provided) then
            string = to_string(self%number, self%precision)
        else
            string = to_string(self%number)
        end if
    end function

    subroutine write_to(self, sink)
        class(json_number_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        call sink%append(char(self%to_compact_string()))
    end subroutine
end module
