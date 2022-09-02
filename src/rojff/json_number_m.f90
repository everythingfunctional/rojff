module rojff_json_number_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t
    use rojff_utils_m, only: to_string

    implicit none
    private
    public :: json_number_t, create_json_number

    type, extends(json_value_t) :: json_number_t
        double precision :: number
        integer :: precision
        logical :: precision_provided
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
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
            json_number%precision = 0
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

    subroutine write_to_compactly(self, sink)
        class(json_number_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        if (self%precision_provided) then
            call sink%append(to_string(self%number, self%precision))
        else
            call sink%append(to_string(self%number))
        end if
    end subroutine

    subroutine write_to_expanded(self, indentation_level, sink)
        class(json_number_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        associate(unused => indentation_level); end associate
        call self%write_to_compactly(sink)
    end subroutine
end module
