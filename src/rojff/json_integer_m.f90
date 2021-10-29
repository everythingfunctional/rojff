module rojff_json_integer_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_integer_t, create_json_integer

    type, extends(json_value_t) :: json_integer_t
        integer :: number
    contains
        procedure :: equals
        procedure :: write_to_compactly
        procedure :: write_to_expanded
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

    elemental function equals(lhs, rhs)
        class(json_integer_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_integer_t)
            equals = lhs%number == rhs%number
        class default
            equals = .false.
        end select
    end function

    subroutine write_to_compactly(self, sink)
        class(json_integer_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        character(len=11) :: temp

        write(temp, '(I0)') self%number
        call sink%append(trim(temp))
    end subroutine

    subroutine write_to_expanded(self, indentation_level, sink)
        class(json_integer_t), intent(in) :: self
        integer, intent(in) :: indentation_level
        class(string_sink_t), intent(inout) :: sink

        associate(unused => indentation_level); end associate
        call self%write_to_compactly(sink)
    end subroutine
end module
