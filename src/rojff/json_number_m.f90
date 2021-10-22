module rojff_json_number_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

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

    interface to_string
        module procedure to_string_with_significant_digits
        module procedure to_string_without_significant_digits
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
        if (self%precision_provided) then
            call sink%append(to_string(self%number, self%precision))
        else
            call sink%append(to_string(self%number))
        end if
    end subroutine

    pure function to_string_without_significant_digits(number) result(string)
        double precision, intent(in) :: number
        character(len=:), allocatable :: string

        string = to_string(number, 17)
    end function

    pure function to_string_with_significant_digits( &
            number, significant_digits) result(string_)
        double precision, intent(in) :: number
        integer, intent(in) :: significant_digits
        character(len=:), allocatable :: string_

        integer, parameter :: C_LEN = 34
        double precision, parameter :: MACHINE_TINY = tiny(0.0d0)
        double precision :: abs_num
        character(len=C_LEN) :: exponent_part
        character(len=C_LEN) :: floating_part
        character(len=7) :: format_string
        character(len=:), allocatable :: intermediate
        character(len=:), allocatable :: intermediate_basic
        character(len=:), allocatable :: intermediate_scientific
        integer :: scale_

        abs_num = abs(number)
        if (abs_num <= MACHINE_TINY) then
            string_ = "0.0"
            return
        end if
        scale_ = floor(log10(abs_num))
        if (scale_ <= -2) then
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) &
                    abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') &
                        "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = cover_empty_decimal( &
                        remove_trailing_zeros(trim(floating_part)))

                if (len(intermediate_scientific) < len(intermediate_basic)) then
                    intermediate = intermediate_scientific
                else
                    intermediate = intermediate_basic
                end if
            else
                intermediate = intermediate_scientific
            end if
        end if
        if (number < 0.0D0) then
            string_ = "-" // intermediate
        else
            string_ = intermediate
        end if
    end function

    pure function cover_empty_decimal(number) result(fixed)
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: fixed

        if (last_character(number) == ".") then
            fixed = number // "0"
        else if (first_character(number) == ".") then
            fixed = "0" // number
        else
            fixed = number
        end if
    end function

    pure function first_character(string)
        character(len=*), intent(in) :: string
        character(len=1) :: first_character

        first_character = string(1:1)
    end function

    pure function last_character(string)
        character(len=*), intent(in) :: string
        character(len=1) :: last_character

        integer :: length

        length = len(string)
        last_character = string(length:length)
    end function

    pure recursive function remove_trailing_zeros(number) result(trimmed)
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: trimmed

        if (last_character(number) == "0") then
            trimmed = remove_trailing_zeros(without_last_character(number))
        else
            trimmed = number
        end if
    end function

    pure function without_last_character(string) result(trimmed)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: trimmed

        trimmed = string(1:len(string) - 1)
    end function
end module
