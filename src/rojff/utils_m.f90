module rojff_utils_m
    use erloff, only: message_type_t

    implicit none
    private
    public :: is_nan, to_string, INVALID_INPUT

    interface to_string
        module procedure integer_to_string
        module procedure to_string_with_significant_digits
        module procedure to_string_without_significant_digits
    end interface

    type(message_type_t), parameter :: INVALID_INPUT = message_type_t( &
            "Invalid Input")
contains
    pure function integer_to_string(number) result(string)
        integer, intent(in) :: number
        character(len=:), allocatable :: string

        character(len=11) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function

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

        character(len=:), allocatable :: intermediate

        if (is_nan(number)) then
            intermediate = "NaN"
        else if (is_infinity(number)) then
            intermediate = "Inf"
        else if (is_zero(number)) then
            intermediate = "0.0"
        else
            block
                integer, parameter :: C_LEN = 34
                double precision :: abs_num
                character(len=C_LEN) :: exponent_part
                character(len=C_LEN) :: floating_part
                character(len=7) :: format_string
                character(len=:), allocatable :: intermediate_basic
                character(len=:), allocatable :: intermediate_scientific
                integer :: scale_

                abs_num = abs(number)
                scale_ = floor(log10(abs_num))
                if (scale_ <= -2) then
                    write(format_string, '(A,I0,A)') &
                            "(f0.", significant_digits-1, ")"
                    write(floating_part, format_string) &
                            abs_num * 10.0d0**(-scale_)
                    write(exponent_part, '(A,I0)') 'e', scale_
                    intermediate = &
                            cover_empty_decimal( &
                                    remove_trailing_zeros(trim(floating_part))) &
                            // trim(exponent_part)
                else
                    write(format_string, '(A,I0,A)') &
                            "(f0.", significant_digits-1, ")"
                    write(floating_part, format_string) abs_num / 10.0d0**scale_
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
            end block
        end if
        if (is_negative(number)) then
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

    pure function is_infinity(val)
        double precision, intent(in) :: val
        logical :: is_infinity


        if (is_nan(val)) then
            is_infinity = .false.
        else
            ! This isn't quite right. It is conceivable for a processor
            ! to have numbers greater than huge that aren't considered infinity,
            ! but for now I don't know of a reliable way to test for that
            is_infinity = val < -huge(val) .or. val > huge(val)
        end if
    end function

    pure function is_nan(val)
        double precision, intent(in) :: val
        logical :: is_nan

        is_nan = .not.(val >= 0 .or. val <= 0)
    end function

    pure function is_negative(val)
        double precision, intent(in) :: val
        logical :: is_negative

        if (is_nan(val)) then
            is_negative = .false.
        else
            is_negative = sign(1.0d0, val) < 0
        end if
    end function

    pure function is_zero(val)
        double precision, intent(in) :: val
        logical :: is_zero

        if (is_nan(val)) then
            is_zero = .false.
        else
            is_zero = .not. (val > 0 .or. val < 0)
        end if
    end function
end module
