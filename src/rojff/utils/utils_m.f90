module rojff_utils_m
    implicit none
    private
    public :: is_nan, to_string

    interface to_string
        pure module function integer_to_string(number) result(string)
            implicit none
            integer, intent(in) :: number
            character(len=:), allocatable :: string
        end function

        pure module function to_string_without_significant_digits( &
                number) result(string)
            implicit none
            double precision, intent(in) :: number
            character(len=:), allocatable :: string
        end function

        pure module function to_string_with_significant_digits( &
                number, significant_digits) result(string_)
            implicit none
            double precision, intent(in) :: number
            integer, intent(in) :: significant_digits
            character(len=:), allocatable :: string_
        end function
    end interface

    interface
        pure module function is_nan(val)
            implicit none
            double precision, intent(in) :: val
            logical :: is_nan
        end function
    end interface
end module
