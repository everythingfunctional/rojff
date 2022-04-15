module integer_input_m
    use veggies, only: input_t

    implicit none
    private
    public :: integer_input_t

    type, extends(input_t) :: integer_input_t
        private
        character(len=:), allocatable :: string_
        integer :: value__
    contains
        private
        procedure, public :: string
        procedure, public :: value_
    end type

    interface integer_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(string, value_) result(number_input)
        character(len=*), intent(in) :: string
        integer, intent(in)  :: value_
        type(integer_input_t) :: number_input

        number_input%string_ = string
        number_input%value__ = value_
    end function

    pure function string(self)
        class(integer_input_t), intent(in) :: self
        character(len=:), allocatable :: string

        string = self%string_
    end function

    pure function value_(self)
        class(integer_input_t), intent(in) :: self
        integer :: value_

        value_ = self%value__
    end function
end module
