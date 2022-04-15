module number_input_m
    use veggies, only: input_t

    implicit none
    private
    public :: number_input_t

    type, extends(input_t) :: number_input_t
        private
        character(len=:), allocatable :: string_
        double precision :: value__
    contains
        private
        procedure, public :: string
        procedure, public :: value_
    end type

    interface number_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(string, value_) result(number_input)
        character(len=*), intent(in) :: string
        double precision, intent(in)  :: value_
        type(number_input_t) :: number_input

        number_input%string_ = string
        number_input%value__ = value_
    end function

    pure function string(self)
        class(number_input_t), intent(in) :: self
        character(len=:), allocatable :: string

        string = self%string_
    end function

    pure function value_(self)
        class(number_input_t), intent(in) :: self
        double precision :: value_

        value_ = self%value__
    end function
end module
