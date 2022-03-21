module triangle_m
    implicit none
    private
    public :: triangle_t

    type :: triangle_t
        private
        real :: height_, width_
    contains
        procedure :: area
    end type

    interface triangle_t
        module procedure construct
    end interface
contains
    pure function construct(height, width) result(triangle)
        real, intent(in) :: height, width
        type(triangle_t) :: triangle

        triangle%height_ = height
        triangle%width_ = width
    end function

    pure function area(self)
        class(triangle_t), intent(in) :: self
        real :: area

        area = 0.5 * self%height_ * self%width_
    end function
end module
