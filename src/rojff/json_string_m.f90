module rojff_json_string_m
    use rojff_json_value_m, only: json_value_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_string_t, create_json_string_unsafe, json_string_unsafe

    type, extends(json_value_t) :: json_string_t
        character(len=:), allocatable :: string
    contains
        procedure :: equals
        procedure :: write_to_compactly
    end type
contains
    function json_string_unsafe(string) result(json_string)
        character(len=*), intent(in) :: string
        type(json_string_t) :: json_string

        json_string%string = string
    end function

    subroutine create_json_string_unsafe(json, string)
        class(json_value_t), allocatable, intent(out) :: json
        character(len=*), intent(in) :: string

        type(json_string_t), allocatable :: local

        allocate(local)
        local%string = string
        call move_alloc(local, json)
    end subroutine

    elemental function equals(lhs, rhs)
        class(json_string_t), intent(in) :: lhs
        class(json_value_t), intent(in) :: rhs
        logical :: equals

        select type (rhs)
        type is (json_string_t)
            equals = lhs%string == rhs%string
        class default
            equals = .false.
        end select
    end function

    subroutine write_to_compactly(self, sink)
        class(json_string_t), intent(in) :: self
        class(string_sink_t), intent(inout) :: sink

        call sink%append('"' // self%string // '"')
    end subroutine
end module
