module rojff_fallible_json_member_m
    use erloff, only: error_list_t, module_t, procedure_t
    use iso_varying_string, only: varying_string
    use rojff_fallible_json_string_m, only: fallible_json_string_t
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_string_m, only: json_string_t
    use rojff_json_value_m, only: json_value_t

    implicit none
    private
    public :: fallible_json_member_t

    type :: fallible_json_member_t
        type(json_member_t) :: member
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_member_t
        module function from_character_and_value( &
                key, value_) result(fallible_member)
            implicit none
            character(len=*), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_character_and_fallible_value( &
                key, maybe_value) result(fallible_member)
            implicit none
            character(len=*), intent(in) :: key
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_string_and_value( &
                key, value_) result(fallible_member)
            implicit none
            type(varying_string), intent(in) :: key
            class(json_value_t), intent(in) :: value_
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_string_and_fallible_value( &
                key, maybe_value) result(fallible_member)
            implicit none
            type(varying_string), intent(in) :: key
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_json_string_and_fallible_value( &
                key, maybe_value) result(fallible_member)
            implicit none
            type(json_string_t), intent(in) :: key
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_fallible_string_and_value( &
                maybe_key, value_) result(fallible_member)
            implicit none
            type(fallible_json_string_t), intent(in) :: maybe_key
            class(json_value_t), intent(in) :: value_
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_fallible_string_and_fallible_value( &
                maybe_key, maybe_value) result(fallible_member)
            implicit none
            type(fallible_json_string_t), intent(in) :: maybe_key
            type(fallible_json_value_t), intent(in) :: maybe_value
            type(fallible_json_member_t) :: fallible_member
        end function

        module function from_fallible_member( &
                maybe_member, module_, procedure_) result(fallible_member)
            implicit none
            type(fallible_json_member_t), intent(in) :: maybe_member
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            type(fallible_json_member_t) :: fallible_member
        end function
    end interface

    interface
        elemental module function failed(self)
            implicit none
            class(fallible_json_member_t), intent(in) :: self
            logical :: failed
        end function
    end interface
end module