module rojff_fallible_json_member_m
    use erloff, only: error_list_t, module_t, procedure_t
    use iso_varying_string, only: varying_string, char
    use rojff_fallible_json_string_m, only: fallible_json_string_t
    use rojff_fallible_json_value_m, only: fallible_json_value_t
    use rojff_json_member_m, only: json_member_t, json_member_unsafe
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
        module procedure from_character_and_value
        module procedure from_character_and_fallible_value
        module procedure from_string_and_value
        module procedure from_string_and_fallible_value
        module procedure from_json_string_and_fallible_value
        module procedure from_fallible_string_and_value
        module procedure from_fallible_string_and_fallible_value
        module procedure from_fallible_member
    end interface

    character(len=*), parameter :: MODULE_NAME = "rojff_fallible_json_member_m"
contains
    function from_character_and_value(key, value_) result(fallible_member)
        character(len=*), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t( &
                fallible_json_member_t(fallible_json_string_t(key), value_), &
                module_t(MODULE_NAME), &
                procedure_t("from_character_and_value"))
    end function

    function from_character_and_fallible_value(key, maybe_value) result(fallible_member)
        character(len=*), intent(in) :: key
        type(fallible_json_value_t), intent(in) :: maybe_value
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t( &
                fallible_json_string_t( &
                        fallible_json_string_t(key), &
                        module_t(MODULE_NAME), &
                        procedure_t("from_character_and_fallible_value")), &
                maybe_value)
    end function

    function from_string_and_value(key, value_) result(fallible_member)
        type(varying_string), intent(in) :: key
        class(json_value_t), intent(in) :: value_
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t( &
                fallible_json_member_t(char(key), value_), &
                module_t(MODULE_NAME), &
                procedure_t("from_string_and_value"))
    end function

    function from_string_and_fallible_value(key, maybe_value) result(fallible_member)
        type(varying_string), intent(in) :: key
        type(fallible_json_value_t), intent(in) :: maybe_value
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t( &
                fallible_json_string_t( &
                        fallible_json_string_t(char(key)), &
                        module_t(MODULE_NAME), &
                        procedure_t("from_string_and_fallible_value")), &
                maybe_value)
    end function

    function from_json_string_and_fallible_value(key, maybe_value) result(fallible_member)
        type(json_string_t), intent(in) :: key
        type(fallible_json_value_t), intent(in) :: maybe_value
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t( &
                fallible_json_string_t(key), maybe_value)
    end function

    function from_fallible_string_and_value( &
            maybe_key, value_) result(fallible_member)
        type(fallible_json_string_t), intent(in) :: maybe_key
        class(json_value_t), intent(in) :: value_
        type(fallible_json_member_t) :: fallible_member

        fallible_member = fallible_json_member_t(maybe_key, fallible_json_value_t(value_))
    end function

    function from_fallible_string_and_fallible_value( &
            maybe_key, maybe_value) result(fallible_member)
        type(fallible_json_string_t), intent(in) :: maybe_key
        type(fallible_json_value_t), intent(in) :: maybe_value
        type(fallible_json_member_t) :: fallible_member

        if (any([maybe_key%failed(), maybe_value%failed()])) then
            fallible_member%errors = error_list_t([maybe_key%errors, maybe_value%errors])
        else
            fallible_member%member = json_member_t(maybe_key%string, maybe_value%json)
        end if
    end function

    function from_fallible_member( &
            maybe_member, module_, procedure_) result(fallible_member)
        type(fallible_json_member_t), intent(in) :: maybe_member
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_member_t) :: fallible_member

        if (maybe_member%failed()) then
            fallible_member%errors = error_list_t( &
                    maybe_member%errors, module_, procedure_)
        else
            fallible_member%member = maybe_member%member
        end if
    end function

    elemental function failed(self)
        class(fallible_json_member_t), intent(in) :: self
        logical :: failed

        failed = self%errors%has_any()
    end function
end module