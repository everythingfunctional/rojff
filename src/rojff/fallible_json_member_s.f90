submodule(rojff_fallible_json_member_m) rojff_fallible_json_member_s
    use iso_varying_string, only: char

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_fallible_json_member_m"
contains
    module procedure from_character_and_value
        fallible_member = fallible_json_member_t( &
                fallible_json_member_t(fallible_json_string_t(key), value_), &
                module_t(MODULE_NAME), &
                procedure_t("from_character_and_value"))
    end procedure

    module procedure from_character_and_fallible_value
        fallible_member = fallible_json_member_t( &
                    fallible_json_string_t( &
                            fallible_json_string_t(key), &
                            module_t(MODULE_NAME), &
                            procedure_t("from_character_and_fallible_value")), &
                    maybe_value)
    end procedure

    module procedure from_string_and_value
        fallible_member = fallible_json_member_t( &
                    fallible_json_member_t(char(key), value_), &
                    module_t(MODULE_NAME), &
                    procedure_t("from_string_and_value"))
    end procedure

    module procedure from_string_and_fallible_value
        fallible_member = fallible_json_member_t( &
                    fallible_json_string_t( &
                            fallible_json_string_t(char(key)), &
                            module_t(MODULE_NAME), &
                            procedure_t("from_string_and_fallible_value")), &
                    maybe_value)
    end procedure

    module procedure from_json_string_and_fallible_value
        fallible_member = fallible_json_member_t( &
                    fallible_json_string_t(key), maybe_value)
    end procedure

    module procedure from_fallible_string_and_value
        fallible_member = fallible_json_member_t(maybe_key, fallible_json_value_t(value_))
    end procedure

    module procedure from_fallible_string_and_fallible_value
        if (any([maybe_key%failed(), maybe_value%failed()])) then
            fallible_member%errors = error_list_t([maybe_key%errors, maybe_value%errors])
        else
            fallible_member%member = json_member_t(maybe_key%string, maybe_value%json)
        end if
    end procedure

    module procedure from_fallible_member
        if (maybe_member%failed()) then
            fallible_member%errors = error_list_t( &
                    maybe_member%errors, module_, procedure_)
        else
            fallible_member%member = maybe_member%member
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule