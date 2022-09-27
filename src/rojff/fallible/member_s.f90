submodule(rojff_fallible_json_member_m) rojff_fallible_json_member_s
    use iso_varying_string, only: char
    use rojff_fallible_json_value_m, only: move_into_fallible_value
    use rojff_json_member_m, only: move_into_member
    use rojff_fallible_json_string_m, only: move_into_fallible_string

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
        type(fallible_json_string_t) :: local_key
        type(fallible_json_value_t) :: local_value

        local_key = maybe_key
        local_value = maybe_value
        call move_into_fallible_member(fallible_member, local_key, local_value)
    end procedure

    module procedure from_member
        fallible_member%member = member
    end procedure

    module procedure from_errors
        fallible_member%errors = errors
    end procedure

    module procedure from_fallible_member
        if (maybe_member%failed()) then
            fallible_member%errors = error_list_t( &
                    maybe_member%errors, module_, procedure_)
        else
            fallible_member%member = maybe_member%member
        end if
    end procedure

    module procedure move_character_and_value
        type(fallible_json_member_t) :: maybe_member
        type(fallible_json_value_t) :: maybe_value

        call move_into_fallible_value(maybe_value, value_)
        call move_into_fallible_member(maybe_member, key, maybe_value)
        call move_into_fallible_member( &
                fallible_member, &
                maybe_member, &
                module_t(MODULE_NAME), &
                procedure_t("move_character_and_value"))
    end procedure

    module procedure move_character_and_fallible_value
        character(len=*), parameter :: PROCEDURE_NAME = "move_character_and_fallible_value"
        type(fallible_json_member_t) :: maybe_member
        type(fallible_json_string_t) :: maybe_key

        maybe_key = fallible_json_string_t( &
                fallible_json_string_t(key), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        call move_into_fallible_member(maybe_member, maybe_key, maybe_value)
        call move_into_fallible_member( &
                fallible_member, &
                maybe_member, &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
    end procedure

    module procedure move_string_and_value
        type(fallible_json_member_t) :: maybe_member

        call move_into_fallible_member(maybe_member, char(key), value_)
        call move_into_fallible_member( &
                fallible_member, &
                maybe_member, &
                module_t(MODULE_NAME), &
                procedure_t("move_string_and_value"))
    end procedure

    module procedure move_string_and_fallible_value
        type(fallible_json_member_t) :: maybe_member

        call move_into_fallible_member(maybe_member, char(key), maybe_value)
        call move_into_fallible_member( &
                fallible_member, &
                maybe_member, &
                module_t(MODULE_NAME), &
                procedure_t("move_string_and_fallible_value"))
    end procedure

    module procedure move_json_string_and_fallible_value
        type(fallible_json_member_t) :: maybe_member
        type(fallible_json_string_t) :: maybe_string

        call move_into_fallible_string(maybe_string, key)
        call move_into_fallible_member(maybe_member, maybe_string, maybe_value)
        call move_into_fallible_member( &
                fallible_member, &
                maybe_member, &
                module_t(MODULE_NAME), &
                procedure_t("move_json_string_and_fallible_value"))
    end procedure

    module procedure move_fallible_string_and_value
        type(fallible_json_value_t) :: maybe_value

        call move_into_fallible_value(maybe_value, value_)
        call move_fallible_string_and_fallible_value(fallible_member, maybe_key, maybe_value)
    end procedure

    module procedure move_fallible_string_and_fallible_value
        if (any([maybe_key%failed(), maybe_value%failed()])) then
            fallible_member%errors = error_list_t([maybe_key%errors, maybe_value%errors])
        else
            call move_into_member(fallible_member%member, maybe_key%string, maybe_value%value_)
        end if
    end procedure

    module procedure move_from_fallible_member
        if (maybe_member%failed()) then
            fallible_member%errors = error_list_t( &
                    maybe_member%errors, module_, procedure_)
        else
            call move_into_member(fallible_member%member, maybe_member%member)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule