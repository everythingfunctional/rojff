submodule(rojff_json_string_m) rojff_json_string_s
    use iso_varying_string, only: char

    implicit none
contains
    module procedure json_string_unsafe_c
        json_string%string = string
    end procedure

    module procedure json_string_unsafe_s
        json_string = json_string_unsafe(char(string))
    end procedure

    module procedure create_json_string_unsafe_c
        allocate(json)
        json%string = string
    end procedure

    module procedure create_json_string_unsafe_s
        call create_json_string_unsafe(json, char(string))
    end procedure

    module procedure move_into_json_string_unsafe
        allocate(json)
        call move_alloc(string, json%string)
    end procedure

    module procedure equals
        select type (rhs)
        type is (json_string_t)
            equals = lhs%string == rhs%string .and. len(lhs%string) == len(rhs%string)
        class default
            equals = .false.
        end select
    end procedure

    module procedure write_to_compactly
        call sink%append('"' // self%string // '"')
    end procedure

    module procedure write_to_expanded
        associate(unused => indentation_level); end associate
        call sink%append('"' // self%string // '"')
    end procedure
end submodule