submodule(rojff_json_null_m) rojff_json_null_s
    implicit none
contains
    module procedure create_json_null
        allocate(json)
    end procedure

    module procedure equals
        associate(unused => lhs); end associate
        select type (rhs)
        type is (json_null_t)
            equals = .true.
        class default
            equals = .false.
        end select
    end procedure

    module procedure write_to_compactly
        associate(unused => self); end associate
        call sink%append("null")
    end procedure

    module procedure write_to_expanded
        associate(unused => indentation_level); end associate
        associate(unused => self); end associate
        call sink%append("null")
    end procedure
end submodule