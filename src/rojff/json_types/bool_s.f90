submodule(rojff_json_bool_m) rojff_json_bool_s
    implicit none
contains
    module procedure constructor
        json_bool%bool = bool
    end procedure

    module procedure create_json_bool
        allocate(json)
        json%bool = bool
    end procedure

    module procedure equals
        select type (rhs)
        type is (json_bool_t)
            equals = lhs%bool .eqv. rhs%bool
        class default
            equals = .false.
        end select
    end procedure

    module procedure write_to_compactly
        if (self%bool) then
            call sink%append("true")
        else
            call sink%append("false")
        end if
    end procedure

    module procedure write_to_expanded
        associate(unused => indentation_level); end associate
        if (self%bool) then
            call sink%append("true")
        else
            call sink%append("false")
        end if
    end procedure
end submodule