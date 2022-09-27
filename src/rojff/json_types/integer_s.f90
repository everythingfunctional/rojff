submodule(rojff_json_integer_m) rojff_json_integer_s
    implicit none
contains
    module procedure constructor
        json_integer%number = number
    end procedure

    module procedure create_json_integer
        allocate(json)
        json%number = number
    end procedure

    module procedure equals
        select type (rhs)
        type is (json_integer_t)
            equals = lhs%number == rhs%number
        class default
            equals = .false.
        end select
    end procedure

    module procedure write_to_compactly
        character(len=11) :: temp

        write(temp, '(I0)') self%number
        call sink%append(trim(temp))
    end procedure

    module procedure write_to_expanded
        associate(unused => indentation_level); end associate
        call self%write_to_compactly(sink)
    end procedure
end submodule