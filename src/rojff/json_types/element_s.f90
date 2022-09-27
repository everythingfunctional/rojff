submodule(rojff_json_element_m) rojff_json_element_s
    implicit none
contains
    module procedure constructor
        element%json = json
    end procedure

    module procedure move_from_value
        call move_alloc(json, element%json)
    end procedure

    module procedure move_from_element
        call move_alloc(from%json, to%json)
    end procedure

    module procedure equals
        equals = lhs%json == rhs%json
    end procedure

    module procedure write_to_compactly
        call self%json%write_to_compactly(sink)
    end procedure

    module procedure write_to_expanded
        call self%json%write_to_expanded(indentation_level, sink)
    end procedure
end submodule