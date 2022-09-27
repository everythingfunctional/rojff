submodule(rojff_fallible_json_element_m) rojff_fallible_json_element_s
    use rojff_json_element_m, only: move_into_element

    implicit none
contains
    module procedure from_element
        fallible_element%element = element
    end procedure

    module procedure from_errors
        fallible_element%errors = errors
    end procedure

    module procedure from_fallible_element
        if (maybe_element%failed()) then
            fallible_element%errors = error_list_t( &
                    maybe_element%errors, module_, procedure_)
        else
            fallible_element%element = maybe_element%element
        end if
    end procedure

    module procedure from_json_value
        fallible_element%element = json_element_t(value_)
    end procedure

    module procedure from_fallible_value
        if (maybe_value%failed()) then
            fallible_element%errors = maybe_value%errors
        else
            fallible_element%element = json_element_t(maybe_value%value_)
        end if
    end procedure

    module procedure move_from_value
        call move_into_element(fallible_element%element, value_)
    end procedure

    module procedure move_from_fallible_value
        if (maybe_value%failed()) then
            fallible_element%errors = maybe_value%errors
        else
            call move_into_element(fallible_element%element, maybe_value%value_)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule