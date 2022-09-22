submodule(rojff_fallible_json_element_m) rojff_fallible_json_element_s
    implicit none
contains
    module procedure from_json_value
        fallible_element%element = json_element_t(value_)
    end procedure

    module procedure from_fallible_value
        if (maybe_value%failed()) then
            fallible_element%errors = maybe_value%errors
        else
            fallible_element%element = json_element_t(maybe_value%json)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule