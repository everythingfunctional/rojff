submodule(rojff_fallible_json_value_m) rojff_fallible_json_value_s
    implicit none
contains
    module procedure from_value
        fallible_value%value_ = value_
    end procedure

    module procedure from_errors
        fallible_value%errors = errors
    end procedure

    module procedure from_fallible_value
        if (maybe_value%failed()) then
            fallible_value%errors = error_list_t( &
                    maybe_value%errors, module_, procedure_)
        else
            fallible_value%value_ = maybe_value%value_
        end if
    end procedure

    module procedure move_from_value
        call move_alloc(value_, fallible_value%value_)
    end procedure

    module procedure move_from_fallible_value
        if (maybe_value%failed()) then
            fallible_value%errors = error_list_t( &
                    maybe_value%errors, module_, procedure_)
        else
            call move_alloc(maybe_value%value_, fallible_value%value_)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule