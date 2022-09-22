submodule(rojff_fallible_json_value_m) rojff_fallible_json_value_s
    implicit none
contains
    module procedure from_value
        fallible_json%json = json
    end procedure

    module procedure from_errors
        fallible_json%errors = errors
    end procedure

    module procedure from_fallible_json
        if (original%failed()) then
            new%errors = error_list_t(original%errors, module_, procedure_)
        else
            new%json = original%json
        end if
    end procedure

    module procedure move_into_fallible_json
        call move_alloc(json, fallible_json%json)
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule