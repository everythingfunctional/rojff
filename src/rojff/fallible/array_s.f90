submodule(rojff_fallible_json_array_m) rojff_fallible_json_array_s
    implicit none
contains
    module procedure from_array
        fallible_array%array = array
    end procedure

    module procedure from_errors
        fallible_array%errors = errors
    end procedure

    module procedure from_fallible_array
        if (maybe_array%failed()) then
            fallible_array%errors = error_list_t( &
                    maybe_array%errors, module_, procedure_)
        else
            fallible_array%array = maybe_array%array
        end if
    end procedure

    module procedure from_elements
        fallible_array%array = json_array_t(elements)
    end procedure

    module procedure from_fallible_elements
        if (any(maybe_elements%failed())) then
            block
                type(error_list_t), allocatable :: all_errors(:)
                integer :: i, num_elements
                num_elements = size(maybe_elements)
                allocate(all_errors(num_elements))
                do concurrent (i = 1 : num_elements)
                    all_errors(i) = maybe_elements(i)%errors
                end do
                fallible_array%errors = error_list_t(all_errors)
            end block
        else
            fallible_array = fallible_json_array_t(maybe_elements%element)
        end if
    end procedure

    module procedure fallible_json_value_from_fallible_array
        if (maybe_array%failed()) then
            fallible_value = fallible_json_value_t(maybe_array%errors)
        else
            fallible_value = fallible_json_value_t(maybe_array%array)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule