submodule(rojff_fallible_json_array_m) rojff_fallible_json_array_s
    use rojff_fallible_json_value_m, only: move_into_fallible_value
    use rojff_json_array_m, only: move_into_array
    use rojff_json_element_m, only: move_into_element
    use rojff_json_value_m, only: json_value_t

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
        type(fallible_json_element_t), allocatable :: local_elements(:)

        local_elements = maybe_elements
        call move_into_fallible_array(fallible_array, local_elements)
    end procedure

    module procedure fallible_json_value_from_fallible_array
        if (maybe_array%failed()) then
            fallible_value = fallible_json_value_t(maybe_array%errors)
        else
            fallible_value = fallible_json_value_t(maybe_array%array)
        end if
    end procedure

    module procedure move_elements
        call move_into_array(fallible_array%array, elements)
    end procedure

    module procedure move_fallible_elements
        integer :: i, num_elements

        num_elements = size(maybe_elements)
        if (any(maybe_elements%failed())) then
            block
                type(error_list_t), allocatable :: all_errors(:)

                allocate(all_errors(num_elements))
                do concurrent (i = 1 : num_elements)
                    all_errors(i) = maybe_elements(i)%errors
                end do
                fallible_array%errors = error_list_t(all_errors)
            end block
        else
            block
                type(json_element_t), allocatable :: local_elements(:)

                allocate(local_elements(num_elements))
                do i = 1, num_elements
                    call move_into_element(local_elements(i), maybe_elements(i)%element)
                end do
                call move_into_fallible_array(fallible_array, local_elements)
            end block
        end if
    end procedure

    module procedure move_from_array
        call move_alloc(array, fallible_array%array)
    end procedure

    module procedure move_from_fallible_array
        if (maybe_array%failed()) then
            fallible_array%errors = error_list_t( &
                    maybe_array%errors, module_, procedure_)
        else
            call move_alloc(maybe_array%array, fallible_array%array)
        end if
    end procedure

    module procedure move_to_fallible_value
        class(json_value_t), allocatable :: tmp_val

        if (fallible_array%failed()) then
            fallible_value = fallible_json_value_t(fallible_array%errors)
        else
            call move_alloc(fallible_array%array, tmp_val)
            call move_into_fallible_value(fallible_value, tmp_val)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule