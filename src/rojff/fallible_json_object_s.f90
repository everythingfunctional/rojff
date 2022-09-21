submodule(rojff_fallible_json_object_m) rojff_fallible_json_object_s
    use erloff, only: fatal_t
    use rojff_json_object_m, only: move_into_object_unsafe
    use rojff_utils_m, only: INVALID_INPUT

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_fallible_json_object_m"
contains
    module procedure from_members
        type(json_member_t), allocatable :: local_members(:)

        allocate(local_members, source = members)
        call move_into_fallible_object(fallible_object, local_members)
    end procedure

    module procedure from_fallible_members
        if (any(maybe_members%failed())) then
            block
                type(error_list_t), allocatable :: all_errors(:)
                integer :: i, num_members
                num_members = size(maybe_members)
                allocate(all_errors(num_members))
                do concurrent (i = 1 : num_members)
                    all_errors(i) = maybe_members(i)%errors
                end do
                fallible_object%errors = error_list_t(all_errors)
            end block
        else
            fallible_object = fallible_json_object_t( &
                    fallible_json_object_t(maybe_members%member), &
                    module_t(MODULE_NAME), &
                    procedure_t("from_fallible_members"))
        end if
    end procedure

    module procedure from_fallible_object
        if (maybe_object%failed()) then
            fallible_object%errors = error_list_t( &
                    maybe_object%errors, module_, procedure_)
        else
            fallible_object%object = maybe_object%object
        end if
    end procedure

    module procedure move_from_members_into_fallible_object
        logical :: has_duplicates
        integer :: i, j

        has_duplicates = .false.
        unique_search: do i = 2, size(members)
            do j = 1, i-1
                if (members(i)%key == members(j)%key) then
                    has_duplicates = .true.
                    exit unique_search
                end if
            end do
        end do unique_search

        if (has_duplicates) then
            fallible_object%errors = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("move_from_members_into_fallible_object"), &
                    'Duplicate key found: "' // members(i)%key // '"'))
        else
            call move_into_object_unsafe(fallible_object%object, members)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule