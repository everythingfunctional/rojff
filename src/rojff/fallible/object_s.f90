submodule(rojff_fallible_json_object_m) rojff_fallible_json_object_s
    use erloff, only: fatal_t
    use rojff_constants_m, only: INVALID_INPUT
    use rojff_fallible_json_value_m, only: move_into_fallible_value
    use rojff_json_member_m, only: move_into_member
    use rojff_json_object_m, only: move_into_object_unsafe
    use rojff_json_value_m, only: json_value_t

    implicit none

    character(len=*), parameter :: MODULE_NAME = "rojff_fallible_json_object_m"
contains
    module procedure from_object
        fallible_object%object = object
    end procedure

    module procedure from_errors
        fallible_object%errors = errors
    end procedure

    module procedure from_members
        type(json_member_t), allocatable :: local_members(:)

        allocate(local_members, source = members)
        call move_into_fallible_object(fallible_object, local_members)
    end procedure

    module procedure from_fallible_members
        integer :: i, num_members

        num_members = size(maybe_members)
        if (any(maybe_members%failed())) then
            block
                type(error_list_t), allocatable :: all_errors(:)

                allocate(all_errors(num_members))
                do concurrent (i = 1 : num_members)
                    all_errors(i) = maybe_members(i)%errors
                end do
                fallible_object%errors = error_list_t(all_errors)
            end block
        else
            block
                type(fallible_json_member_t), allocatable :: local_maybes(:)
                type(json_member_t), allocatable :: local_members(:)

                local_maybes = maybe_members
                allocate(local_members(num_members))
                do i = 1, num_members
                    call move_into_member(local_members(i), local_maybes(i)%member)
                end do
                fallible_object = fallible_json_object_t( &
                        fallible_json_object_t(local_members), &
                        module_t(MODULE_NAME), &
                        procedure_t("from_fallible_members"))
            end block
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

    module procedure fallible_json_value_from_fallible_array
        if (maybe_object%failed()) then
            fallible_value = fallible_json_value_t(maybe_object%errors)
        else
            fallible_value = fallible_json_value_t(maybe_object%object)
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

    module procedure move_from_fallible_members
        integer :: i, num_members

        num_members = size(maybe_members)
        if (any(maybe_members%failed())) then
            block
                type(error_list_t), allocatable :: all_errors(:)

                allocate(all_errors(num_members))
                do concurrent (i = 1 : num_members)
                    all_errors(i) = maybe_members(i)%errors
                end do
                fallible_object%errors = error_list_t(all_errors)
            end block
        else
            block
                type(json_member_t), allocatable :: local_members(:)
                type(fallible_json_object_t) :: local_object

                allocate(local_members(num_members))
                do i = 1, num_members
                    call move_into_member(local_members(i), maybe_members(i)%member)
                end do
                call move_into_fallible_object(local_object, local_members)
                call move_into_fallible_object(&
                        fallible_object, &
                        local_object, &
                        module_t(MODULE_NAME), &
                        procedure_t("move_from_fallible_members"))
            end block
        end if
    end procedure

    module procedure move_from_object
        call move_alloc(fallible_object%object, object)
    end procedure

    module procedure move_from_fallible_object
        if (maybe_object%failed()) then
            fallible_object%errors = error_list_t( &
                    maybe_object%errors, module_, procedure_)
        else
            call move_alloc(maybe_object%object, fallible_object%object)
        end if
    end procedure

    module procedure move_to_fallible_value
        class(json_value_t), allocatable :: tmp_val

        if (fallible_object%failed()) then
            fallible_value = fallible_json_value_t(fallible_object%errors)
        else
            call move_alloc(fallible_object%object, tmp_val)
            call move_into_fallible_value(fallible_value, tmp_val)
        end if
    end procedure

    module procedure failed
        failed = self%errors%has_any()
    end procedure
end submodule