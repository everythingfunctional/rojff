module rojff_fallible_json_object_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use rojff_fallible_json_member_m, only: fallible_json_member_t
    use rojff_json_member_m, only: json_member_t
    use rojff_json_object_m, only: json_object_t, json_object_unsafe, move_into_object_unsafe
    use rojff_json_value_m, only: json_value_t
    use rojff_utils_m, only: INVALID_INPUT

    implicit none
    private
    public :: fallible_json_object_t, move_into_fallible_object

    type :: fallible_json_object_t
        type(json_object_t), allocatable :: object
        type(error_list_t) :: errors
    contains
        procedure :: failed
    end type

    interface fallible_json_object_t
        module procedure from_members
        module procedure from_fallible_members
        module procedure from_fallible_object
    end interface

    interface move_into_fallible_object
        module procedure move_from_members_into_fallible_object
    end interface

    character(len=*), parameter :: MODULE_NAME = "rojff_fallible_json_object_m"
contains
    function from_members(members) result(fallible_object)
        type(json_member_t), intent(in) :: members(:)
        type(fallible_json_object_t) :: fallible_object

        type(json_member_t), allocatable :: local_members(:)

        allocate(local_members, source = members)
        call move_into_fallible_object(fallible_object, local_members)
    end function

    function from_fallible_members(maybe_members) result(fallible_object)
        type(fallible_json_member_t), intent(in) :: maybe_members(:)
        type(fallible_json_object_t) :: fallible_object

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
    end function

    function from_fallible_object( &
            maybe_object, module_, procedure_) result(fallible_object)
        type(fallible_json_object_t), intent(in) :: maybe_object
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_object_t) :: fallible_object

        if (maybe_object%failed()) then
            fallible_object%errors = error_list_t( &
                    maybe_object%errors, module_, procedure_)
        else
            fallible_object%object = maybe_object%object
        end if
    end function

    subroutine move_from_members_into_fallible_object(fallible_object, members)
        type(fallible_json_object_t), intent(out) :: fallible_object
        type(json_member_t), allocatable, intent(inout) :: members(:)

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
    end subroutine

    elemental function failed(self)
        class(fallible_json_object_t), intent(in) :: self
        logical :: failed

        failed = self%errors%has_any()
    end function
end module