module fallible_triangle_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use fallible_real_m, only: fallible_real_t
    use rojff, only: fallible_json_value_t, json_object_t, json_value_t
    use triangle_m, only: triangle_t

    implicit none
    private
    public :: fallible_triangle_t

    type :: fallible_triangle_t
        private
        type(error_list_t) :: errors_
        type(triangle_t) :: triangle_
    contains
        procedure :: failed
        procedure :: errors
        procedure :: triangle
    end type

    interface fallible_triangle_t
        module procedure from_fallible_triangle
        module procedure from_fallible_json
        module procedure from_json_value
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallible_triangle_m"
contains
    function from_fallible_triangle(maybe_triangle, module_, procedure_) result(fallible_triangle)
        type(fallible_triangle_t), intent(in) :: maybe_triangle
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_triangle_t) :: fallible_triangle

        if (maybe_triangle%failed()) then
            fallible_triangle%errors_ = error_list_t( &
                    maybe_triangle%errors_, module_, procedure_)
        else
            fallible_triangle%triangle_ = maybe_triangle%triangle_
        end if
    end function

    function from_fallible_json(maybe_json) result(fallible_triangle)
        type(fallible_json_value_t), intent(in) :: maybe_json
        type(fallible_triangle_t) :: fallible_triangle

        if (maybe_json%failed()) then
            fallible_triangle%errors_ = maybe_json%errors
        else
            fallible_triangle = fallible_triangle_t( &
                    fallible_triangle_t(maybe_json%value_), &
                    module_t(MODULE_NAME), &
                    procedure_t("from_fallible_json"))
        end if
    end function

    function from_json_value(json) result(fallible_triangle)
        class(json_value_t), intent(in) :: json
        type(fallible_triangle_t) :: fallible_triangle

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_object_t)
            fallible_triangle = fallible_triangle_t( &
                    from_json_object(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            fallible_triangle%errors_ = error_list_t(fatal_t( &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "Expected " // json%to_compact_string() // " to be a json object"))
        end select
    end function

    function from_json_object(json) result(fallible_triangle)
        type(json_object_t), intent(in) :: json
        type(fallible_triangle_t) :: fallible_triangle

        type(fallible_real_t) :: maybe_height, maybe_width

        maybe_height = fallible_real_t(json%get("height"))
        maybe_width = fallible_real_t(json%get("width"))
        if (any([maybe_height%failed(), maybe_width%failed()])) then
            fallible_triangle%errors_ = error_list_t( &
                    [maybe_height%errors(), maybe_width%errors()], &
                    module_t(MODULE_NAME), &
                    procedure_t("from_json_object"))
        else
            fallible_triangle%triangle_ = triangle_t( &
                    maybe_height%real_(), maybe_width%real_())
        end if
    end function

    pure function failed(self)
        class(fallible_triangle_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function errors(self)
        class(fallible_triangle_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function triangle(self)
        class(fallible_triangle_t), intent(in) :: self
        type(triangle_t) :: triangle

        triangle = self%triangle_
    end function
end module
