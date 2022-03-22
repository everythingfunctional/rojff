program main
    use erloff, only: error_list_t
    use fallible_triangle_m, only: fallible_triangle_t
    use iso_fortran_env, only: error_unit, output_unit
    use iso_varying_string, only: operator(//), put_line
    use rojff, only: parse_json_from_file
    use strff, only: to_string
    use triangle_m, only: triangle_t

    implicit none

    type(fallible_triangle_t) :: maybe_triangle

    maybe_triangle = fallible_triangle_t( &
            parse_json_from_file("example/custom_types/input.json"))
    if (maybe_triangle%failed()) then
        block
            type(error_list_t) :: errors
            errors = maybe_triangle%errors()
            call put_line(error_unit, errors%to_string())
        end block
    else
        block
            type(triangle_t) :: triangle
            triangle = maybe_triangle%triangle()
            call put_line( &
                    output_unit, &
                    "The area of the triangle is: " // to_string(triangle%area()))
        end block
    end if
end program
