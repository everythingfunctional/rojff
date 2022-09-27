submodule(rojff_json_number_m) rojff_json_number_s
    use rojff_utils_m, only: is_nan, to_string

    implicit none
contains
    module procedure constructor
        json_number%number = number
        if (present(precision)) then
            json_number%precision = precision
            json_number%precision_provided = .true.
        else
            json_number%precision = 0
            json_number%precision_provided = .false.
        end if
    end procedure

    module procedure create_json_number
        allocate(json)
        json%number = number
        if (present(precision)) then
            json%precision = precision
            json%precision_provided = .true.
        else
            json%precision_provided = .false.
        end if
    end procedure

    module procedure equals
        select type (rhs)
        type is (json_number_t)
            if (is_nan(lhs%number) .and. is_nan(rhs%number)) then
                equals = .true.
            else if (is_nan(lhs%number)) then
                equals = .false.
            else if (is_nan(rhs%number)) then
                equals = .false.
            else
                ! avoids warnings about comparing floating point numbers
                equals = .not. (lhs%number < rhs%number .or. lhs%number > rhs%number)
            end if
        class default
            equals = .false.
        end select
    end procedure

    module procedure write_to_compactly
        if (self%precision_provided) then
            call sink%append(to_string(self%number, self%precision))
        else
            call sink%append(to_string(self%number))
        end if
    end procedure

    module procedure write_to_expanded
        associate(unused => indentation_level); end associate
        call self%write_to_compactly(sink)
    end procedure
end submodule