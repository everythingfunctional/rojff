submodule(rojff_json_member_m) rojff_json_member_s
    use iso_varying_string, only: char

    implicit none
contains
    module procedure deleted
        associate(unused => key); end associate
        associate(unused => value_); end associate
        error stop "Do not use the intrinsic structure constructor!"
    end procedure

    module procedure construct_v
        json_member%key = key%string
        json_member%value_ = value_
    end procedure

    module procedure construct_e
        json_member%key = key%string
        json_member%value_ = element%json
    end procedure

    module procedure json_member_unsafe_cv
        json_member%key = key
        json_member%value_ = value_
    end procedure

    module procedure json_member_unsafe_ce
        json_member%key = key
        json_member%value_ = element%json
    end procedure

    module procedure json_member_unsafe_sv
        json_member%key = char(key)
        json_member%value_ = value_
    end procedure

    module procedure json_member_unsafe_se
        json_member%key = char(key)
        json_member%value_ = element%json
    end procedure

    module procedure move_into_member_v
        call move_alloc(key%string, member%key)
        deallocate(key)
        call move_alloc(value_, member%value_)
    end procedure

    module procedure move_into_member_e
        call move_alloc(key%string, member%key)
        call move_alloc(element%json, member%value_)
    end procedure

    module procedure move_into_member_unsafe_cv
        member%key = key
        call move_alloc(value_, member%value_)
    end procedure

    module procedure move_into_member_unsafe_sv
        member%key = char(key)
        call move_alloc(value_, member%value_)
    end procedure

    module procedure move_into_member_unsafe_ce
        member%key = key
        call move_alloc(element%json, member%value_)
    end procedure

    module procedure move_into_member_unsafe_se
        member%key = char(key)
        call move_alloc(element%json, member%value_)
    end procedure

    module procedure equals
        equals = &
                lhs%key == rhs%key &
                .and. len(lhs%key) == len(rhs%key) &
                .and. lhs%value_ == rhs%value_
    end procedure

    module procedure write_to_compactly
        call sink%append('"')
        call sink%append(self%key)
        call sink%append('":')
        call self%value_%write_to_compactly(sink)
    end procedure

    module procedure write_to_expanded
        call sink%append('"' // self%key // '" : ')
        call self%value_%write_to_expanded(indentation_level, sink)
    end procedure
end submodule