!> Simple reader for JSON files
program json_cat
    use erloff, only: error_list_t
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use iso_varying_string, only : put_line, char
    use rojff, only: parse_json_from_string, fallible_json_value_t, json_value_t
    use strff, only: read_file

    implicit none
    type(fallible_json_value_t) :: json
    character(len=:), allocatable :: argument
    integer :: iarg, narg

    narg = command_argument_count()
    if (narg == 0) then
        error stop "Please provide a JSON file as command line argument"
    end if

    do iarg = 1, narg
        call get_argument(iarg, argument)

        write(error_unit, '("#", *(1x, g0))') "Reading from", argument
        json = parse_json_from_string(char(read_file(argument)))
        if (json%failed()) then
            call put_line(error_unit, json%errors%to_string())
            cycle
        end if

        call put_line(output_unit, json%json%to_compact_string())
    end do

contains

    !> Obtain the command line argument at a given index
    subroutine get_argument(idx, arg)
        !> Index of command line argument, range [0:command_argument_count()]
        integer, intent(in) :: idx
        !> Command line argument
        character(len=:), allocatable, intent(out) :: arg

        integer :: length, stat

        call get_command_argument(idx, length=length, status=stat)
        if (stat /= 0) then
            return
        endif

        allocate(character(len=length) :: arg, stat=stat)
        if (stat /= 0) then
            return
        endif

        if (length > 0) then
            call get_command_argument(idx, arg, status=stat)
            if (stat /= 0) then
                deallocate(arg)
                return
            end if
        end if

    end subroutine get_argument

end program json_cat
