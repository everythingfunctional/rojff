program expander
    !! parse json from first file, and write it pretty-printed to the second
    use, intrinsic :: iso_fortran_env, only: error_unit, int64, real64
    use iso_varying_string, only: put_line
    use rojff, only: parse_json_from_file, fallible_json_value_t

    implicit none

    type(fallible_json_value_t) :: json
    character(len=:), allocatable :: input_file, output_file
    integer :: narg
    integer(int64) :: start, finish
    real(real64) :: count_rate

    narg = command_argument_count()
    if (narg /= 2) then
        error stop "Please provide input and output files as command line arguments"
    end if

    call get_argument(1, input_file)
    call get_argument(2, output_file)

    call system_clock(start, count_rate)
    json = parse_json_from_file(input_file)
    call system_clock(finish)
    if (json%failed()) then
        call put_line(error_unit, json%errors%to_string())
    else
        print *, "Parsed in ", (finish-start)/real(count_rate), " seconds"
        call system_clock(start)
        call json%json%save_expanded_to(output_file, status="REPLACE")
        call system_clock(finish)
        print *, "Saved in ", (finish-start)/real(count_rate), " seconds"
    end if

contains
    subroutine get_argument(idx, arg)
        !! Obtain the command line argument at a given index
        integer, intent(in) :: idx !! Index of command line argument, range [0:command_argument_count()]
        character(len=:), allocatable, intent(out) :: arg !! Command line argument

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
end program
