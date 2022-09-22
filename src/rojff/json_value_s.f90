submodule(rojff_json_value_m) rojff_json_value_s
    use rojff_file_sink_m, only: file_sink_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
contains
    module procedure to_compact_string
        type(string_sink_t) :: sink

        call self%write_to_compactly(sink)
        call sink%move_into(string)
    end procedure

    module procedure save_compactly_to
        type(file_sink_t) :: sink
        integer :: unit

        if (present(status)) then
            if (present(iostat)) then
                if (present(iomsg)) then
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            iostat=iostat, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            iostat=iostat, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            else
                if (present(iomsg)) then
                    error stop "IOMSG= has no effect without IOSTAT="
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            end if
        else
            if (present(iostat)) then
                if (present(iomsg)) then
                    open( &
                            newunit=unit, &
                            file=file, &
                            iostat=iostat, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            iostat=iostat, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            else
                if (present(iomsg)) then
                    error stop "IOMSG= has no effect without IOSTAT="
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            end if
        end if
        sink = file_sink_t(unit)
        call self%write_to_compactly(sink)
        close(unit, status="KEEP")
    end procedure

    module procedure to_expanded_string
        type(string_sink_t) :: sink

        call self%write_to_expanded(0, sink)
        call sink%move_into(string)
    end procedure

    module procedure save_expanded_to
        type(file_sink_t) :: sink
        integer :: unit

        if (present(status)) then
            if (present(iostat)) then
                if (present(iomsg)) then
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            iostat=iostat, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            iostat=iostat, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            else
                if (present(iomsg)) then
                    error stop "IOMSG= has no effect without IOSTAT="
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            end if
        else
            if (present(iostat)) then
                if (present(iomsg)) then
                    open( &
                            newunit=unit, &
                            file=file, &
                            iostat=iostat, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            iostat=iostat, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            else
                if (present(iomsg)) then
                    error stop "IOMSG= has no effect without IOSTAT="
                else
                    open( &
                            newunit=unit, &
                            file=file, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
                end if
            end if
        end if
        sink = file_sink_t(unit)
        call self%write_to_expanded(0, sink)
        close(unit, status="KEEP")
    end procedure
end submodule