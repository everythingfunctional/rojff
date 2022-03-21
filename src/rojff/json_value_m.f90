module rojff_json_value_m
    use rojff_file_sink_m, only: file_sink_t
    use rojff_string_builder_m, only: string_builder_t
    use rojff_string_sink_m, only: string_sink_t

    implicit none
    private
    public :: json_value_t

    type, abstract :: json_value_t
    contains
        procedure(equals_i), deferred :: equals
        generic :: operator(==) => equals
        procedure(write_to_compactly_i), deferred :: write_to_compactly
        procedure(write_to_expanded_i), deferred :: write_to_expanded
        procedure :: to_compact_string
        procedure :: save_compactly_to
        procedure :: to_expanded_string
        procedure :: save_expanded_to
    end type

    abstract interface
        elemental function equals_i(lhs, rhs) result(equals)
            import :: json_value_t

            implicit none

            class(json_value_t), intent(in) :: lhs, rhs
            logical :: equals
        end function

        subroutine write_to_compactly_i(self, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            class(string_sink_t), intent(inout) :: sink
        end subroutine

        subroutine write_to_expanded_i(self, indentation_level, sink)
            import :: json_value_t, string_sink_t

            implicit none

            class(json_value_t), intent(in) :: self
            integer, intent(in) :: indentation_level
            class(string_sink_t), intent(inout) :: sink
        end subroutine
    end interface
contains
    function to_compact_string(self) result(string)
        class(json_value_t), intent(in) :: self
        character(len=:), allocatable :: string

        type(string_builder_t) :: sink
        character(len=:), allocatable :: string_

        call self%write_to_compactly(sink)
        call sink%move_into(string_)
        call move_alloc(string_, string)
    end function

    subroutine save_compactly_to(self, file, status, iostat, iomsg)
        class(json_value_t), intent(in) :: self
        character(len=*), intent(in) :: file
        character(len=*), optional, intent(in) :: status
        integer, optional, intent(out) :: iostat
        character(len=:), allocatable, optional, intent(out) :: iomsg

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
                    open( &
                            newunit=unit, &
                            file=file, &
                            status=status, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
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
                    open( &
                            newunit=unit, &
                            file=file, &
                            iomsg=iomsg, &
                            action="WRITE", &
                            access="STREAM", &
                            form="FORMATTED")
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
    end subroutine

    function to_expanded_string(self) result(string)
        class(json_value_t), intent(in) :: self
        character(len=:), allocatable :: string

        type(string_builder_t) :: sink
        character(len=:), allocatable :: string_

        call self%write_to_expanded(0, sink)
        call sink%move_into(string_)
        call move_alloc(string_, string)
    end function

    subroutine save_expanded_to(self, file, status, iostat, iomsg)
        class(json_value_t), intent(in) :: self
        character(len=*), intent(in) :: file
        character(len=*), optional, intent(in) :: status
        integer, optional, intent(out) :: iostat
        character(len=:), allocatable, optional, intent(out) :: iomsg

        type(file_sink_t) :: sink
        integer :: unit

        if (present(status)) then
            if (present(iostat)) then
                if (present(iomsg)) then
                    open(newunit = unit, file = file, status=status, iostat=iostat, iomsg=iomsg, action="WRITE")
                else
                    open(newunit = unit, file = file, status=status, iostat=iostat, action="WRITE")
                end if
            else
                if (present(iomsg)) then
                    open(newunit = unit, file = file, status=status, iomsg=iomsg, action="WRITE")
                else
                    open(newunit = unit, file = file, status=status, action="WRITE")
                end if
            end if
        else
            if (present(iostat)) then
                if (present(iomsg)) then
                    open(newunit = unit, file = file, iostat=iostat, iomsg=iomsg, action="WRITE")
                else
                    open(newunit = unit, file = file, iostat=iostat, action="WRITE")
                end if
            else
                if (present(iomsg)) then
                    open(newunit = unit, file = file, iomsg=iomsg, action="WRITE")
                else
                    open(newunit = unit, file = file, action="WRITE")
                end if
            end if
        end if
        sink = file_sink_t(unit)
        call self%write_to_expanded(0, sink)
        close(unit, status="KEEP")
    end subroutine
end module
