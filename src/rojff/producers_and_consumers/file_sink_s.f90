submodule(rojff_file_sink_m) rojff_file_sink_s
    implicit none
contains
    module procedure constructor
        file_sink%unit = unit
    end procedure

    module procedure append
        write(self%unit, '(A)', advance='NO') part
    end procedure
end submodule