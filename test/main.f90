! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use json_test, only: &
                json_json => test_json
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = json_json()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
