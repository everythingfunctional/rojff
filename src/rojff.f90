module rojff
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, rojff!"
  end subroutine say_hello
end module rojff
