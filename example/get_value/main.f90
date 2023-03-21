!> This example program parses a JSON string and prints the value of the "hello" key.
program main
  use rojff
  use iso_varying_string

  implicit none

  type(fallible_json_value_t) :: json, result

  json = parse_json_from_string('{ "hello": "World!"}')

  if (json%failed()) then
    print *, char(json%errors%to_string()); stop 1
  end if

  select type (obj => json%value_)
  type is (json_object_t)
    result = obj%get('hello')

    if (result%failed()) then
      print *, char(result%errors%to_string()); stop 1
    end if

    select type (str => result%value_)
    type is (json_string_t)
      print *, 'Hello, ', str%string
    class default
      print *, 'Not a string: ', str%to_compact_string(); stop 1
    end select

  class default
    print *, 'Not an object: ', obj%to_compact_string(); stop 1
  end select

end program
