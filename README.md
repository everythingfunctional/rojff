# rojff: Return of JSON (for Fortran)

[![pipeline status](https://gitlab.com/everythingfunctional/rojff/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/rojff/commits/main)

With an interface inspired by [jsonff](https://gitlab.com/everythingfunctional/jsonff),
the data semantics and parser are redesigned to allow for high performance.

A full set of procedures are provided for either combination of two orthogonal aspects of constructing a JSON data structure:

* Functional style constructors enabling single-expression construction **vs** move semantics to avoid data copying
* Check for possible errors **vs** avoid error checks

Functions with the same name as the type (or `unsafe` substituted for `_t` to indicate avoidance of error checks) are provided for the functional style.
Subroutines named `create_*` or `move_into_*` are provided for avoiding data copying.

No error checking is required for values of type `json_null_t`, `json_bool_t`, `json_integer_t`, or `json_number_t`, so no fallible types or unsafe procedures are provided for them.
Construction of these types done like:

functional | move
-----------|------
`json_null_t()` | `create_json_null(null_var)`
`json_bool_t(.true.)` | `create_json_bool(bool_var, .true.)`
`json_integer_t(1)` | `create_json_integer(int_var, 1)`
`json_number_t(3.14d0)` | `create_json_number(num_var, 3.14d0)`

The construction of a string can check that it is a valid json string (i.e. contains only proper escape sequences and no unescaped quotes).
The construction of an object can check that no duplicate keys are present.
The construction of strings, arrays and objects can be accomplished via the following different methods.

functional unsafe | function with errors | move unsafe | move with errors
------------------|----------------------|-------------|-----------------
N/A               | `fallible_json_value_t(val)` | N/A | `move_into_fallible_json_value(maybe_val, val)`
`json_string_unsafe(string)` | `fallible_json_string_t(string)` | `create_json_string_unsafe(str, string)` | `create_fallible_json_string(maybe_str, string)`
`json_element_t(val)` | `fallible_json_element_t(maybe_val)` | `move_into_element(elem, val)` | `move_into_fallible_element(maybe_elem, maybe_val)`
`json_array_t(elems)` | `fallible_json_array_t(maybe_elems)` | `move_into_array(array, elems)` | `move_into_fallible_array(maybe_array, maybe_elems)`
`json_member_unsafe(key, val)` | `fallible_json_member_t(key, val)` | `move_into_member_unsafe(member, key, val)` | `move_into_fallible_member(maybe_member, maybe_key, maybe_val)`
`json_object_unsafe(members)` | `fallible_json_object_t(maybe_members)` | `move_into_object(obj, members)` | `move_into_fallible_object(maybe_obj, maybe_members)`

Full examples of constructing the same data structure all four different ways are provided in the [construction method test](test/construction_method_test.f90).
It is recommended to start with the functional style with error checking.
If sufficient testing has been performed, and any strings will not come from user input, then switching to the unsafe methods can be considered.
If sufficient evidence has been seen that performance is a problem, then it may be beneficial to switch to the `create_*` and `move_*` methods.

Once constructed, JSON values can be converted to string representation in either compact or expanded, human-readable formats.
Additionally, procedures are provided (`parse_json_from_string` and `parse_json_from_file`)
that parse contents into a JSON data structure.
It also provides reasonable error messages in the event the contents do not contain valid JSON.

A string generated from a valid JSON data structure is guaranteed to be able to be parsed by the parser into exactly the same data structure.
Note that a data structure parsed from a string is not necessarily guaranteed to produce exactly the same string,
since formatting is not important to JSON data.

Using rojff
------------

This section provides some examples to use rojff in your project.
A detailed references on the functionality of rojff is available in
[the developer documentation](https://everythingfunctional.gitlab.io/rojff).


### Reading and writing a JSON

To create a simple reader and writer for JSON three steps are usually required.
A small and concise example to read JSON from a file and pretty print it is given here.

```fortran
program example
    use iso_varying_string, only: char
    use rojff

    implicit none

    type(fallible_json_value_t) :: parsed_json

    parsed_json = parse_json_from_file('index.json')

    if (parsed_json%failed()) then
        error stop char(parsed_json%errors%to_string())
    end if

    print *, parsed_json%value_%to_expanded_string()
end program
```

A JSON document can be parsed from a string with the `parse_json_from_string` function
or by reading a file with the `parse_json_from_file` function.
Both functions return an instance of a `fallible_json_value_t` type
representing a union of an `error_list_t` and a `json_value_t`.
To verify the correctness of the parsed JSON the `fallible_json_value_t` can be checked with the `%failed()` method.
In case of failure the `error_list_t` component can be accessed via `%errors`
and should be handled appropriately by the caller.

If the JSON document was correct the `json_value_t` can be accessed via `%value_` and processed further.
Serialization with the `%to_expanded_string()` or `%to_compact_string()` method returns a `character(len=:), allocatable`.
Serialization to a file can be performed with the `%save_expanded_to(file_name)` or `%save_compactly_to(file_name)` methods.
If there is a chance the file may already exist, the `status="replace"` optional argument should be included.

The *literal* value (e.g. null) is identified by its type alone and contains no data.
The scalar values (e.g. bool, string, integer and number) contain their values,
which can be accessed via `%bool`, `%string`, and `%number` respectively.
The composite values (e.g. object and array), contain zero or more elements, which can be accessed via the `get` type bound procedures.
Retrieving an element from an object requires a string (the key), and from an array requires an integer (the index).
Both procedures return a `fallible_json_value_t`, as it would be possible to ask for an element that does not exist.
Additionally, one can access the whole array of values from a json array as an array of `json_element_t` objects via `%elements`.
One can get arrays of keys (as `varying_string`s) and values (as `json_element_t`s) from an object,
which are guaranteed to be returned in matching order, via `get_keys` and `get_values`.

### Real World Usage For Inputs

An example of parsing a JSON string and retrieving a value by its key can be found in `example/get_value`. It also includes basic error handling.

In most cases, however, you're going to have inputs in JSON format corresponding to derived types in your program.
The example in `example/custom_types` illustrates how this might be done for a triangle area calculator.
It allows for easy composition and reuse, while still handling all the possible errors that might occur with regards to erroneous input.
Any errors that occur trying to access a component of the JSON are propagated through.
Any mismatches in the expected types of the data are able to generate meaningful errors.
