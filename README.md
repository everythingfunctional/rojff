# rojff: Return of JSON (for Fortran)

[![pipeline status](https://gitlab.com/everythingfunctional/rojff/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/rojff/commits/main)

With an interface inspired by [jsonff](https://gitlab.com/everythingfunctional/jsonff),
the data semantics and parser are redesigned to allow for high performance.

The constructors, `json_null_t`, `json_bool_t`, `json_integer_t`,
`json_number_t`, `fallible_json_string_t`, `json_array_t` and `json_object_t`
are provided for building up JSON data structures in Fortran.
Note that the procedure to create a `json_string_t` ensures that the string is valid according to the JSON standard.
*Unsafe* versions of the json string and member constructors are provided
in the event you are certain that you are only dealing with valid strings.

Additionally there are procedures which use `allocatable` arguments
to move data into a JSON data structure, in order to avoid copying data.
One should generally avoid using these procedures to construct the JSON
unless armed with specific evidence that using the functional constructors
is actually significantly impacting performance due to the data copying.

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

```Fortran
program example
    use iso_varying_string
    use jsonff
    use erloff

    implicit none

    type(fallible_json_value_t) :: parsed_json
    character(len=:), alloctable :: string

    parsed_json = parse_json_from_file('index.json')

    if (parsed_json%failed()) then
        call put_line(parsed_json%errors%to_string())
        error stop
    end if

    string = parsed_json%json%to_expanded_string()
end program example
```

A JSON document can be parsed from a string with the `parse_json_from_string` function
or by reading a file with the `parse_json_from_file` function.
Both functions return an instance of a `fallible_json_value_t` type
representing a union of an `error_list_t` and a `json_value_t`.
To verify the correctness of the parsed JSON the `fallible_json_value_t` can be checked with the `%failed()` method.
In case of failure the `error_list_t` component can be accessed via `%errors`
and should be handled appropriately by the caller.

If the JSON document was correct the `json_value_t` can be accessed via `%json` and processed further.
Serialization with the `%to_expanded_string()` or `%to_compact_string()` method returns a `character(len=:), allocatable`.

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

In most cases, you're going to have inputs in JSON format corresponding to derived types in your program.
The example in `example/custom_types` illustrates how this might be done for a triangle area calculator.
It allows for easy composition and reuse, while still handling all the possible errors that might occur with regards to erroneous input.
Any errors that occur trying to access a component of the JSON are propagated through.
Any mismatches in the expected types of the data are able to generate meaningful errors.
