module fparse
    use fparse_types
    use fparse_schema, only: init_schema, add_block, add_key
    use fparse_parser, only: parse_input
    use fparse_getters, only: get_string, &
                              get_integer, &
                              get_real, &
                              get_logical, &
                              get_integer_list, &
                              get_real_list, &
                              get_string_list, &
                              has_block, &
                              has_key
    implicit none

    private

    public :: schema_type, input_tree_type
    public :: init_schema, add_block, add_key
    public :: parse_input
    public :: get_string, get_integer, get_real, get_logical
    public :: get_integer_list, get_real_list, get_string_list
    public :: has_block, has_key

end module fparse
