program example_main
    use fparse
    implicit none

    type(schema_type) :: schema
    type(input_tree_type) :: tree
    character(len=:), allocatable :: system_name
    integer :: nelecs
    real(kind=8) :: dr, rmax
    integer :: meshr
    character(len=:), allocatable :: mode
    character(len=:), allocatable :: flags(:)
    character(len=:), allocatable :: terms(:)
    integer :: nflags
    integer :: nterms
    logical :: mode_from_input
    real(kind=8) :: zval
    real(kind=8) :: cutoff
    integer, allocatable :: levels(:)
    integer :: nlevels
    logical :: use_cache

    call init_schema(schema)
    call build_schema(schema)

    call parse_input(schema, 'examples/example_input.in', tree, 6)

    call get_string(schema, tree, 'target', 'system', system_name, 6)
    call get_integer(schema, tree, 'target', 'nelecs', nelecs, 6)
    call get_real(schema, tree, 'target', 'z', zval, 6)
    call get_string_list(schema, tree, 'target', 'terms', terms, nterms, 6)

    call get_real(schema, tree, 'grid', 'dr', dr, 6)
    call get_real(schema, tree, 'grid', 'rmax', rmax, 6)
    call get_integer(schema, tree, 'grid', 'meshr', meshr, 6)

    mode_from_input = has_key(tree, 'options', 'mode')
    call get_string(schema, tree, 'options', 'mode', mode, 6)
    call get_logical(schema, tree, 'options', 'use_cache', use_cache, 6)
    call get_string_list(schema, tree, 'options', 'flags', flags, nflags, 6)
    call get_real(schema, tree, 'settings', 'cutoff', cutoff, 6)
    call get_integer_list(schema, tree, 'settings', 'levels', levels, nlevels, 6)

    write(*,*) 'system: ', trim(system_name)
    write(*,*) 'nelecs: ', nelecs
    write(*,*) 'z: ', zval
    write(*,*) 'dr: ', dr
    write(*,*) 'rmax: ', rmax
    write(*,*) 'meshr: ', meshr
    if (mode_from_input) then
        write(*,*) 'mode: ', trim(mode), ' (input)'
    else
        write(*,*) 'mode: ', trim(mode), ' (default)'
    endif
    write(*,*) 'use_cache: ', use_cache
    write(*,*) 'flags: ', nflags
    write(*,*) 'cutoff: ', cutoff
    write(*,*) 'levels: ', nlevels
    write(*,*) 'terms: ', nterms

contains

    !--------------------------------------------------------------------------
    ! build_schema
    !
    ! purpose: define the schema for the example input file.
    !
    ! inputs:
    !          1. schema
    !
    ! outputs:
    !          1. schema
    !--------------------------------------------------------------------------
    subroutine build_schema(schema)
        implicit none
        type(schema_type), intent(inout) :: schema

        call add_block(schema, 'TARGET', .true.)
        call add_key(schema, 'TARGET', 'system', .true., .false., '')
        call add_key(schema, 'TARGET', 'nelecs', .true., .false., '')
        call add_key(schema, 'TARGET', 'z', .false., .true., '0.0')
        call add_key(schema, 'TARGET', 'terms', .false., .false., '')

        call add_block(schema, 'GRID', .true.)
        call add_key(schema, 'GRID', 'dr', .true., .false., '')
        call add_key(schema, 'GRID', 'rmax', .true., .false., '')
        call add_key(schema, 'GRID', 'meshr', .true., .false., '')

        call add_block(schema, 'OPTIONS', .false.)
        call add_key(schema, 'OPTIONS', 'mode', .false., .true., 'standard')
        call add_key(schema, 'OPTIONS', 'flags', .false., .false., '')
        call add_key(schema, 'OPTIONS', 'use_cache', .false., .true., 'off')

        call add_block(schema, 'SETTINGS', .false.)
        call add_key(schema, 'SETTINGS', 'cutoff', .false., .true., '1.0e-6')
        call add_key(schema, 'SETTINGS', 'levels', .false., .false., '')

    end subroutine build_schema

end program example_main
