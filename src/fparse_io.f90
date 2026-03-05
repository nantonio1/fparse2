module fparse_io
    use fparse_util, only: strip_comment, &
                           is_blank, &
                           split_first_token, &
                           normalize_name, &
                           is_absolute_path, &
                           get_dirname, &
                           join_path
    implicit none

    integer, parameter :: max_include_depth = 20

contains

    !--------------------------------------------------------------------------
    ! load_file_lines
    !
    ! purpose: load lines from a file and expand @include directives.
    !
    ! inputs:
    !          1. filename
    !          2. ounit
    !
    ! outputs:
    !          1. lines
    !          2. sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    subroutine load_file_lines(filename, lines, sources, line_nums, ounit)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: lines(:)
        character(len=:), allocatable, intent(out) :: sources(:)
        integer, allocatable, intent(out) :: line_nums(:)
        integer, intent(in) :: ounit
        !
        character(len=:), allocatable :: stack(:)
        integer :: depth

        allocate(character(len=1) :: lines(0))
        allocate(character(len=1) :: sources(0))
        allocate(line_nums(0))
        allocate(character(len=1) :: stack(0))

        depth = 0

        call load_file_recursive(filename, lines, sources, line_nums, stack, depth, ounit)

    end subroutine load_file_lines


    !--------------------------------------------------------------------------
    ! load_file_recursive
    !
    ! purpose: recursive include expansion for input files.
    !
    ! inputs:
    !          1. filename
    !          2. ounit
    !
    ! outputs:
    !          1. lines
    !          2. sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    recursive subroutine load_file_recursive(filename, lines, sources, &
        line_nums, stack, depth, ounit)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(inout) :: lines(:)
        character(len=:), allocatable, intent(inout) :: sources(:)
        integer, allocatable, intent(inout) :: line_nums(:)
        character(len=:), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        integer, intent(in) :: ounit
        !
        integer :: unit, io, line_num
        character(len=1024) :: raw
        character(len=:), allocatable :: code
        character(len=:), allocatable :: token
        character(len=:), allocatable :: rest
        character(len=:), allocatable :: inc_file
        character(len=:), allocatable :: dirname

        if (depth >= max_include_depth) then
            write(ounit,*) 'error: include depth exceeded for ', trim(filename)
            stop
        endif

        if (is_in_stack(filename, stack)) then
            write(ounit,*) 'error: include loop detected at ', trim(filename)
            stop
        endif

        call push_stack(stack, filename)
        depth = depth + 1

        open(newunit=unit, file=trim(filename), status='old', iostat=io)
        if (io /= 0) then
            write(ounit,*) 'error: cannot open file ', trim(filename)
            stop
        endif

        dirname = get_dirname(trim(filename))
        io = 0
        line_num = 0
        do while (io == 0)
            read(unit,'(A)', iostat=io) raw
            if (io /= 0) exit
            line_num = line_num + 1

            call strip_comment(raw, code)
            if (is_blank(code)) cycle

            call split_first_token(code, token, rest)
            if (normalize_name(token) == '@INCLUDE' .or. &
                normalize_name(token) == 'INCLUDE') then
                if (len_trim(rest) == 0) then
                    write(ounit,*) 'error: missing include filename at ', &
                        trim(filename), ':', line_num
                    stop
                endif
                if (is_absolute_path(rest)) then
                    inc_file = trim(rest)
                else
                    inc_file = join_path(dirname, trim(rest))
                endif
                call load_file_recursive(inc_file, lines, sources, &
                    line_nums, stack, depth, ounit)
            else
                call append_line(lines, sources, line_nums, &
                    trim(code), trim(filename), line_num)
            endif
        enddo
        close(unit)

        depth = depth - 1
        call pop_stack(stack)

    end subroutine load_file_recursive


    !--------------------------------------------------------------------------
    ! append_line
    !
    ! purpose: append a line and metadata to arrays.
    !
    ! inputs:
    !          1. line
    !          2. source
    !          3. line_num
    !
    ! outputs:
    !          1. lines
    !          2. sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    subroutine append_line(lines, sources, line_nums, line, source, line_num)
        implicit none
        character(len=:), allocatable, intent(inout) :: lines(:)
        character(len=:), allocatable, intent(inout) :: sources(:)
        integer, allocatable, intent(inout) :: line_nums(:)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: source
        integer, intent(in) :: line_num
        !
        character(len=:), allocatable :: tmp_lines(:)
        character(len=:), allocatable :: tmp_sources(:)
        integer, allocatable :: tmp_nums(:)
        integer :: n
        integer :: len_lines, len_sources
        integer :: new_len_lines, new_len_sources

        n = size(lines)
        len_lines = 0
        len_sources = 0
        if (n > 0) then
            len_lines = len(lines(1))
            len_sources = len(sources(1))
        endif
        new_len_lines = max(1, max(len_lines, len_trim(line)))
        new_len_sources = max(1, max(len_sources, len_trim(source)))
        allocate(character(len=new_len_lines) :: tmp_lines(n+1))
        allocate(character(len=new_len_sources) :: tmp_sources(n+1))
        allocate(tmp_nums(n+1))

        if (n > 0) then
            tmp_lines(1:n) = lines
            tmp_sources(1:n) = sources
            tmp_nums(1:n) = line_nums
        endif
        tmp_lines(n+1) = line
        tmp_sources(n+1) = source
        tmp_nums(n+1) = line_num

        call move_alloc(tmp_lines, lines)
        call move_alloc(tmp_sources, sources)
        call move_alloc(tmp_nums, line_nums)

    end subroutine append_line


    !--------------------------------------------------------------------------
    ! push_stack
    !
    ! purpose: push a filename onto include stack.
    !
    ! inputs:
    !          1. stack
    !          2. filename
    !
    ! outputs:
    !          1. stack
    !--------------------------------------------------------------------------
    subroutine push_stack(stack, filename)
        implicit none
        character(len=:), allocatable, intent(inout) :: stack(:)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: tmp(:)
        integer :: n

        n = size(stack)
        if (n > 0) then
            allocate(character(len=max(1, max(len(stack(1)), &
                len_trim(filename)))) :: tmp(n+1))
        else
            allocate(character(len=max(1, len_trim(filename))) :: tmp(n+1))
        endif
        if (n > 0) then
            tmp(1:n) = stack
        endif
        tmp(n+1) = trim(filename)
        call move_alloc(tmp, stack)

    end subroutine push_stack


    !--------------------------------------------------------------------------
    ! pop_stack
    !
    ! purpose: pop the top of the include stack.
    !
    ! inputs:
    !          1. stack
    !
    ! outputs:
    !          1. stack
    !--------------------------------------------------------------------------
    subroutine pop_stack(stack)
        implicit none
        character(len=:), allocatable, intent(inout) :: stack(:)
        !
        character(len=:), allocatable :: tmp(:)
        integer :: n

        n = size(stack)
        if (n <= 1) then
            allocate(character(len=1) :: tmp(0))
        else
            allocate(character(len=len(stack(1))) :: tmp(n-1))
            tmp = stack(1:n-1)
        endif
        call move_alloc(tmp, stack)
    end subroutine pop_stack


    !--------------------------------------------------------------------------
    ! is_in_stack
    !
    ! purpose: check if a filename is in the include stack.
    !
    ! inputs:
    !          1. filename
    !          2. stack
    !
    ! outputs:
    !          1. is_in_stack
    !--------------------------------------------------------------------------
    logical function is_in_stack(filename, stack)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(in) :: stack(:)
        !
        integer :: i

        is_in_stack = .false.
        do i = 1, size(stack)
            if (trim(stack(i)) == trim(filename)) then
                is_in_stack = .true.
                return
            endif
        enddo

    end function is_in_stack

end module fparse_io
