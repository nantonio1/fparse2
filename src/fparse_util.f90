module fparse_util
    implicit none
    character(len=1), parameter :: backslash_char = achar(92)

contains

    !--------------------------------------------------------------------------
    ! normalize_name
    !
    ! purpose: return trimmed, left-adjusted, uppercase name.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function normalize_name(input) result(output)
        implicit none
        character(len=*), intent(in) :: input
        !
        character(len=:), allocatable :: output
        character(len=:), allocatable :: tmp
        integer :: i

        tmp = adjustl(trim(input))
        allocate(character(len=len(tmp)) :: output)
        output = tmp
        do i = 1, len(output)
            if (output(i:i) >= 'a' .and. output(i:i) <= 'z') then
                output(i:i) = achar(iachar(output(i:i)) - 32)
            endif
        enddo

    end function normalize_name


    !--------------------------------------------------------------------------
    ! normalize_key
    !
    ! purpose: strip brackets if present and normalize to uppercase.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function normalize_key(input) result(output)
        implicit none
        character(len=*), intent(in) :: input
        !
        character(len=:), allocatable :: output
        character(len=:), allocatable :: tmp
        integer :: n

        tmp = adjustl(trim(input))
        n = len(tmp)
        if (n >= 2) then
            if (tmp(1:1) == '[' .and. tmp(n:n) == ']') then
                tmp = tmp(2:n-1)
            endif
        endif
        output = normalize_name(tmp)

    end function normalize_key


    !--------------------------------------------------------------------------
    ! strip_comment
    !
    ! purpose: remove comment starting with # and trim.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. code
    !--------------------------------------------------------------------------
    subroutine strip_comment(line, code)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: code
        !
        integer :: pos

        pos = index(line, '#')
        if (pos > 0) then
            code = trim(line(1:pos-1))
        else
            code = trim(line)
        endif

    end subroutine strip_comment


    !--------------------------------------------------------------------------
    ! is_blank
    !
    ! purpose: check if a string is empty after trimming.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. is_blank
    !--------------------------------------------------------------------------
    logical function is_blank(line)
        implicit none
        character(len=*), intent(in) :: line

        is_blank = (len_trim(line) == 0)

    end function is_blank


    !--------------------------------------------------------------------------
    ! ends_with_backslash
    !
    ! purpose: check if a trimmed line ends with a backslash.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. ends_with_backslash
    !--------------------------------------------------------------------------
    logical function ends_with_backslash(line)
        implicit none
        character(len=*), intent(in) :: line
        !
        integer :: n

        n = len_trim(line)
        if (n <= 0) then
            ends_with_backslash = .false.
        else
            ends_with_backslash = (line(n:n) == backslash_char)
        endif

    end function ends_with_backslash


    !--------------------------------------------------------------------------
    ! remove_trailing_backslash
    !
    ! purpose: remove a trailing backslash from a trimmed line.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function remove_trailing_backslash(line) result(output)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: output
        !
        integer :: n

        n = len_trim(line)
        if (n <= 0) then
            output = ''
        else
            if (line(n:n) == backslash_char) then
                output = trim(line(1:n-1))
            else
                output = trim(line)
            endif
        endif

    end function remove_trailing_backslash


    !--------------------------------------------------------------------------
    ! split_first_token
    !
    ! purpose: split a line into first token and remainder.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. token
    !          2. rest
    !--------------------------------------------------------------------------
    subroutine split_first_token(line, token, rest)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: token
        character(len=:), allocatable, intent(out) :: rest
        !
        integer :: pos
        character(len=:), allocatable :: tmp

        tmp = adjustl(trim(line))
        pos = index(tmp, ' ')
        if (pos == 0) then
            token = tmp
            rest = ''
        else
            token = tmp(1:pos-1)
            rest = adjustl(trim(tmp(pos+1:)))
        endif

    end subroutine split_first_token


    !--------------------------------------------------------------------------
    ! split_tokens
    !
    ! purpose: split a string into space-separated tokens.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. tokens
    !          2. ntokens
    !--------------------------------------------------------------------------
    subroutine split_tokens(input, tokens, ntokens)
        implicit none
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: ntokens
        !
        integer :: i, n, start, finish
        character(len=:), allocatable :: str

        str = adjustl(trim(input))
        n = len_trim(str)
        if (n == 0) then
            allocate(character(len=1) :: tokens(0))
            ntokens = 0
            return
        endif

        ntokens = 0
        start = 1
        do while (start <= n)
            finish = index(str(start:), ' ')
            if (finish == 0) then
                ntokens = ntokens + 1
                exit
            else
                ntokens = ntokens + 1
                start = start + finish
                do while (start <= n .and. str(start:start) == ' ')
                    start = start + 1
                enddo
            endif
        enddo

        allocate(character(len=len(str)) :: tokens(ntokens))
        ntokens = 0
        start = 1
        do while (start <= n)
            finish = index(str(start:), ' ')
            if (finish == 0) then
                ntokens = ntokens + 1
                tokens(ntokens) = str(start:n)
                exit
            else
                ntokens = ntokens + 1
                tokens(ntokens) = str(start:start+finish-2)
                start = start + finish
                do while (start <= n .and. str(start:start) == ' ')
                    start = start + 1
                enddo
            endif
        enddo

    end subroutine split_tokens


    !--------------------------------------------------------------------------
    ! is_absolute_path
    !
    ! purpose: check if a path is absolute.
    !
    ! inputs:
    !          1. path
    !
    ! outputs:
    !          1. is_absolute_path
    !--------------------------------------------------------------------------
    logical function is_absolute_path(path)
        implicit none
        character(len=*), intent(in) :: path

        if (len_trim(path) == 0) then
            is_absolute_path = .false.
        else
            is_absolute_path = (path(1:1) == '/')
        endif

    end function is_absolute_path


    !--------------------------------------------------------------------------
    ! get_dirname
    !
    ! purpose: return directory of a file path.
    !
    ! inputs:
    !          1. path
    !
    ! outputs:
    !          1. dirname
    !--------------------------------------------------------------------------
    function get_dirname(path) result(dirname)
        implicit none
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: dirname
        integer :: i

        i = len_trim(path)
        do while (i > 0)
            if (path(i:i) == '/') exit
            i = i - 1
        enddo

        if (i <= 0) then
            dirname = '.'
        else
            dirname = path(1:i-1)
            if (len_trim(dirname) == 0) then
                dirname = '/'
            endif
        endif

    end function get_dirname


    !--------------------------------------------------------------------------
    ! join_path
    !
    ! purpose: join directory and filename into a path.
    !
    ! inputs:
    !          1. dirname
    !          2. filename
    !
    ! outputs:
    !          1. path
    !--------------------------------------------------------------------------
    function join_path(dirname, filename) result(path)
        implicit none
        character(len=*), intent(in) :: dirname
        character(len=*), intent(in) :: filename
        !
        character(len=:), allocatable :: path
        character(len=:), allocatable :: d

        d = trim(dirname)
        if (d == '/' .or. d == '') then
            path = trim(d) // trim(filename)
        else
            path = trim(d) // '/' // trim(filename)
        endif

    end function join_path

end module fparse_util
