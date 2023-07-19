program MainProgram
    use PipeModule
    implicit none

    character(len=17) :: pipe_location = "../priv/pipe_file"
! dscal: int n, d da, d dx, int incx
    integer :: n=3, incx=1, pipe, discard=1
    double precision :: da = 2, dx(3) = [1,2,3]

    print *, "Init: ", n, da, dx, incx

    call pipe_open_write(pipe_location, pipe)
    call pipe_write_characters("dscal", pipe)
    call pipe_write_integer(n, pipe)
    call pipe_write_double(da, pipe)
    call pipe_write_doubles(dx, pipe)
    call pipe_write_integer(incx, pipe)
    call pipe_write_integer(discard, pipe)
    call pipe_close(pipe)

    call pipe_open_read(pipe_location, pipe)
    call pipe_skip(pipe)
    call pipe_read_integer(n, pipe)
    call pipe_read_double(da, pipe)
    call pipe_read_doubles(dx, pipe)
    call pipe_read_integer(incx, pipe)
    call pipe_read_integer(discard, pipe)
    call pipe_close(pipe)

    print *, "Expected da = da * 2"
    print *, "End: ", n, da, dx, incx


end program MainProgram