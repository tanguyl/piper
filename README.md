piper
=====

This application "pipes" data from fortran to erlang. The goal is to execute the test library of lapack, but executing the LAPACK functions in Erlang's BLAS library, and hence testing if the wrapper is correct.

To communicate, any fortran program needs to sends data to the named pipe priv/pipe_file (created via mkfifo) using the fortran/pipe_module.f90. If the application is running, it will read the request, execute the matching blas, and execute back the request.

Build
-----

    $ rebar3 compile
