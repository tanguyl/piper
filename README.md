piper
=====

This application "pipes" data from fortran to erlang. The goal is to execute the test library of lapack, but executing the LAPACK functions in Erlang's BLAS library, and hence testing if the wrapper is correct.

To communicate, any fortran program needs to sends data to the named pipe priv/pipe_file (created via mkfifo) using the fortran/pipe_module.f90. If the application is running, it will read the request, execute the matching blas, and send back the result.

Build
-----

    $ rebar3 compile



Communication protocol
----
Piper consits in a Ping Pong match between a fortran caller and the Piper callee. The caller sends a stream of packets, Piper reads/modifies/sends it back.
Each communication consists in a stream of packets:
```
(PACKET_SIZE(int, 32bits), PACKET_TYPE(int, 32 bits), PACKET_CONTENT(binary, PACKET_SIZE bits))
```
where PACKET_TYPE is one of 

```
0   integer
1   string
2   array
3   character
4   real
5   double precision
6   logical
7   complex
8   complex16
```
On fortran's side, a writing sequence would consist of: ```fortran call pipe_open_write(pipe_location, pipe)```, a sequence of write, ```fortran call pipe_close(pipe)``` .
A reading sequence would be ```fortran call pipe_open_read(pipe_location, pipe)```, a sequence of read, ```fortran call pipe_close(pipe)``` .

For each PACKET_TYPE, ```pipe_[read|write]_[PACKET_TYPE_NAME](s)(variable, pipe)``` is implemeted to either read/write the variable from/to the pipe, variable being either a scalar or an array (suffix s). 
```
 integer :: n=3, m(2)=[1,2]
 pipe_write_integer(n, pipe)
 pipe_write_integers(m,pipe)

 pipe_read_integer(n,pipe)
 pipe_read_integers(m,pipe)
```
