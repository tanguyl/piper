program MainProgram
  use PipeModule
  implicit none
  
  character(len=17) :: pipe_location = "../priv/pipe_file"
  integer :: pipe
  integer ::          d = 42,      e(2) = [42, 43]
  character ::        f = 'n'
  real ::             g = 32.0,    h(2) = [42.0, 43.0]
  double precision :: i = 32.0,    j(2) = [42.0, 43.0]
  logical ::          k = .true.,  l(2) = [.true., .false.]
  complex ::          m = (42,43), n(2) = [(42,43), (44, 45)]
  complex*16 ::       o = (42,43), p(2) = [(42,43), (44,45)]

  character(3) :: c = "end"


  !---------------
  ! testing integer

  print *, "Testing integer."
  print *, "At start: ", d, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_integer(d, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_integer(d, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", d, c
  
  !---------------
  ! testing integers

  print *, "Testing integers."
  print *, "At start: ", e, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_integers(e, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_integers(e, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", e, c
  
  !---------------
  ! testing character

  print *, "Testing character."
  print *, "At start: ", d, f, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_integer(d, pipe)
  call pipe_write_character(f, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_integer(d, pipe)
  call pipe_read_character(f, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", d, f, c
  
  !---------------
  ! testing real

  print *, "Testing real."
  print *, "At start: ", g, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_real(g, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_real(g, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", g, c
  
  !---------------
  ! testing reals

  print *, "Testing reals."
  print *, "At start: ", h, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_reals(h, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_reals(h, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", h, c
  
  !---------------
  ! testing double precision

  print *, "Testing double precisoin."
  print *, "At start: ", i, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_double(i, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_double(i, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", i, c
  
  !---------------
  ! testing reals

  print *, "Testing doubless."
  print *, "At start: ", j, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_doubles(j, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_doubles(j, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", j, c
  
  !---------------
  ! testing logical

  print *, "Testing logical."
  print *, "At start: ", k, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_logical(k, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_logical(k, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", k, c
  
  !---------------
  ! testing logicals

  print *, "Testing logicals."
  print *, "At start: ", l, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_logicals(l, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_logicals(l, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", l, c
  
  !---------------
  ! testing complex

  print *, "Testing complex."
  print *, "At start: ", m, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_complex(m, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_complex(m, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", m, c
  
  !---------------
  ! testing complexs

  print *, "Testing complexs."
  print *, "At start: ", n, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_complexs(n, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_complexs(n, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", n, c

  
  
  !---------------
  ! testing complex16

  print *, "Testing complex16."
  print *, "At start: ", o, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_complex16(o, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_complex16(o, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", o, c
  
  !---------------
  ! testing complex16s

  print *, "Testing complex16s."
  print *, "At start: ", p, c

  call pipe_open_write(pipe_location, pipe)
  call pipe_write_complex16s(p, pipe)
  call pipe_write_characters(c, pipe)
  call pipe_close(pipe)

  call pipe_open_read(pipe_location, pipe)
  call pipe_read_complex16s(p, pipe)
  call pipe_read_characters(c, pipe)
  call pipe_close(pipe)

  print *, "at end:   ", p, c
end program MainProgram