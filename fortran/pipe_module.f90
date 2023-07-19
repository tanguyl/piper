module PipeModule

! Each message is of type: size(32 bits), type(32bit int), binary of size bits)

  implicit none
  public

  !character(len=:), allocatable :: pipe_name

contains

  subroutine pipe_open_write(pipe_name, pipe)
    implicit none
    character(len=*), intent(in) :: pipe_name
    integer, intent(out) :: pipe
    integer :: ierr

    ! Open the named pipe for writing
    open(newunit=pipe, file=pipe_name, status='replace', access='stream', action='write', iostat=ierr)
    if (ierr /= 0) then
      write(*, *) "Error opening the named pipe."
      return
    end if

  end subroutine 
  
  subroutine pipe_open_read(pipe_name, pipe)
    implicit none
    character(len=*), intent(in) :: pipe_name
    integer, intent(out) :: pipe
    integer :: ierr

    ! Open the named pipe for writing
    open(newunit=pipe, file=pipe_name, status='replace', access='stream', action='read', iostat=ierr)
    if (ierr /= 0) then
      write(*, *) "Error opening the named pipe."
      return
    end if

  end subroutine


  subroutine pipe_close(pipe)
    implicit none
    integer, intent(in) :: pipe
    close(pipe)
  end subroutine 

  
  subroutine pipe_skip(pipe)
    ! Skip next packet.
    implicit none
    integer, intent(in) :: pipe
    integer   :: size, discard, i
    character :: discard_c
    
    read(pipe) size
    read(pipe) discard

    size = size/8
    do i=1,size
      read(pipe) discard_c
    end do
  
  end subroutine

  

  subroutine pipe_write_character(param, pipe)
    implicit none
    character, intent(in) :: param
    integer, intent(in) :: pipe

    ! Write the parameters to the pipe
    write(pipe) 8
    write(pipe) 3
    write(pipe) param

  end subroutine

  subroutine pipe_read_character(param, pipe)
    implicit none
    character, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine

  subroutine pipe_write_characters(param, pipe)
    
    implicit none
    character(*), intent(in) :: param
    integer, intent(in)      :: pipe

    integer :: length
    length = LEN_TRIM(param)
    
    ! In fortran, CHARACTER are encoded on 8 bits.
    write(pipe) 8*length
    write(pipe) 1
    write(pipe) param
  
  end subroutine



  subroutine pipe_read_characters(param, pipe)
    character(*), intent(out) :: param
    integer, intent(in) :: pipe
    integer :: skip

    read(pipe) skip
    read(pipe) skip
    read(pipe) param
  end subroutine

  

  subroutine pipe_write_integer(param, pipe)
    implicit none
    integer, intent(in) :: param, pipe

    ! Write the parameters to the pipe
    write(pipe) 32
    write(pipe) 0
    write(pipe) param

  end subroutine

  subroutine pipe_read_integer(param, pipe)
    implicit none
    integer, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine

  subroutine pipe_write_integers(param, pipe)
    implicit none
    integer, intent(in) :: param(:), pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 32*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_integers(param, pipe)
    implicit none
    integer, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine 

  
  subroutine pipe_write_real(param, pipe)
    implicit none
    real, intent(in) :: param
    integer, intent(in) :: pipe

    ! Write the parameters to the pipe
    write(pipe) 32
    write(pipe) 4
    write(pipe) param

  end subroutine

  subroutine pipe_read_real(param, pipe)
    implicit none
    real, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine

  subroutine pipe_write_reals(param, pipe)
    implicit none
    real, intent(in) :: param(:)
    integer, intent(in) :: pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 32*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_reals(param, pipe)
    implicit none
    real, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine 


  subroutine pipe_write_double(param, pipe)
    implicit none
    double precision, intent(in) :: param
    integer, intent(in) :: pipe
  
    ! Write the parameters to the pipe
    write(pipe) 64
    write(pipe) 5
    write(pipe) param
  
  end subroutine
  
  subroutine pipe_read_double(param, pipe)
    implicit none
    double precision, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard
  
    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param
  
  end subroutine
    
  subroutine pipe_write_doubles(param, pipe)
    implicit none
    double precision, intent(in) :: param(:)
    integer, intent(in) :: pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 64*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_doubles(param, pipe)
    implicit none
    double precision, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine 

  subroutine pipe_write_logical(param, pipe)
    implicit none
    logical, intent(in) :: param
    integer, intent(in) :: pipe
  
    ! Write the parameters to the pipe
    write(pipe) 32
    write(pipe) 6
    write(pipe) param
  
  end subroutine
  
  subroutine pipe_read_logical(param, pipe)
    implicit none
    logical, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard
  
    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param
  
  end subroutine

  
  subroutine pipe_write_logicals(param, pipe)
    implicit none
    logical, intent(in) :: param(:)
    integer, intent(in) :: pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 32*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_logicals(param, pipe)
    implicit none
    logical, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine


  subroutine pipe_write_complex(param, pipe)
    implicit none
    complex, intent(in) :: param
    integer, intent(in) :: pipe
  
    ! Write the parameters to the pipe
    write(pipe) 64
    write(pipe) 7
    write(pipe) param
  
  end subroutine
  
  subroutine pipe_read_complex(param, pipe)
    implicit none
    complex, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard
  
    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param
  
  end subroutine
    
  subroutine pipe_write_complexs(param, pipe)
    implicit none
    complex, intent(in) :: param(:)
    integer, intent(in) :: pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 64*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_complexs(param, pipe)
    implicit none
    complex, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine


  subroutine pipe_write_complex16(param, pipe)
    implicit none
    complex*16, intent(in) :: param
    integer, intent(in) :: pipe
  
    ! Write the parameters to the pipe
    write(pipe) 128
    write(pipe) 8
    write(pipe) param
  
  end subroutine
  
  subroutine pipe_read_complex16(param, pipe)
    implicit none
    complex*16, intent(out) :: param
    integer, intent(in)  :: pipe
    integer :: discard
  
    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param
  
  end subroutine
    
  subroutine pipe_write_complex16s(param, pipe)
    implicit none
    complex*16, intent(in) :: param(:)
    integer, intent(in) :: pipe
    integer :: n_elem

    n_elem = size(param)

    ! Write the parameters to the pipe
    write(pipe) 128*n_elem
    write(pipe) 2
    write(pipe) param

  end subroutine

  subroutine pipe_read_complex16s(param, pipe)
    implicit none
    complex*16, intent(out) :: param(:)
    integer, intent(in)  :: pipe
    integer :: discard

    ! Write the parameters to the pipe
    read(pipe) discard
    read(pipe) discard
    read(pipe) param

  end subroutine 

end module PipeModule