program main
  use mult
  implicit none


  integer :: m
      open(unit = 7, file = "../data.dat")

    do m = 1,35
      call makeSimulation(m**2);
    end do

    call makeSimulation(2048);
      close(7)


end
