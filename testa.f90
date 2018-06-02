program main
  use mult
  implicit none


  integer :: error
  WRITE(*,*) "m1"

  makeSimulation(5);
end
function makeSimulation(N)
  implicit none
  use mult
    real,allocatable :: m1(:,:), m2(:,:);
    real result(:,:);
    integer, intent(in) :: N;
    integer :: i,j;
    real (kind = 8) :: k;

    allocate(m1(N,N));
    allocate(m2(N,N));
    k=1.d0;
    do i = 1, N
        do j = 1, N
                m1(i,j) = k;
                k = k + 1.d0;
        end do
    end do
    do i = 1, N
        do j = 1, N
                m2(i,j) = k;
                k = k + 1.d0;
        end do
    end do

    call mult2(m1,m2,result,error)
    WRITE(*,*) result
    if(allocated(m1)) (deallocate(m1))

end subroutine
