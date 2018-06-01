program main
  use mult
  implicit none
  real :: m1(2,2), m2(2,2)
  real :: result(2,2)
  integer :: error
  WRITE(*,*) "m1"
  m1(1,1) = 1;
  m1(1,2) = 2;
  m1(2,1) = 3;
  m1(2,2) = 4;
  m2(1,1) = 5;
  m2(1,2) = 6;
  m2(2,1) = 7;
  m2(2,2) = 8;
  WRITE(*,*) m1(1,:)
  WRITE(*,*) m1(2,:)
  WRITE(*,*) "m2"

  WRITE(*,*) m2(1,:)
  WRITE(*,*) m2(2,:)
    WRITE(*,*) "aaa1"
  call mult1(m1,m2,result,error)
    WRITE(*,*) result(1,:)
    WRITE(*,*) result(2,:)
      WRITE(*,*) "aaa2"
    call mult2(m1,m2,result,error)
    WRITE(*,*) result(1,:)
    WRITE(*,*) result(2,:)
        WRITE(*,*) "aaa3"
      call mult3(m1,m2,result,error)
      WRITE(*,*) result(1,:)
      WRITE(*,*) result(2,:)
          WRITE(*,*) "aaa4"
        call mult4(m1,m2,result,error)
        WRITE(*,*) result(1,:)
        WRITE(*,*) result(2,:)
            WRITE(*,*) "aaa"
                result = matmul(m1,m2)
                WRITE(*,*) result(1,:)
                WRITE(*,*) result(2,:)

end
