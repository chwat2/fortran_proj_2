module mult

    implicit none

contains

  subroutine makeSimulation(N)
    implicit none

      real,allocatable :: m1(:,:), m2(:,:),result(:,:);

      integer, intent(in) :: N;
      integer :: i,j;
      real (kind = 8) :: k;
      integer :: error;
integer (kind = 4):: iclock;
real (kind = 8) :: start,finish

      allocate(m1(N,N));
      allocate(m2(N,N));
      allocate(result(N,N));

      error = 0;

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
call cpu_time(start)
      call mult2(m1,m2,result,error)
call cpu_time(finish)
      WRITE(*,*) result .eq. matmul(m1,m2)
      print '("Time = ", f6.3," seconds.")',finish-start
      if (allocated(m1)) deallocate(m1)
      if (allocated(m2)) deallocate(m2)
      if (allocated(result)) deallocate(result)

  end subroutine


  subroutine mult10(m1, m2, result, error)

      real, intent(in) :: m1(:, :), m2(:, :)
      real, intent(out) :: result(:, :)
      integer, intent(out) :: error
      integer :: shape1(2), shape2(2), shape3(2)
      error = 0
      shape1 = shape(m1)
      shape2 = shape(m2)
      shape3 = shape(result)

      if (shape1(2) .NE. shape2(1)) then
          error = 1
          return
      endif

      if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
          error = 2
          return
      endif

      result = matmul(m1,m2)
  end subroutine

    subroutine mult1(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: i, j, k, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        WRITE(*,*) shape1

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif

        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do i = 1, shape1(1)
            do j = 1, shape2(2)
                do k = 1, shape1(2)
                    result(i,j) = result(i,j) + m1(i,k) * m2(k,j)
                end do
            end do
        end do

    end subroutine



    subroutine mult2(m1, m2, result, error)
        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: i, j, shape1(2), shape2(2), shape3(2)

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif

        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        do i = 1, shape1(1)
            do j = 1, shape2(2)
                result(i,j) = DOT_PRODUCT(m1(i,:), m2(:,j))
            end do
        end do

    end subroutine

    subroutine mult3(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: shape1(2), shape2(2), shape3(2)
        integer(kind = 4) :: i, j, k, jj, ii
        integer (kind = 4) :: ichunk

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif

        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        ! use -funroll-loops
        ichunk = 512 ! I have a 3MB cache size (real*4)
        do ii = 1, shape1(1), ichunk
           do jj = 1, shape2(2), ichunk

              do i = ii, min(ii + ichunk - 1, shape1(1))
                 do j = jj, min(jj + ichunk - 1, shape2(2))
                    do k = 1, shape1(2)
                       result(i,j) = result(i,j) + m1(i,k) * m2(k,j)
                    end do
                 end do
              end do

           end do
        end do

    end subroutine

    subroutine mult4(m1, m2, result, error)

        real, intent(in) :: m1(:, :), m2(:, :)
        real, intent(out) :: result(:, :)
        integer, intent(out) :: error
        integer :: shape1(2), shape2(2), shape3(2)
        integer(kind = 4) :: i, j, k, jj, kk
        integer (kind = 4) :: ichunk

        result = 0.d0
        error = 0

        shape1 = shape(m1)
        shape2 = shape(m2)
        shape3 = shape(result)

        if (shape1(2) .NE. shape2(1)) then
            error = 1
            return
        endif

        if ((shape3(1) .NE. shape1(1)) .OR. (shape3(2) .NE. shape2(2))) then
            error = 2
            return
        endif

        ! use -funroll-loops
        ichunk = 512 ! I have a 3MB cache size (real*4)
        do i = 1, shape1(1)
            do j = 1, shape2(2)
                result(i,j) = DOT_PRODUCT(m1(i,:), m2(:,j))
            end do
        end do

    end subroutine

end module
