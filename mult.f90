module mult

    implicit none

contains

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
