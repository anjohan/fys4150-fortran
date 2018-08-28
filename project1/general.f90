module general
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    contains
        subroutine general_algorithm(a, b, c, d, v)
            real(dp), intent(inout) :: a(:), b(:), c(:), d(:), v(:)

            real(dp) :: r
            integer :: i, n

            n = size(v)

            ! row reduction
            do i = 2, n
                r    = a(i-1)/b(i-1)
                b(i) = b(i) - c(i-1)*r
                d(i) = d(i) - d(i-1)*r
            end do

            v(n) = d(n)/b(n)

            ! backward substitution
            do i = n-1, 1, -1
                v(i) = (d(i) - c(i)*v(i+1))/b(i)
            end do
        end subroutine
end module
