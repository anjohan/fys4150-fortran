module special
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    contains
        subroutine special_algorithm(v, d)
            real(dp), intent(inout) :: v(:), d(:)
            real(dp), allocatable   :: b(:)
            integer :: n, i

            n = size(v)

            b = [((i+1.0d0)/i, i = 1, n)]

            ! row reduction
            do i = 2, n
                d(i) = d(i) + d(i-1)/b(i-1)
            end do

            v(n) = d(n)/b(n)

            ! backward substitution
            do i = n-1, 1, -1
                v(i) = (d(i) + v(i+1))/b(i)
            end do
        end subroutine
end module
