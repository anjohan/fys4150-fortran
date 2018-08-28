module general
    use iso_fortran_env, only: int64
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    contains
        function general_algorithm(a, b, c, d, v) result(t)
            real(dp), intent(inout) :: a(:), b(:), c(:), d(:), v(:)

            real(dp) :: t, r
            integer :: i, n
            integer(int64) :: t1, t2, cpu_rate

            n = size(v)

            call system_clock(t1, cpu_rate)

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

            call system_clock(t2, cpu_rate)

            t = real(t2 - t1, kind=dp)/cpu_rate
        end function
end module
