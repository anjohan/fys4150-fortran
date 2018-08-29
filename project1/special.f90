module special
    use iso_fortran_env, only: int64
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    contains
        function special_algorithm(d, v) result(t)
            real(dp), intent(inout) :: d(:), v(:)
            real(dp), allocatable   :: b(:)
            integer :: n, i

            real(dp) :: t
            integer(int64) :: t1, t2, cpu_rate

            n = size(v)

            allocate(b(n))

            call system_clock(t1, cpu_rate)

            b(1) = 2

            ! row reduction
            do i = 2, n
                b(i) = (i+1.0d0)/i
                d(i) = d(i) + d(i-1)/b(i-1)
            end do

            v(n) = d(n)/b(n)

            ! backward substitution
            do i = n-1, 1, -1
                v(i) = (d(i) + v(i+1))/b(i)
            end do

            call system_clock(t2, cpu_rate)

            t = real(t2 - t1, kind=dp)/cpu_rate
        end function
end module
