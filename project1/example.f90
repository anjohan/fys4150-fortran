program example
    use general, only: general_algorithm, dp
    use special, only: special_algorithm
    implicit none

    real(dp) :: h
    real(dp), allocatable :: a(:), b(:), c(:), x(:), f(:), exact(:), &
                             d_s(:), d_g(:), v_s(:), v_g(:)
    integer :: n_pow, n, i, outfile

    write(*,*) "Give log10(n):"
    read(*,*) n_pow

    n = 10**n_pow
    h = 1.0d0/(n + 1)

    allocate(x(0:n+1), f(0:n+1), exact(0:n+1))
    x(:) = [(i*h, i = 0, n+1)]
    f(:) = 100*exp(-10*x(:))
    exact(:) = 1 - (1-exp(-10.0d0))*x(:) - exp(-10*x(:))

    allocate(v_s(0:n+1), v_g(0:n+1))
    v_s(0) = 0; v_s(n+1) = 0; v_g(0) = 0; v_g(n+1) = 0

    d_s = h**2 * f(1:n)
    d_g = d_s

    allocate(a(1:n-1), b(1:n), c(1:n-1))
    a(:) = -1
    b(:) = 2
    c(:) = -1

    call special_algorithm(v_s(1:n), d_s(1:n))
    call general_algorithm(a, b, c, d_g(1:n), v_g(1:n))

    open(newunit=outfile, file="results.dat", status="replace")

    write(outfile, *) "x special general exact"

    do i = 0, n+1
        write(outfile, *) x(i), v_s(i), v_g(i), exact(i)
    end do

    close(outfile)
end program
