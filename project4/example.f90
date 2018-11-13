program example
    use iso_fortran_env, only: real64
    use mod_ising
    implicit none

    type(ising) :: sim
    integer, allocatable :: Ls(:), equilibration_cycles, simulation_cycles
    real(real64), allocatable :: Ts(:), Tmax, Tmin

    integer :: i, j, num_Ls, num_Ts, u_E, u_M, u_CV, u_chi

    num_Ts = 40
    Tmin = 2.2d0
    Tmax = 2.4d0
    Ts = [(Tmin + i*(Tmax-Tmin)/(num_Ts-1), i = 0, num_Ts-1)]
    Ls = [200, 400]

    equilibration_cycles = 10000*num_images()
    simulation_cycles = 100000*num_images()

    num_Ts = size(Ts); num_Ls = size(Ls)

    if (this_image() == 1) then
        open(newunit=u_E, file="E.dat", status="replace")
        write(u_E, "('T', *(:,x,'L=',i0))") Ls
        open(newunit=u_M, file="M.dat", status="replace")
        write(u_M, "('T', *(:,x,'L=',i0))") Ls
        open(newunit=u_CV, file="CV.dat", status="replace")
        write(u_CV, "('T', *(:,x,'L=',i0))") Ls
        open(newunit=u_chi, file="chi.dat", status="replace")
        write(u_chi, "('T', *(:,x,'L=',i0))") Ls

        write(*,"(a,x,i0,x,a)") "Using", num_images(), "images"
    end if

    do j = 1, size(Ts)
        if (this_image() == 1) then
            write(u_E, "(f0.6)", advance="no") Ts(j)
            write(u_M, "(f0.6)", advance="no") Ts(j)
            write(u_CV, "(f0.6)", advance="no") Ts(j)
            write(u_chi, "(f0.6)", advance="no") Ts(j)
        end if

        do i = 1, size(Ls)
            write(*,*) "T = ", Ts(j), ", L = ", Ls(i)
            call sim%init(Ls(i), Ts(j))

            call sim%metropolis(equilibration_cycles)
            call sim%metropolis(simulation_cycles)

            if (this_image() == 1) then
                write(u_E, "(x,f0.6)", advance="no") sim%E
                write(u_M, "(x,f0.6)", advance="no") sim%absM
                write(u_CV, "(x,f0.6)", advance="no") sim%C_V
                write(u_chi, "(x,f0.6)", advance="no") sim%chi
            end if
        end do

        if (this_image() == 1) then
            write(u_E, "('')")
            write(u_M, "('')")
            write(u_CV, "('')")
            write(u_chi, "('')")
        end if
    end do

    if (this_image() == 1) then
        close(u_E); close(u_M); close(u_CV); close(u_chi)
    end if
end program
