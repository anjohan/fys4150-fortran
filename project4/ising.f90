module mod_ising
    use iso_fortran_env, only: int64, real64
    implicit none

    type ising
        integer, allocatable :: lattice(:,:), index_map(:), is(:), js(:)
        integer :: L
        real(real64) :: E, E2, M, absM, M2, T, beta, C_V, chi
        real(real64), allocatable :: exps(:), tmp_reals(:,:)

        contains
            procedure :: metropolis, one_cycle, init, reset_expvals, energy
    end type

    contains
        subroutine metropolis(self, num_cycles)
            class(ising), intent(inout) :: self
            integer, intent(in) :: num_cycles

            integer :: cycles_per_image, i, norm

            self%M = sum(self%lattice)
            call self%energy()
            call self%reset_expvals()

            cycles_per_image = num_cycles/num_images()

            do i = 1, cycles_per_image
                call self%one_cycle()
            end do

            call co_sum(self%E)
            call co_sum(self%E2)
            call co_sum(self%M)
            call co_sum(self%M2)
            call co_sum(self%absM)

            norm = num_cycles

            self%M2 = self%M2/norm
            self%absM = self%absM/norm
            self%E = self%E/norm
            self%E2 = self%E2/norm
            self%M = self%M/norm
            self%C_V = (self%E2 - self%E**2)/(self%T**2)
            self%chi = (self%M2 - self%M**2)/(self%T)

            self%M2 = self%M2/self%L**2
            self%absM = self%absM/self%L**2
            self%E = self%E/self%L**2
            self%E2 = self%E2/self%L**2
            self%M = self%M/self%L**2
            self%C_V = self%C_V/self%L**2
            self%chi = self%chi/self%L**2

        end subroutine

        subroutine energy(self)
            class(ising), intent(inout) :: self
            integer :: i, j

            associate(E => self%E, l => self%lattice, map => self%index_map)
                E = 0
                do j = 0, self%L-1
                    do i = 0, self%L-1
                        E = E - l(i,j)*(l(i,map(j+1)) + l(map(i+1),j))
                    end do
                end do
            end associate
        end subroutine


        subroutine one_cycle(self)
            class(ising), intent(inout) :: self
            integer :: k, j, i, dE
            logical :: accept
            real(real64) :: rnd

            call random_number(self%tmp_reals)
            self%is(:) = floor(self%tmp_reals(:,1)*self%L)
            self%js(:) = floor(self%tmp_reals(:,2)*self%L)

            do k = 1, self%L**2
                i = self%is(k)
                j = self%js(k)
                associate(lattice => self%lattice, index_map => self%index_map)
                associate(up => lattice(i, index_map(j+1)), &
                          down => lattice(i, index_map(j-1)), &
                          right => lattice(index_map(i+1), j), &
                          left => lattice(index_map(i-1), j))
                    accept = .false.
                    dE = 2*lattice(i,j)*(up+down+right+left)
                    if (dE < 0) then
                        accept = .true.
                    else
                        call random_number(rnd)
                        accept = rnd < self%exps(dE)
                    end if

                    if (accept) then
                    !write(*,*) "Accepted", self%E, self%M
                        lattice(i,j) = - lattice(i,j)
                        self%E = self%E + dE
                        self%M = self%M + 2*lattice(i,j)
                    end if
                end associate
                end associate
            end do

            self%E2 = self%E2 + self%E**2
            self%absM = self%absM + abs(self%M)
            self%M2 = self%M2 + self%M**2
        end subroutine

        subroutine init(self, L, T)
            class(ising), intent(inout) :: self
            integer, intent(in) :: L
            real(real64), intent(in) :: T

            integer :: i

            if (allocated(self%lattice)) deallocate(self%lattice)

            allocate(self%lattice(0:L-1,0:L-1))
            self%lattice(:,:) = 1.0d0

            self%L = L
            self%T = T
            self%beta = 1.0d0/T

            if (allocated(self%exps)) deallocate(self%exps)

            allocate(self%exps(-8:8))

            do i = -8, 8, 4
                self%exps(i) = exp(-self%beta*i)
            end do

            if (allocated(self%index_map)) deallocate(self%index_map)
            allocate(self%index_map(-1:L))

            self%index_map(-1) = L-1
            self%index_map(L) = 0

            do i = 0, L-1
                self%index_map(i) = i
            end do

            if (allocated(self%tmp_reals)) deallocate(self%tmp_reals)
            allocate(self%tmp_reals(L**2,2))
            if (allocated(self%is)) deallocate(self%is)
            allocate(self%is(L**2))
            if (allocated(self%js)) deallocate(self%js)
            allocate(self%js(L**2))

            call self%reset_expvals()
        end subroutine

        subroutine reset_expvals(self)
            class(ising), intent(inout) :: self

             self%E2 = 0; self%absM = 0; self%M2 = 0
        end subroutine
end module
