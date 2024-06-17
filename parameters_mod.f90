module params
    implicit none
    integer, parameter :: jmax=101, nmax=500
    real(8), parameter :: range_x = 1.0d0, dx = range_x / (dble(jmax) - 1.0d0), ex_time = 0.2d0, &
    & cfl = 0.15d0, gamma = 1.4d0, beta = 2.5d0, phi = 1.0d0 / 3.0d0
end module params