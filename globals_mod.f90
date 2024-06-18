module globals
    use params
    real(8), save :: dt, x(0:jmax), y(0:jmax), bq(-2:jmax+2, 3), e(-1:jmax, 3), res(3), time = 0.0d0
    integer, save :: exit_flag = 0
end module globals