subroutine integ
    use params
    use globals

    implicit none
    !Bの式
    bq(0:jmax, 1:3) = bq(0:jmax, 1:3) - (dt/dx) * (e(0:jmax, 1:3) - e(-1:jmax-1, 1:3))
end subroutine integ