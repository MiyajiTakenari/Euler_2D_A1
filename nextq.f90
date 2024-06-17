subroutine nextq
    use params
    use globals

    implicit none
    !Bの式
    bq(1:jmax, 1:3) = bq(1:jmax, 1:3) - (dt/dx) * (e(1:jmax, 1:3) - e(0:jmax-1, 1:3))
end subroutine nextq