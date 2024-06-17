function qtobq(rho, u, p) result(big_q)
    use params
    use globals

    implicit none
    real(8), intent(in) :: rho, u, p
    real(8) big_q(3)
    big_q(1) = rho
    big_q(2) = rho * u
    big_q(3) = (p/(gamma-1.0d0)) + 0.5d0 * rho * u * u
end function qtobq

function bqtoq(big_q) result(q)
    use params
    use globals

    implicit none
    real(8), intent(in) :: big_q(3)
    real(8) q(3)
    q(1) = big_q(1)
    q(2) = big_q(2)/big_q(1)
    q(3) = (gamma-1.0d0) * (big_q(3) - 0.5d0 * big_q(1) * (big_q(2)/big_q(1)) ** 2.0d0)
end function bqtoq