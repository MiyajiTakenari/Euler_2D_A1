subroutine delbar(qim, qi, qip, bar_plus, bar_minus)
    use params
    use interface_mod, only : minmod

    implicit none
    real(8), intent(in) :: qim(3), qi(3), qip(3)
    real(8), intent(out) :: bar_plus(3), bar_minus(3)
    real(8) plus(3), minus(3)
    plus(:) = qip(:) - qi(:)
    minus(:) = qi(:) - qim(:)
    bar_plus(:) = minmod(plus(:), beta * minus(:))
    bar_minus(:) = minmod(minus(:), beta * plus(:))
end subroutine delbar