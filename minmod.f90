function minmod(a, b) result(z)
    implicit none
    real(8), intent(in) :: a(3), b(3)
    real(8) z(3)
    integer i
    do i = 1, 3
        z(i) = sign(1.0d0, a(i)) * max(0.0d0, min(abs(a(i)), b(i) * sign(1.0d0, a(i))))
    end do
end function minmod