module interface_mod
    interface

        function qtobq(rho, u, p) result(big_q)
            real(8), intent(in) :: rho, u, p
            real(8) big_q(3)
        end function qtobq

        function bqtoq(big_q) result(q)
            real(8), intent(in) :: big_q(3)
            real(8) q(3)
        end function bqtoq

        subroutine init
        end subroutine init

        subroutine bound
        end subroutine bound

        subroutine cflc
        end subroutine cflc

        function minmod(a, b) result(z)
            real(8), intent(in) :: a(3), b(3)
            real(8) z(3)
        end function minmod

        subroutine delbar(qim, qi, qip, bar_plus, bar_minus)
            real(8), intent(in) :: qim(3), qi(3), qip(3)
            real(8), intent(out) :: bar_plus(3), bar_minus(3)
        end subroutine delbar

        subroutine xflux
        end subroutine xflux

        subroutine nextq
        end subroutine nextq

        subroutine calc_res
        end subroutine calc_res

    end interface
end module interface_mod