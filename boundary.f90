function ave_m(met, i, j) result(ave)
    implicit none
    integer, intent(in) :: i, j
    real(8), intent(in) :: met(-3:, -3:)
    real(8) ave
    ave = (met(i, j) + met(i-1, j)) / 2.0d0
end function ave_m

function ave_n(met, i, j) result(ave)
    implicit none
    integer, intent(in) :: i, j
    real(8), intent(in) :: met(-3:, -3:)
    real(8) ave
    ave = (met(i, j) + met(i, j-1)) / 2.0d0
end function ave_n

subroutine bound
    use globals
    use interface_mod, only : qtobq, bqtoq, ave_m, ave_n

    implicit none
    real(8) temp_q(4), q_bc(4), bu, bv, u, v
    integer i, j
    !境界条件: qからq_bcを出す、その後 q_bc→bq
        !BD4
    !BD1    !BD2
        !BD3

    !BD3
    !slip condition
    !j=j+1, u_j = -u_j+1, rho_j = rho_j+1, e_j = e_j+1
    do i = -2, 102
        !rho_-1 = rho_0, u_-1 = -u_0, e_-1 = e_0
        !rho_j = rho_j+1
        !temp_q(2) = u_j+1 = u, temp_q(3) = v_j+1 = v
        temp_q(:) = bqtoq(bq(i, j+1, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_j+1, bv=UU_j+1を求め、q_bc(2)=u_j, q_bc(3)=v_jを求める
        bu = ave_n(nx, i, j+1) * u + ave_n(ny, i, j+1) * v
        bv = -ave_n(ny, i, j+1) * u + ave_n(nx, i, j+1) * v
        q_bc(2) = (-ave_n(nx, i, j) * bu - ave_n(ny, i, j) * bv) / (ave_n(nx, i, j) ** 2.0d0 + ave_n(ny, i, j) ** 2.0d0)
        q_bc(3) = (-ave_n(ny, i, j) * bu + ave_n(nx, i, j) * bv) / (ave_n(nx, i, j) ** 2.0d0 + ave_n(ny, i, j) ** 2.0d0)

        !pは適当、e=bq(i, j, 4)はe_j = e_j+1
        bq(i, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(i, j, 4) = bq(i, j+1, 4)
    end do

    temp_q(1:4) = bqtoq(bq(0, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(-1, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
    !rho_-2=rho_1, u_-2=-u_1, pre_-2=pre_1
    temp_q(1:3) = bqtoq(bq(1, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(-2, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))

    !wall_out
    !rho_101=rho_100, u_101=-u_100, pre_101=pre_100
    temp_q(1:3) = bqtoq(bq(100, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(101, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
    !rho_102=rho_99, u_102=-u_99, pre_102=pre_99
    temp_q(1:3) = bqtoq(bq(99, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(102, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
end subroutine bound

!BD3ひな型
!!slip condition
!!j=j+1, u_j = -u_j+1, rho_j = rho_j+1, e_j = e_j+1
!do i = -2, 102
    !!rho_j = rho_j+1
    !!temp_q(2) = u_j+1 = u, temp_q(3) = v_j+1 = v
    !temp_q(:) = bqtoq(bq(i, j+1, :))
    !q_bc(1) = temp_q(1)
    !u = temp_q(2)
    !v = temp_q(3)
    !! bu=U_j+1, bv=UU_j+1を求め、q_bc(2)=u_j, q_bc(3)=v_jを求める
    !bu = ave_n(nx, i, j+1) * u + ave_n(ny, i, j+1) * v
    !bv = -ave_n(ny, i, j+1) * u + ave_n(nx, i, j+1) * v
    !q_bc(2) = (-ave_n(nx, i, j) * bu - ave_n(ny, i, j) * bv) / (ave_n(nx, i, j) ** 2.0d0 + ave_n(ny, i, j) ** 2.0d0)
    !q_bc(3) = (-ave_n(ny, i, j) * bu + ave_n(nx, i, j) * bv) / (ave_n(nx, i, j) ** 2.0d0 + ave_n(ny, i, j) ** 2.0d0)

    !!pは適当、e=bq(i, j, 4)はe_j = e_j+1
    !bq(i, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
    !bq(i, j, 4) = bq(i, j+1, 4)
!end do