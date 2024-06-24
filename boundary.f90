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
        !rho_-1 = rho_0
        !temp_q(2) = u_0 = u, temp_q(3) = v_0 = v
        temp_q(:) = bqtoq(bq(i, 0, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_0, bv=UU_0を求め、q_bc(2)=u_-1, q_bc(3)=v_-1を求める
        bu = ave_n(nx, i, 0) * u + ave_n(ny, i, 0) * v
        bv = -ave_n(ny, i, 0) * u + ave_n(nx, i, 0) * v
        q_bc(2) = (-ave_n(nx, i, -1) * bu - ave_n(ny, i, -1) * bv) / (ave_n(nx, i, -1) ** 2.0d0 + ave_n(ny, i, -1) ** 2.0d0)
        q_bc(3) = (-ave_n(ny, i, -1) * bu + ave_n(nx, i, -1) * bv) / (ave_n(nx, i, -1) ** 2.0d0 + ave_n(ny, i, -1) ** 2.0d0)
        !pは適当、e=bq(i, -1, 4)はe_-1 = e_0
        bq(i, -1, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(i, -1, 4) = bq(i, 0, 4)

        !rho_-2 = rho_1, u_-2 = -u_1, e_-2 = e_1
        !rho_-2 = rho_1
        !temp_q(2) = u_1 = u, temp_q(3) = v_1 = v
        temp_q(:) = bqtoq(bq(i, 1, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_1, bv=UU_1を求め、q_bc(2)=u_-2, q_bc(3)=v_-2を求める
        bu = ave_n(nx, i, 1) * u + ave_n(ny, i, 1) * v
        bv = -ave_n(ny, i, 1) * u + ave_n(nx, i, 1) * v
        q_bc(2) = (-ave_n(nx, i, -2) * bu - ave_n(ny, i, -2) * bv) / (ave_n(nx, i, -2) ** 2.0d0 + ave_n(ny, i, -2) ** 2.0d0)
        q_bc(3) = (-ave_n(ny, i, -2) * bu + ave_n(nx, i, -2) * bv) / (ave_n(nx, i, -2) ** 2.0d0 + ave_n(ny, i, -2) ** 2.0d0)
        !pは適当、e=bq(i, -2, 4)はe_-2 = e_1
        bq(i, -2, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(i, -2, 4) = bq(i, 1, 4)
    end do


    !BD4
    !slip condition
    !j+1=j, u_j+1 = -u_j, rho_j+1 = rho_j, e_j+1 = e_j
    do i = -2, 102
        !101=100, u_101 = -u_100, rho_101 = rho_100, e_101 = e_100
        !rho_101 = rho_100
        !temp_q(2) = u_100 = u, temp_q(3) = v_100 = v
        temp_q(:) = bqtoq(bq(i, 100, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_100, bv=UU_100を求め、q_bc(2)=u_101, q_bc(3)=v_101を求める
        bu = ave_n(nx, i, 100) * u + ave_n(ny, i, 100) * v
        bv = -ave_n(ny, i, 100) * u + ave_n(nx, i, 100) * v
        q_bc(2) = (-ave_n(nx, i, 101) * bu - ave_n(ny, i, 101) * bv) / (ave_n(nx, i, 101) ** 2.0d0 + ave_n(ny, i, 101) ** 2.0d0)
        q_bc(3) = (-ave_n(ny, i, 101) * bu + ave_n(nx, i, 101) * bv) / (ave_n(nx, i, 101) ** 2.0d0 + ave_n(ny, i, 101) ** 2.0d0)
        !pは適当、e=bq(i, 101, 4)はe_101 = e_100
        bq(i, 101, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(i, 101, 4) = bq(i, 100, 4)

        !102=99, u_102 = -u_99, rho_102 = rho_99, e_102 = e_99
        !rho_102 = rho_99
        !temp_q(2) = u_99 = u, temp_q(3) = v_99 = v
        temp_q(:) = bqtoq(bq(i, 99, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_99, bv=UU_99を求め、q_bc(2)=u_102, q_bc(3)=v_102を求める
        bu = ave_n(nx, i, 99) * u + ave_n(ny, i, 99) * v
        bv = -ave_n(ny, i, 99) * u + ave_n(nx, i, 99) * v
        q_bc(2) = (-ave_n(nx, i, 102) * bu - ave_n(ny, i, 102) * bv) / (ave_n(nx, i, 102) ** 2.0d0 + ave_n(ny, i, 102) ** 2.0d0)
        q_bc(3) = (-ave_n(ny, i, 102) * bu + ave_n(nx, i, 102) * bv) / (ave_n(nx, i, 102) ** 2.0d0 + ave_n(ny, i, 102) ** 2.0d0)
        !pは適当、e=bq(i, 102, 4)はe_102 = e_99
        bq(i, 102, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(i, 102, 4) = bq(i, 99, 4)
    end do


    !BD1
    !slip condition
    !i=i+1, u_i = -u_i+1, uu_i = uu_i+1, rho_i = rho_i+1, e_i = e_i+1
    do j = -2, 102
        !-1=0, u_-1 = -u_0, uu_-1 = uu_0, rho_-1 = rho_0, e_-1 = e_0
        !rho_-1 = rho_0
        !temp_q(2) = u_0 = u, temp_q(3) = v_0 = v
        temp_q(:) = bqtoq(bq(0, j, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_0, bv=UU_0を求め、q_bc(2)=u_-1, q_bc(3)=v_-1を求める
        bu = ave_m(mx, 0, j) * u + ave_m(my, 0, j) * v
        bv = -ave_m(my, 0, j) * u + ave_m(mx, 0, j) * v
        q_bc(2) = (-ave_m(mx, -1, j) * bu - ave_m(my, -1, j) * bv) / (ave_m(mx, -1, j) ** 2.0d0 + ave_m(my, -1, j) ** 2.0d0)
        q_bc(3) = (-ave_m(my, -1, j) * bu + ave_m(mx, -1, j) * bv) / (ave_m(mx, -1, j) ** 2.0d0 + ave_m(my, -1, j) ** 2.0d0)
        !pは適当、e=bq(-1, j, 4)はe_-1 = e_0
        bq(-1, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(-1, j, 4) = bq(0, j, 4)

        !-2=1, u_-2 = -u_1, uu_-2 = uu_1, rho_-2 = rho_1, e_-2 = e_1
        !rho_-2 = rho_1
        !temp_q(2) = u_1 = u, temp_q(3) = v_1 = v
        temp_q(:) = bqtoq(bq(1, j, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_1, bv=UU_1を求め、q_bc(2)=u_-2, q_bc(3)=v_-2を求める
        bu = ave_m(mx, 1, j) * u + ave_m(my, 1, j) * v
        bv = -ave_m(my, 1, j) * u + ave_m(mx, 1, j) * v
        q_bc(2) = (-ave_m(mx, -2, j) * bu - ave_m(my, -2, j) * bv) / (ave_m(mx, -2, j) ** 2.0d0 + ave_m(my, -2, j) ** 2.0d0)
        q_bc(3) = (-ave_m(my, -2, j) * bu + ave_m(mx, -2, j) * bv) / (ave_m(mx, -2, j) ** 2.0d0 + ave_m(my, -2, j) ** 2.0d0)
        !pは適当、e=bq(-2, j, 4)はe_-2 = e_1
        bq(-2, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(-2, j, 4) = bq(1, j, 4)
    end do


    !BD2
    !slip condition
    !i+1=i, u_i+1 = -u_i, uu_i+1 = uu_i, rho_i+1 = rho_i, e_i+1 = e_i
    do j = -2, 102
        !101=100, u_101 = -u_100, uu_101 = uu_100, rho_101 = rho_100, e_101 = e_100
        !rho_101 = rho_100
        !temp_q(2) = u_100 = u, temp_q(3) = v_100 = v
        temp_q(:) = bqtoq(bq(100, j, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_100, bv=UU_100を求め、q_bc(2)=u_101, q_bc(3)=v_101を求める
        bu = ave_m(mx, 100, j) * u + ave_m(my, 100, j) * v
        bv = -ave_m(my, 100, j) * u + ave_m(mx, 100, j) * v
        q_bc(2) = (-ave_m(mx, 101, j) * bu - ave_m(my, 101, j) * bv) / (ave_m(mx, 101, j) ** 2.0d0 + ave_m(my, 101, j) ** 2.0d0)
        q_bc(3) = (-ave_m(my, 101, j) * bu + ave_m(mx, 101, j) * bv) / (ave_m(mx, 101, j) ** 2.0d0 + ave_m(my, 101, j) ** 2.0d0)
        !pは適当、e=bq(101, j, 4)はe_101 = e_100
        bq(101, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(101, j, 4) = bq(100, j, 4)

        !102=99, u_102 = -u_99, uu_102 = uu_99, rho_102 = rho_99, e_102 = e_99
        !rho_102 = rho_99
        !temp_q(2) = u_99 = u, temp_q(3) = v_99 = v
        temp_q(:) = bqtoq(bq(99, j, :))
        q_bc(1) = temp_q(1)
        u = temp_q(2)
        v = temp_q(3)
        ! bu=U_99, bv=UU_99を求め、q_bc(2)=u_102, q_bc(3)=v_102を求める
        bu = ave_m(mx, 99, j) * u + ave_m(my, 99, j) * v
        bv = -ave_m(my, 99, j) * u + ave_m(mx, 99, j) * v
        q_bc(2) = (-ave_m(mx, 102, j) * bu - ave_m(my, 102, j) * bv) / (ave_m(mx, 102, j) ** 2.0d0 + ave_m(my, 102, j) ** 2.0d0)
        q_bc(3) = (-ave_m(my, 102, j) * bu + ave_m(mx, 102, j) * bv) / (ave_m(mx, 102, j) ** 2.0d0 + ave_m(my, 102, j) ** 2.0d0)
        !pは適当、e=bq(102, j, 4)はe_102 = e_99
        bq(102, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
        bq(102, j, 4) = bq(99, j, 4)
    end do

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

!BD4ひな型
!!slip condition
!!j+1=j, u_j+1 = -u_j, rho_j+1 = rho_j, e_j+1 = e_j
!do i = -2, 102
    !!rho_j+1 = rho_j
    !!temp_q(2) = u_j = u, temp_q(3) = v_j = v
    !temp_q(:) = bqtoq(bq(i, j, :))
    !q_bc(1) = temp_q(1)
    !u = temp_q(2)
    !v = temp_q(3)
    !! bu=U_j, bv=UU_jを求め、q_bc(2)=u_j+1, q_bc(3)=v_j+1を求める
    !bu = ave_n(nx, i, j) * u + ave_n(ny, i, j) * v
    !bv = -ave_n(ny, i, j) * u + ave_n(nx, i, j) * v
    !q_bc(2) = (-ave_n(nx, i, j+1) * bu - ave_n(ny, i, j+1) * bv) / (ave_n(nx, i, j+1) ** 2.0d0 + ave_n(ny, i, j+1) ** 2.0d0)
    !q_bc(3) = (-ave_n(ny, i, j+1) * bu + ave_n(nx, i, j+1) * bv) / (ave_n(nx, i, j+1) ** 2.0d0 + ave_n(ny, i, j+1) ** 2.0d0)

    !!pは適当、e=bq(i, j+1, 4)はe_j+1 = e_j
    !bq(i, j+1, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
    !bq(i, j+1, 4) = bq(i, j, 4)
!end do

!BD1ひな型
!!slip condition
!!i=i+1, u_i = -u_i+1, uu_i = uu_i+1, rho_i = rho_i+1, e_i = e_i+1
!do j = -2, 102
    !!rho_i = rho_i+1
    !!temp_q(2) = u_i+1 = u, temp_q(3) = v_i+1 = v
    !temp_q(:) = bqtoq(bq(i+1, j, :))
    !q_bc(1) = temp_q(1)
    !u = temp_q(2)
    !v = temp_q(3)
    !! bu=U_i+1, bv=UU_i+1を求め、q_bc(2)=u_i, q_bc(3)=v_iを求める
    !bu = ave_m(mx, i+1, j) * u + ave_m(my, i+1, j) * v
    !bv = -ave_m(my, i+1, j) * u + ave_m(mx, i+1, j) * v
    !q_bc(2) = (-ave_m(mx, i, j) * bu - ave_m(my, i, j) * bv) / (ave_m(mx, i, j) ** 2.0d0 + ave_m(my, i, j) ** 2.0d0)
    !q_bc(3) = (-ave_m(my, i, j) * bu + ave_m(mx, i, j) * bv) / (ave_m(mx, i, j) ** 2.0d0 + ave_m(my, i, j) ** 2.0d0)

    !!pは適当、e=bq(i, j, 4)はe_i = e_i+1
    !bq(i, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
    !bq(i, j, 4) = bq(i+1, j, 4)
!end do

!BD2ひな型
!!slip condition
!!i+1=i, u_i+1 = -u_i, uu_i+1 = uu_i, rho_i+1 = rho_i, e_i+1 = e_i
!do j = -2, 102
    !!rho_i+1 = rho_i
    !!temp_q(2) = u_i = u, temp_q(3) = v_i = v
    !temp_q(:) = bqtoq(bq(i, j, :))
    !q_bc(1) = temp_q(1)
    !u = temp_q(2)
    !v = temp_q(3)
    !! bu=U_i, bv=UU_iを求め、q_bc(2)=u_i+1, q_bc(3)=v_i+1を求める
    !bu = ave_m(mx, i, j) * u + ave_m(my, i, j) * v
    !bv = -ave_m(my, i, j) * u + ave_m(mx, i, j) * v
    !q_bc(2) = (-ave_m(mx, i+1, j) * bu - ave_m(my, i+1, j) * bv) / (ave_m(mx, i+1, j) ** 2.0d0 + ave_m(my, i+1, j) ** 2.0d0)
    !q_bc(3) = (-ave_m(my, i+1, j) * bu + ave_m(mx, i+1, j) * bv) / (ave_m(mx, i+1, j) ** 2.0d0 + ave_m(my, i+1, j) ** 2.0d0)

    !!pは適当、e=bq(i+1, j, 4)はe_i+1 = e_i
    !bq(i+1, j, :) = qtobq(q_bc(1), q_bc(2), q_bc(3), 0.0d0)
    !bq(i+1, j, 4) = bq(i, j, 4)
!end do