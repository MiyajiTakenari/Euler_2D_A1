subroutine bound
    use globals
    use interface_mod, only : qtobq, bqtoq

    implicit none
    real(8) temp_q(3), q_bc(3)
    !境界条件
    !qからq_bcを出す、その後 q_bc→bq
    !wall condition
    !wall_in
    !rho_0=rho_1, u_0=-u_1, pre_0=pre_1
    temp_q(1:3) = bqtoq(bq(1, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(0, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
    !rho_-1=rho_2, u_-1=-u_2, pre_-1=pre_2
    temp_q(1:3) = bqtoq(bq(2, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(-1, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))

    !wall_out
    !rho_102=rho_101, u_102=-u_101, pre_102=pre_101
    temp_q(1:3) = bqtoq(bq(101, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(102, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
    !rho_103=rho_100, u_103=-u_100, pre_103=pre_100
    temp_q(1:3) = bqtoq(bq(100, 1:3))
    q_bc(1) = temp_q(1)
    q_bc(2) = -temp_q(2)
    q_bc(3) = temp_q(3)
    bq(103, 1:3) = qtobq(q_bc(1), q_bc(2), q_bc(3))
end subroutine bound