subroutine bound
    use globals
    use interface_mod, only : qtobq, bqtoq

    implicit none
    real(8) temp_q(3), q_bc(3)
    !境界条件
    !qからq_bcを出す、その後 q_bc→bq
    !wall condition
    !wall_in
    !rho_-1=rho_0, u_-1=-u_0, pre_-1=pre_-
    temp_q(1:3) = bqtoq(bq(0, 1:3))
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