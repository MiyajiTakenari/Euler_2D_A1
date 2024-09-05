
program visualize
    implicit none
    integer :: fo=20,i=0,j=0,GridNum=18,Grid=19,Qbin=100,index=1,Nx,Ny !,k=0,l=0,cnt,iold=0,jold=0
    character filename*128,meshfile*64 !,generalcoord*32,pltfile*64
    real(8),allocatable :: X(:,:),Y(:,:),Q(:,:,:),m(:,:,:),n(:,:,:)
    real(8) :: xcenter,ycenter,rho,u,v,p,t
    real(8),parameter :: GAMMA=1.4d0,PI=acos(-1.0d0)


    !マニュアル著者の環境では，格子数を"100 100"のように記載したファイルを用意して，
    !格子数を前処理，流れ場の計算，後処理，のどのプログラムからも読み出せるようにしています．
    !そのため，格子数を読みだすコードを書いています．!!格子数ファイルがGridNum.txt
    open(GridNum,file = 'GridNum.txt')
    read(GridNum,*)Nx,Ny
    close(GridNum)
    write(*,*)Nx,Ny

    allocate(X(-3:Nx+4,-3:Ny+4),Y(-3:Nx+4,-3:Ny+4),Q(-1:Nx+2,-1:Ny+2,4),& !Flag(-1:Nx+2,-1:Ny+2)
                &m(-2:Nx+3,-2:Ny+3,2),n(-2:Nx+3,-2:Ny+3,2)) !,prf(-1:Nx+2,-1:Ny+2))


    !メッシュの情報を読み込みます．
    !HOGEHOGEをメッシュの座標を記載しているファイル名に置き換えて使用してください
    !※マニュアル著者は，メッシュの座標を記載したファイルと計算結果(基本量Qの内容)が対応し，後処理の際に
    !読み込むメッシュの情報に間違いがないように，Qの結果を出力するファイルの1行目に使用したメッシュファイル名を出力しています
    !
    !# Qを出力するファイルの内容
    !   meshfile.txt (←メッシュの座標を記載したファイル名)
    !   1.0000         2.0000        3.0000       4.0000 (←以下がQの内容)
    !   1.0000         2.0000        3.0000       4.0000
    !   1.0000         2.0000        3.0000       4.0000
    !                           ・
    !                           ・
    !                           ・
    !これに従って読み込むので，40行目はQを出力したファイルの1行目，すなわちメッシュの座標を記載したファイル名を読み込む，ということになります
    !以上の内容はマニュアル著者が利便性のために行った細工なので，ご自身の環境に合わせてファイル読み込みを行ってください
    write(filename,'("HOGEHOGE_",i1.1,".dat")')index !index(integer)をfilename(char)に代入し、文字+整数を文字に変換
    open(Qbin,file = filename)
    read(Qbin,*)meshfile
    write(*,*)meshfile

    !読み込んだファイル名を用いてファイルオープン，座標を読み込んでいきます
    open(Grid,file = meshfile)
    do j=-3,Ny+4
        do i=-3,Nx+4
            read(Grid,*)X(i,j),Y(i,j)
        enddo
    enddo
    close(Grid)

    !Qの内容を読み込みます
    do j=-1,Ny+2
        do i=-1,Nx+2
            read(Qbin,*)Q(i,j,1),Q(i,j,2),Q(i,j,3),Q(i,j,4)
        enddo
    enddo
    close(Qbin)

    !paraviewに読み込ませるVTKファイルの名前です．適宜変更してください
    write(filename,'("hogehoge_",i3.3,".vtk")')index
    open(fo,file = filename)

    !VTKフォーマットに従い，69行目までは指定されています．ただし66行目のFUGAは任意のファイル名なので適宜変更してください
    write(fo,"('# vtk DataFile Version 3.0')")
    write(fo,"('FUGA')")
    write(fo,"('ASCII')")
    write(fo,"('DATASET STRUCTURED_GRID')") !非構造格子を用いる場合は 'DATASET UNSTRUCTURED GRID'　となります
    write(fo,"('DIMENSIONS',3(1x,i3))") Nx,Ny,1 !マニュアル著者は2次元の計算を行っているのでz座標の格子数は1です
    write(fo,"('POINTS ',i9,' float')") Nx*Ny    !3次元計算を行う方は Nx*Ny*Nz となります
    do j=1,Ny
        do i=1,Nx
            xcenter  = 0.25d0 *(X(i,j) + X(i-1,j) + X(i,j-1) +X(i-1,j-1))
            ycenter  = 0.25d0 *(Y(i,j) + Y(i-1,j) + Y(i,j-1) +Y(i-1,j-1))
            !xcenter = m(i,j,2) * 
            write(fo,'(3(f9.4,1x))')xcenter,ycenter,0.0d0
        enddo
    enddo

    !81~83行目まではvtkフォーマット既定のものです．そのあとは出力した座標に対応した値を出力してください
    write(fo,"('POINT_DATA',i9)")Nx*Ny
    write(fo,"('SCALARS rho float')")
    write(fo,"('LOOKUP_TABLE default')")
    do j=1,Ny
        do i=1,Nx
            write(fo,"(f10.7)")Q(i,j,1)
        enddo
    enddo

    !速度のようなベクトル場に対しては　VECTORS　という形式を指定することもできるようです
    write(fo,"('SCALARS U float')")
    write(fo,"('LOOKUP_TABLE default')")
    do j=1,Ny
        do i=1,Nx
            rho = Q(i,j,1)
            u   = Q(i,j,2)/Q(i,j,1)
            v   = Q(i,j,3)/Q(i,j,1)
            p   = (GAMMA-1.0d0)*(Q(i,j,4) - 0.5d0*rho*(u**2.0d0 + v**2.0d0))
            t   = p/rho
            write(fo,"(f10.7)")u
        enddo
    enddo

    write(fo,"('SCALARS V float')")
    write(fo,"('LOOKUP_TABLE default')")
    do j=1,Ny
        do i=1,Nx
            rho = Q(i,j,1)
            u   = Q(i,j,2)/Q(i,j,1)
            v   = Q(i,j,3)/Q(i,j,1)
            p   = (GAMMA-1.0d0)*(Q(i,j,4) - 0.5d0*rho*(u**2.0d0 + v**2.0d0))
            t   = p/rho
            write(fo,"(f10.7)")v
        enddo
    enddo

    write(fo,"('SCALARS p float')")
    write(fo,"('LOOKUP_TABLE default')")
    do j=1,Ny
        do i=1,Nx
            rho = Q(i,j,1)
            u   = Q(i,j,2)/Q(i,j,1)
            v   = Q(i,j,3)/Q(i,j,1)
            p   = (GAMMA-1.0d0)*(Q(i,j,4) - 0.5d0*rho*(u**2.0d0 + v**2.0d0))
            t   = p/rho
            write(fo,"(f10.7)")p
        enddo
    enddo
    close(fo)


    deallocate(X,Y)

end program visualize