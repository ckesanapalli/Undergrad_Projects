! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
! A fortran 95 program for G95
! By Chaitanya Chowdary, NA12B014
!
! OE5450 - Numerical Techniques in Ocean Hydrodynamics
! Final Project
! Implementation of level-set method for solution of bubble in a cavity
! -------------------------------------------------------------------------
! -------------------------------------------------------------------------

program Bubble_by_levelset
  implicit none
  character(100) :: link, ns
  integer :: i, j, k, it

  integer, parameter :: time_steps=30, it_num=200, nx=32, ny=32

  ! Variables for Rectangular Box Values and its Conditions
  real, parameter :: B=1.0, H=1.0, D1=1000.0, D2=1.023, gx=0.0, gy=-10.0

  ! Variables for Grid, Time Step and Initial Values
  real, parameter :: dx=B/nx, dy=H/ny, dt=0.01, rad=0.15, xc=0.5, yc=0.3,

  ! Variables for SAMPLE Method
  real, parameter ::  m0=0.01, rro=D1, maxError=0.001, beta=1.2, lrg=1000

  ! Variables for Level Set Interface
  real, parameter :: dt_=.5/(1/dx +1/dy), alp = dx/2, eps=0.01, pi=3.14

  ! Variables for Miscellaneous Values
  real :: max_num, temp, dxl, dxr, dyl, dyr, del_r, del_l

  real, dimension(nx+2):: x, y
  real, dimension(nx+1,nx+2):: u, ut
  real, dimension(nx+2,nx+1):: v, vt
  real, dimension(nx+1,nx+1):: uu, vv
  real, dimension(nx+2,nx+2):: tmp1, tmp2, p, r, ro, oldArray, rt, phi, phio

  link = "Outputs\"

  u = 0
  v = 0
  ut = 0
  vt = 0
  uu = 0
  vv = 0
  tmp1 = 0
  tmp2 = 0
  p = 0
  oldArray = 0
  r = D1
  rt = 0
  phi = 0
  max_num = 0
  phio=0


!================================ GRID FORMATION ======================================

  do i=1, nx+2
    x(i)=dx*(i-1.5)
  end do

  do j=1, ny+2
    y(j)=dy*(j-1.5)
  end do

  open(1,file= trim(adjustl(link))//'grid.csv')
  do i=2,max(nx,ny)
    write(1,*), x(i),",",y(i)
  end do
  close(1)


  !================================= ITERATIONS =====================================

  do k=0, time_steps

  !==================== INITIAL STATE OF LEVEL SET FUNCTION =========================
    if (k==0) then

      do j=1, ny+2
        do i=1, nx+2
          phi(i,j) = sqrt((x(i)-xc)**2 + (y(j)-yc)**2 ) - rad
        end do
      end do

      do i=2,nx+1
        do j=2,ny+1
          if ((x(i)-xc)**2+(y(j)-yc)**2 < rad**2) then
            r(i,j)=D2
          endif
        end do
      end do


  !====================== ITERATIONS FOR BUBBLE INTERFACE ===========================

    else
  !============================ SIMPLE SOLVER METHOD ================================

      ! Temporary u-velocity
      do i=2, nx
        do j=2, ny+1
          ut(i,j)=u(i,j)+dt*(-0.25*(((u(i+1,j)+u(i,j))**2-(u(i,j)+ &
          & u(i-1,j))**2)/dx+((u(i,j+1)+u(i,j))*(v(i+1,j)+ &
          & v(i,j))-(u(i,j)+u(i,j-1))*(v(i+1,j-1)+v(i,j-1)))/dy)+ &
          & m0/(0.5*(r(i+1,j)+r(i,j)))*( &
          & (u(i+1,j)-2*u(i,j)+u(i-1,j))/dx**2+ &
          & (u(i,j+1)-2*u(i,j)+u(i,j-1))/dy**2 )+ gx )

        end do
      end do

      ! Temporary v-velocity
      do i=2, nx+1
        do j=2, ny
          vt(i,j)=v(i,j)+dt*(-0.25*(((u(i,j+1)+u(i,j))*(v(i+1,j)+ &
          & v(i,j))-(u(i-1,j+1)+u(i-1,j))*(v(i,j)+v(i-1,j)))/dx+ &
          &((v(i,j+1)+v(i,j))**2-(v(i,j)+v(i,j-1))**2)/dy)+ &
          & m0/(0.5*(r(i,j+1)+r(i,j)))*( &
          & (v(i+1,j)-2*v(i,j)+v(i-1,j))/dx**2+ &
          & (v(i,j+1)-2*v(i,j)+v(i,j-1))/dy**2 )+ gy )
        end do
      end do

      ! Compute source term and the coefficient for p(i,j)
      rt=r

      do i=1,nx+2
        rt(i,1)=lrg
        rt(i,ny+2)=lrg
      end do

      do i=1,nx+2
        rt(1,i)=lrg
        rt(nx+2,i)=lrg
      end do

      do i=2, nx+1
        do j=2, ny+1
          tmp1(i,j)= (0.5/dt)*( (ut(i,j)-ut(i-1,j))/dx + (vt(i,j)-vt(i,j-1))/dy )
          tmp2(i,j)=1.0/( (1./dx)*( 1./(dx*(rt(i+1,j)+rt(i,j)))+&
                  & 1./(dx*(rt(i-1,j)+rt(i,j))) )+&
                  & (1./dy)*(1./(dy*(rt(i,j+1)+rt(i,j)))+&
                  & 1./(dy*(rt(i,j-1)+rt(i,j))) ) )
        end do
      end do

      ! Solving the Pressure Equation
      max_num = 1

      do it=1,it_num
        max_num = 0
        oldArray = p

        do i=2, nx+1
          do j=2, ny+1
            p(i,j) = (1.0-beta)*p(i,j)+beta* tmp2(i,j)*(&
                   & (1./dx)*( p(i+1,j)/(dx*(rt(i+1,j)+rt(i,j)))+&
                   & p(i-1,j)/(dx*(rt(i-1,j)+rt(i,j))) )+&
                   & (1./dy)*( p(i,j+1)/(dy*(rt(i,j+1)+rt(i,j)))+&
                   & p(i,j-1)/(dy*(rt(i,j-1)+rt(i,j))) ) - tmp1(i,j))
            max_num = max(abs(oldArray(i,j)-p(i,j)),max_num)
          end do
        end do

        if (max_num < maxError) then
          exit
        endif

      end do

      ! Correction of the u-velocity
      do i=2,nx
        do j=2,ny+1
          u(i,j)=ut(i,j) - dt*(2.0/dx)*(p(i+1,j)-p(i,j))/(r(i+1,j)+r(i,j))
        end do
      end do

      ! Correction of the v-velocity
      do i=2,nx+1
        do j=2,ny
          v(i,j)=vt(i,j)-dt*(2.0/dy)*(p(i,j+1)-p(i,j))/(r(i,j+1)+r(i,j))
        end do
      end do

       ! Half Velocities
      do i=1,nx+1
        do j=1,ny+1
          uu(i,j)=0.5*(u(i,j+1)+u(i,j))
          vv(i,j)=0.5*(v(i+1,j)+v(i,j))
        end do
      end do

  !============================ LEVEL SET FUNCTION ===============================
      do j=2, ny
        do i=2, nx
          dxl = (phi(i,j) - phi(i-1,j))/dx
          dxr = (phi(i+1,j) - phi(i,j))/dx
          dyl = (phi(i,j) - phi(i,j-1))/dy
          dyr = (phi(i,j+1) - phi(i,j))/dy
          del_r = sqrt( max(dxl,0.0)**2 + min(dxr,0.0)**2 + max(dyl,0.0)**2 + min(dyr,0.0)**2 )
          del_l = sqrt( min(dxl,0.0)**2 + max(dxr,0.0)**2 + min(dyl,0.0)**2 + max(dyr,0.0)**2 )

!          phi(i,j) = phi(i,j) + dt*( max((u(i,j)*dxl + v(i,j)*dyl)/sqrt((dxl)**2+(dyl)**2),0.0)*del_r + &
!                                   & min((u(i,j)*dxl + v(i,j)*dyl)/sqrt((dxl)**2+(dyl)**2),0.0)*del_l )
!          phi(i,j) = phi(i,j) - dt*( (u(i+1,j)+2*u(i,j)+u(i-1,j))*&
!                    &(phi(i+1,j)-phi(i-1,j))/(4*dx) + &
!                    &(v(i,j+1)+2*v(i,j)+v(i,j-1))*&
!                    &(phi(i,j+1)-phi(i,j-1))/(4*dy) )
                end do
              end do
              phio=phi;
              do i=2,nx+1
                do j=2,ny+1
                  phi(i,j)=phio(i,j)-(0.5*dt/dx)*(u(i,j)*(phio(i+1,j)&
                      & +phio(i,j))-u(i-1,j)*(phio(i-1,j)+phio(i,j)) )&
                      & -(0.5* dt/dy)*(v(i,j)*(phio(i,j+1)&
                      & +phio(i,j))-v(i,j-1)*(phio(i,j-1)+phio(i,j)) )&
                      & +(m0*dt/dx/dx)*(phio(i+1,j)-2.0*phio(i,j)+phio(i-1,j))&
                      & +(m0*dt/dy/dy)*(phio(i,j+1)-2.0*phio(i,j)+phio(i,j-1))


  ! ----------------- Reinitialization of level Set Function -----------------
          do while ( abs(phi(i,j) - temp) < eps )

            temp = phi(i,j)
            dxl = (phi(i,j) - phi(i-1,j))/dx
            dxr = (phi(i+1,j) - phi(i,j))/dx
            dyl = (phi(i,j) - phi(i,j-1))/dy
            dyr = (phi(i,j+1) - phi(i,j))/dy

            if (temp > 0) then
              phi(i,j) =  temp + dt_*( 1 - sqrt( max( max(dxl,0.0)**2,min(dxr,0.0)**2 ) + max( max(dyl,0.0)**2,min(dyr,0.0)**2 ) ) )
            elseif (temp < 0) then
              phi(i,j) = temp + dt_*( -1 + sqrt( max( min(dxl,0.0)**2,max(dxr,0.0)**2 ) &
                            &+ max( min(dyl,0.0)**2,max(dyr,0.0)**2 ) ) )
            else
              phi(i,j) = temp
            endif

          end do
        end do
      end do


    end if

  ! ----------------- Density values -----------------
    do j=1, ny+2
      do i=1, nx+2
        phi(1,j) = phi(2,j)
        phi(i,1) = phi(i,2)
        phi(nx+2,j) = phi(nx+1,j)
        phi(i,ny+2) = phi(i,ny+1)
      end do
    end do

!======= ADVECT DENSITY using centered difference plus diffusion ==========
    ro=r;
    do i=2,nx+1
      do j=2,ny+1
        r(i,j)=ro(i,j)-(0.5*dt/dx)*(u(i,j)*(ro(i+1,j)&
            & +ro(i,j))-u(i-1,j)*(ro(i-1,j)+ro(i,j)) )&
            & -(0.5* dt/dy)*(v(i,j)*(ro(i,j+1)&
            & +ro(i,j))-v(i,j-1)*(ro(i,j-1)+ro(i,j)) )&
            & +(m0*dt/dx/dx)*(ro(i+1,j)-2.0*ro(i,j)+ro(i-1,j))&
            & +(m0*dt/dy/dy)*(ro(i,j+1)-2.0*ro(i,j)+ro(i,j-1))
      end do
    end do


    write(ns,'(I5)') k
    open(2,file= trim(adjustl(link))//'density\'//trim(adjustl(ns))//'.csv')
    open(3,file= trim(adjustl(link))//'dist_fn\'//trim(adjustl(ns))//'.csv')

    do j=2,ny
      write(2,*), (r(j,i),",",i=2,nx)
      write(3,*), (phi(j,i),",",i=2,nx)
    end do

    close(2)
    close(3)

  end do

end program Bubble_by_levelset
