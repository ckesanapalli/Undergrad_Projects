MODULE foil
USE general
IMPLICIT NONE

REAL (KIND = 8), PARAMETER :: pi = acos(-1d0), tol = 1d-4
INTEGER :: k, buf
TYPE :: panel
	REAL (KIND = 8), DIMENSION(2,2) :: a		! Vertices
	REAL (KIND = 8), DIMENSION(2) :: cp			! Control Point
	REAL (KIND = 8), DIMENSION(2) :: n			! Normal
	REAL (KIND = 8), DIMENSION(2) :: r			! Radial
	REAL (KIND = 8), DIMENSION(2) :: t			! Tangent
	REAL (KIND = 8), DIMENSION(2) :: vel		! Velocity
	REAL (KIND = 8), DIMENSION(2) :: tr	= 0d0	! Thrust
	REAL (KIND = 8) :: s						! Area
	REAL (KIND = 8) :: vn						! Normal Velocity
	REAL (KIND = 8) :: vt						! Tangential Velocity
	! REAL (KIND = 8) :: p						! Pressure
	! REAL (KIND = 8) :: wr = 0d0				! Rate of Work Done
END TYPE panel

! ------------------------------------------------------------	
CONTAINS

! ------------------------------------------------------------	
! ---------------*** Model Functions ***----------------------
! ------------------------------------------------------------	

! ------------------------------------------------------------
! ------------------- Designing Model ------------------------
SUBROUTINE model(n, ln, pan)
	! NACA0012 Airfoil Model
	INTEGER, INTENT(IN) :: n
	REAL (KIND=8), INTENT(IN) :: ln
	TYPE (panel), DIMENSION(n), INTENT(OUT) :: pan
	
	DO i = n/2, n
		pan(i)%a(2,1) = (2*ln/n)*(i-n/2)
		pan(i)%a(2,2) = 5* 12d-2* ( &
				& 0.2969* SQRT(pan(i)%a(2,1)/ln) &
				& - 0.1260* (pan(i)%a(2,1)/ln) &
				& - 0.3516* (pan(i)%a(2,1)/ln)**2 &
				& + 0.2843* (pan(i)%a(2,1)/ln)**3 &
				& - 0.1015* (pan(i)%a(2,1)/ln)**4 )
		pan(n-i)%a(2,1) = pan(i)%a(2,1)
		pan(n-i)%a(2,2) = - pan(i)%a(2,2)
	ENDDO
	
	DO i = 2, n
		pan(i)%a(1,:) = pan(i-1)%a(2,:)
	ENDDO
	pan(1)%a(1,:) = pan(n)%a(2,:)
	  
END SUBROUTINE model
! ------------------------------------------------------------

! ------------------------------------------------------------
! ------------ Model Coordinates at Different Time steps ---------
SUBROUTINE modelmotion(pan, pan0, n, theta, theta_, uinf)
	INTEGER, INTENT(IN) :: n
	REAL(KIND=8), INTENT(IN) :: theta, theta_
	REAL(KIND=8), DIMENSION(2), INTENT(IN) :: uinf
	TYPE (panel), DIMENSION(n), INTENT(IN) :: pan0
	TYPE (panel), DIMENSION(n), INTENT(OUT) :: pan	
	
	! Model Motion
	DO i = 1, n
		pan(i)%a(1,1) = pan0(i)%a(1,1)* cos(theta) &
						& - pan0(i)%a(1,2)* sin(theta)
		
		pan(i)%a(1,2) = pan0(i)%a(1,1)* sin(theta) &
						& + pan0(i)%a(1,2)* cos(theta)
		
		pan(i)%a(2,1) = pan0(i)%a(2,1)* cos(theta) &
						& - pan0(i)%a(2,2)* sin(theta)
		
		pan(i)%a(2,2) = pan0(i)%a(2,1)* sin(theta) &
						& + pan0(i)%a(2,2)* cos(theta)
		! Other Parameters
		CALL pan_para(pan(i))
		
		! Tangent Correction
		pan(i)%t(1) = abs( pan(i)%t(1) )
		
		! Normal Velocity
		pan(i)%vn = (mag(pan(i)%cp)* theta_)* DOT_PRODUCT &
				& ( [-SIN(theta), COS(theta)] , pan(i)%n )
		
		! IF ( i > n/2 ) THEN
		! 	pan(i)%vn = - pan(i)%vn
		! ENDIF
		
	ENDDO
	
END SUBROUTINE modelmotion
! ------------------------------------------------------------

! ------------------------------------------------------------	
SUBROUTINE pan_para(pan)

	TYPE(panel) :: pan	! Panel Parameters
	REAL (KIND = 8), DIMENSION(2) :: r
	
	pan%cp = 5d-1 * (pan%a(2,:) + pan%a(1,:))
	
	pan%t = pan%a(2,:) - pan%a(1,:)
	pan%s = mag(pan%t)
	pan%t = pan%t / pan%s
	! Inside Normal
	pan%n = (/ pan%t(2), -pan%t(1) /) 
	
END SUBROUTINE pan_para
! ------------------------------------------------------------

! ------------------------------------------------------------
! Influence Coefficient A, B
SUBROUTINE influ_ab (m, cpm, n, pan, A, B)
	
	INTEGER, INTENT(IN) :: m,n
	REAL (KIND=8), DIMENSION(m,2), INTENT(IN) :: cpm ! Control Point
	TYPE (panel), DIMENSION(n), INTENT(IN) :: pan !Panel
	REAL (KIND=8), DIMENSION(2,m,n), INTENT(OUT) :: A, B
	REAL (KIND=8) :: u_, v_, beta
	REAL(KIND=8), DIMENSION(n) :: ang
	REAL(KIND=8), DIMENSION(2) :: r1, r2
	
	DO i = 1, n
		ang(i) = ATAN2(-pan(i)%n(1), pan(i)%n(2))
	ENDDO
	
	DO i = 1, m
	DO j = 1, n
		
		r1 = ( pan(j)%a(1,:) - cpm(i,:) )
		r2 = ( pan(j)%a(2,:) - cpm(i,:) )
		
		u_ = LOG(mag(r1)/mag(r2)) / (2d0* pi)
		v_ = (ATAN2( r2(2), r2(1) ) - ATAN2( r1(2), r1(1) )) / (2d0* pi)
		
		! X and Y Influ Coeffs
		A(1,i,j) = u_* cos(ang(j)) - v_* sin(ang(j))
		A(2,i,j) = u_* sin(ang(j)) + v_* cos(ang(j))
		
		B(2,i,j) = - A(1,i,j)
		B(1,i,j) = A(2,i,j)
				
	ENDDO
	ENDDO

END SUBROUTINE influ_ab
! ------------------------------------------------------------

! ------------------------------------------------------------
! Influence Coefficient of Wake B_1
SUBROUTINE influ_b_1 (n, cpm, x_w, pan, A, B)
	
	INTEGER, INTENT(IN) :: n
	REAL (KIND=8), DIMENSION(n,2), INTENT(IN) :: cpm
	REAL (KIND=8), DIMENSION(2), INTENT(IN) :: x_w
	TYPE (panel), DIMENSION(n), INTENT(IN) :: pan
	REAL (KIND=8), DIMENSION(2,n), INTENT(OUT) :: A, B
	REAL (KIND=8) :: u_, v_, w_ang
	REAL(KIND=8), DIMENSION(2,2) :: r
	
	w_ang = ATAN2( (x_w(2) - pan(1)%a(1,2)), (x_w(1) - pan(1)%a(1,1)) )
	
	DO i = 1, n

		!*** Warning: check by interchanging r(i,1,:) to r(i,2,:)
		r(1,:) = ( x_w - cpm(i,:) )
		r(2,:) = ( pan(1)%a(1,:) - cpm(i,:) )

		u_ = LOG(mag(r(1,:))/mag(r(2,:))) / (2d0* pi)
		v_ = (ATAN2( r(2,2), r(2,1) ) - ATAN2( r(1,2), r(1,1) ) ) &
			& / (2d0* pi)
		
		A(1,i) = u_* cos(w_ang) - v_* sin(w_ang)
		A(2,i) = u_* sin(w_ang) + v_* cos(w_ang)
		
		B(2,i) = - A(1,i)
		B(1,i) = A(2,i)
		
	ENDDO

END SUBROUTINE influ_b_1
! ------------------------------------------------------------

! ------------------------------------------------------------
! Influence Coefficient C
SUBROUTINE influ_c (n, cpi, t, xvor, pan, C)
	
	INTEGER, INTENT(IN) :: n, t
	REAL (KIND=8), DIMENSION(n,2) :: cpi
	REAL (KIND=8), DIMENSION(t,2) :: xvor
	REAL (KIND=8), DIMENSION(2,n,t), INTENT(OUT) :: C
	TYPE (panel), DIMENSION(n), INTENT(IN) :: pan
	REAL (KIND=8), DIMENSION(2) :: r

	DO j = 1, t
	DO i = 1, n
		r = ( cpi(i,:) - xvor(j,:) )
		IF (mag(r) == 0d0) THEN
			r = 0d0
		ELSE
			r = r / ( 2d0* pi* mag(r) )
		ENDIF
		C(1,i,j) = r(2)
		C(2,i,j) = -r(1)
		! print*, cpi(i,:), xvor(j,:)
	ENDDO
	ENDDO

END SUBROUTINE influ_c
! ------------------------------------------------------------
END MODULE foil