! Project: 	Flapping Foil using Vortex Panel Method
! Author: 	Chaitanya C. Kesanapalli

PROGRAM project
	USE general
	USE foil
	
	IMPLICIT NONE
	
	! For Run Time
	REAL(KIND=8) :: start, finish
	
	! ================ Input Parameters ===================

	! Foil Data (Check the foil file for the values)
	INTEGER, PARAMETER :: n = 200
	REAL(KIND=8), PARAMETER :: dl = 1d-2, ln = 1d0
	
	! Parameters
	REAL(KIND=8), PARAMETER :: &
				& w = 2*pi/(1d0) ,&			! Angular Velocity
				& amp = pi/(6d0), &			! Amplitude
				& rho = 1d3					! Fluid
				
	REAL(KIND=8), DIMENSION(2), PARAMETER :: &
				& uinf = (/ 2d0, 0d0/)! Flow speed
	
	! Divisions
	REAL(KIND=8), PARAMETER :: dt = 5d-1* (dl/ uinf(1))
	INTEGER, PARAMETER :: tn = ANINT( 1d0* ln/ (uinf(1)* dt))
	
	! Vortices limit for effect on panel (used in C matrix)
	INTEGER, PARAMETER ::  vlim = 21
	
	! ================ Other Variables ===================	
	
	! Panel
	TYPE(panel), DIMENSION(tn+1, n) :: pan
	TYPE(panel), DIMENSION(n) :: pan_buf
	REAL(KIND=8) :: peri = 0d0, theta, theta_ ,&
					& phi_t = 0d0, phi_tan = 0d0 ,&
					& pre = 0d0, gam_buf = 0d0, &
					& r_buf = 0d0, gam_buf2 = 0d0, r_buf2 = 0d0
	
	! Loop variables
	INTEGER :: s, t, lim
	
	! Wake Parameters
	REAL(KIND=8), DIMENSION(tn+1) :: r_w, ang_w
	REAL(KIND=8), DIMENSION(tn+1, 2) :: x_w, u_w
	REAL(KIND=8), DIMENSION(n, 2) :: cps
	
	! Intial state calculation parameters
	REAL(KIND=8), DIMENSION(n+1, n+1) :: Mag_ = 0d0
	REAL(KIND=8), DIMENSION(n) :: ang_ = 0d0
	REAL(KIND=8), DIMENSION(n+1) :: str = 0d0
	
	! Parameters
	REAL(KIND=8), DIMENSION(tn+1, n) :: sigp = 0d0
	REAL(KIND=8), DIMENSION(tn+1) :: gam = 0d0, gam_w = 0d0
	
	! Influence Coefficients of Foil	
	REAL(KIND=8), DIMENSION(2, n, n) :: Ap = 0d0, Bp = 0d0
	REAL(KIND=8), DIMENSION(2, n) :: Ap_1 = 0d0, Bp_1 = 0d0
	REAL(KIND=8), DIMENSION(2, n, vlim) :: Cp = 0d0, cbuf = 0d0
	
	! Influence Coefficients of Wake Panel
	REAL(KIND=8), DIMENSION(2, n) :: Aw = 0d0, Bw = 0d0
	REAL(KIND=8), DIMENSION(2,vlim) :: Cw = 0d0
	
	! Influence Coefficient of Vortices
	! (time, num, axes)
	REAL(KIND=8), DIMENSION(3:tn+2,tn-1,2) :: x_vor = 0d0, u_vor = 0d0, u_buf = 0d0
	REAL(KIND=8), DIMENSION(:, :), ALLOCATABLE :: x_vbuf, Avor_1, Bvor_1
	REAL(KIND=8), DIMENSION(:, :, :), ALLOCATABLE  :: Avor, Bvor, Cvor
	REAL(KIND=8), DIMENSION(vlim) :: ccl = 0d0
	
	! Output Parameters
	CHARACTER(100) :: link
	REAL(KIND=8), DIMENSION(tn+1, 2) :: tipbuf = 0d0
	REAL(KIND=8), DIMENSION(n, 2) :: panbuf = 0d0
	
	! ===========================================================
	! ----------------------- PROGRAM ---------------------------
	! ===========================================================
	print*, "Number of Divisions and time steps: ", n, tn
	
	! Model
	CALL model(n, ln, pan(1,:))
	
	! ===========================================================
	! Main Time Loop
	DO t = 1, tn+1
	! print*, "Time Step:", t, "/", tn+1
	
		IF (t < vlim) THEN
			lim = t
		ELSE
			lim =  vlim
		ENDIF
		
		! ===========================================================
		theta = amp* sin(w*dt*(t-1))
		theta_ = w * amp* cos(w*dt*(t-1))

		! ===========================================================
		! Model Functions --------------------------------------------
		CALL modelmotion(pan(t,:), pan(1,:), n, theta, theta_, uinf)
		
		! ===========================================================
		! PROCESS
		
		DO i =1, n
			peri = peri+ mag(pan(t,i)%a(2,:)-pan(t,i)%a(1,:))
			cps(i,:) = pan(t,i)%cp
			ang_(i) = ATAN2(-pan(t,i)%n(1), pan(t,i)%n(2))
		ENDDO
		
		! ===========================================================
		! ----- At Intial State i.e., T = 0s
		IF (t == 1) THEN
			! The Ap and Bp values do not change because it is a foil
			! shape phenomenon (but it will change in the undulating foil )
			
			pan_buf = pan(t,:)
			! Influence Coefficient
			CALL influ_ab(n, cps, n, pan_buf, Ap, Bp)
			
			DO i = 1, n
			DO j = 1, n
				Ap(1,i,j) = Bp(1,i,j)* sin(ang_(i)) - Bp(2,i,j)* cos(ang_(i))
				Ap(2,i,j) = Bp(1,i,j)* cos(ang_(i)) + Bp(2,i,j)* sin(ang_(i)) 
				Bp(2,i,j) = - Ap(1,i,j)
				Bp(1,i,j) = Ap(2,i,j)
			ENDDO
			ENDDO
			! ===========================================================
			DO i= 1, n
				str(i) = mag(uinf)* sin(ang_(i)- theta)
			ENDDO
			str(n+1) = - mag(uinf)* ( cos(ang_(1)- theta) + cos(ang_(n)- theta) )
			! ===========================================================
			Mag_ = 0d0
			DO j= 1, n
				DO i= 1, n
					Mag_(i,j) = Ap(2,i,j)
					Mag_(i,n+1) = Mag_(i,n+1) + Bp(2,i,j)
				ENDDO
				Mag_(n+1,j) = Ap(1,1,j) + Ap(1,n,j)
				Mag_(n+1,n+1) = Mag_(n+1,n+1) + Bp(1,1,j) + Bp(1,n,j)
				
				! Julia bustos Thesis has the above equation in 23 24 pages wrong
				! I think the above is correct
			ENDDO
			! ===========================================================
			! Solving Equation of the Mag_* str = velmat matrix
			
			CALL Solequ(n+1, Mag_, str)
			
			! ===========================================================
			! Source Strength 
			DO i = 1, n
				sigp(t,i) = str(i)
			ENDDO
			! Vorticity Strength
			gam(t) = str(n+1)
			
			! Circulation at zero sec
			ccl(t) = gam(t)* peri
			! ===========================================================
			! Tangential Velocity at zero sec
			pan(t,:)%vt = matmul( Ap(1,:,:), sigp(t,:) ) + &
						& gam(t)* SUM( Bp(2,:,:), DIM=2 )
			
		ELSE
			! ==================== ALLOCATION ===========================
			ALLOCATE( Avor(2,t-1,n), x_vbuf(t-1,3), Bvor(2,t-1,n), Avor_1(2,t-1), Bvor_1(2,t-1), Cvor(2,t-1,vlim) )
			x_vbuf = 0d0
			Avor = 0d0
			Bvor = 0d0
			Avor_1 = 0d0
			Bvor_1 = 0d0
			Cvor = 0d0
			
			! ===========================================================
			! ----- Iterations for Wake Panel Length Convergence 
			IF (t == 2) THEN
				r_w(t) = dl
			ELSE
				r_w(t) = r_w(t-1)
			ENDIF
			print*, 
			ang_w(t) = - theta
			r_buf = 0d0
			! gam(t) = 1d0
			gam(t) = 5d-2
			
			! ===========================================================
			DO WHILE ( abs( 1d0 - r_buf/r_w(t) ) > tol)	! Whlie Loop 1 -------------------
				r_buf2 = r_buf
				r_buf = r_w(t)
				! Wake Midpoint
				x_w(t,1) = pan(t,1)%a(1,1) + r_w(t)* cos(ang_w(t))
				x_w(t,2) = pan(t,1)%a(1,2) + r_w(t)* sin(ang_w(t))
				
				! ===========================================================
				! Influence Coefficient of Wake B_1
				CALL influ_b_1 (n, cps, x_w, pan(t,:), Ap_1, Bp_1)

				DO i = 1, n
					Ap_1(1,i) = Bp_1(1,i)* sin(ang_(i)) - Bp_1(2,i)* cos(ang_(i))
					Ap_1(2,i) = Bp_1(1,i)* cos(ang_(i)) + Bp_1(2,i)* sin(ang_(i))
					Bp_1(2,i) = - Ap_1(1,i)
					Bp_1(1,i) = Ap_1(2,i)
				ENDDO
				
				! ===========================================================
				! Influence Coefficient of Shed Voritices
				
				IF (t < 3) THEN
					Cp = 0d0
				ELSE
					DO k = 1, t-1
						x_vbuf(k,:) = x_vor(t,k,:)
					ENDDO
					CALL influ_c (n, cps, lim, x_vbuf, pan(t,:), cbuf)
					DO i = 1, n
					DO j = 1, lim
						Cp(1,i,j) = cbuf(1,i,j)* cos(ang_(i)) + cbuf(2,i,j)* sin(ang_(i))
						Cp(2,i,j) = - cbuf(1,i,j)* sin(ang_(i)) + cbuf(2,i,j)* cos(ang_(i))
					ENDDO
					ENDDO
				ENDIF

				! ===========================================================
				! ---- Newton's Method to solve Quadratic equation
				
				gam_buf = 0d0
				
				DO WHILE (abs(gam(t)-gam_buf) > tol)	! Whlie Loop 2 -------------------
					gam_buf2 = gam_buf
					gam_buf = gam(t)
					!*** Warning: Check sign convention ----------------------
					! Vorticity on Wake Element
					gam_w(t) = peri* (gam(t-1)- gam(t))/ r_w(t)
					IF (t <= vlim) THEN
						ccl(t) = peri* gam(t)
					ENDIF
					
					! ===========================================================
					DO i=1, n
						cps(i,:) = pan(t,i)%n
					ENDDO
					
					!*** Warning: Check the thesis Is it UKn or UKn(i)
					sigp(t,:) = - ( SUM(Bp(2,:,:), DIM=2)* gam(t) + &
							& Bp_1(2,:)* gam_w(t) + &
							& MATMUL( Cp(2,:,:), ccl ) + &
							& ( MATMUL(cps,uinf) - pan(t,:)%vn ) )
					
					CALL solequ(n, Ap(2,:,:), sigp(t,:))
					! ===========================================================
					DO i=1, n
						cps(i,:) = pan(t,i)%t
					ENDDO
					
					pan(t,1)%vt = SUM( Ap(1,1,:)* sigp(t,:) ) + &
								& SUM( Bp(1,1,:) )* gam(t) + &
								& Bp_1(1,1)* gam_w(t) + &
								& SUM( Cp(1,1,:)* ccl ) + &
								& DOT_PRODUCT( cps(1,:), uinf )

					pan(t,n)%vt = SUM( Ap(1,n,:)* sigp(t,:) ) + &
								& SUM( Bp(1,n,:) )* gam(t) + &
								& Bp_1(1,n)* gam_w(t) + &
								& SUM( Cp(1,n,:)* ccl ) + &
								& DOT_PRODUCT( cps(n,:), uinf )
					
					gam(t) = gam(t-1) + (dt/ (2*peri))* &
							& ( pan(t,1)%vt**2 - pan(t,n)%vt**2 &
							& + pan(t,1)%vn**2 - pan(t,n)%vn**2 )
					
					print*, "gam = ", gam(t), gam_buf

					IF (abs(gam(t)) > 1d7) THEN
						print*, "Error in Whlie Loop 2: The gamma is diverging"
						EXIT
					ENDIF
				
				ENDDO									! Whlie Loop 2 end -------------------
				! ===========================================================
				! Wake Influence Coefficient
				CALL influ_ab (1, x_w, n, pan, Aw, Bw)

				IF (t < 3) THEN
					Cw = 0d0
				ELSE
					! Influence Coefficient of Shed Voritices
					CALL influ_c (1, x_w, lim, x_vbuf, pan(t,:), Cw)
				ENDIF

				! ===========================================================
				! Local Resultant Velocity
				
				u_w(t,1) = SUM( Aw(1,:)* sigp(t,:) ) + &
						 & SUM( Bw(1,:) )* gam(t) + &
						 & SUM( Cw(1,:)* &
						 & ccl ) + uinf(1)
				
				u_w(t,2) = SUM( Aw(2,:)* sigp(t,:) ) + &
						 & SUM( Bw(2,:) )* gam(t) + &
						 & SUM( Cw(2,:)* &
						 & ccl )+ uinf(2)
				
				r_w(t) = dt* mag(u_w(t,:))
				ang_w(t) = atan(u_w(t,2)/u_w(t,1))
				
				print*, "r_w = ", r_w(t), r_buf
				IF (abs(r_w(t)) > 1d7) THEN
					print*, "Error in Whlie Loop 1: The wake_length is diverging"
					EXIT
				ENDIF
			ENDDO										! Whlie Loop 1 end -------------------
			
			DO i=1, n
				cps(i,:) = pan(t,i)%t
			ENDDO
			pan(t,:)%vt = MATMUL( Ap(1,:,:), sigp(t,:) ) + &
						& SUM( Bp(1,:,:), DIM=2 )* gam(t) + &
						& Bp_1(1,:)* gam_w(t) + &
						& MATMUL( Cp(1,:,:), &
						& ccl ) + &
						! & ( MATMUL( cps, uinf ) - pan(t,:)%vt )
						& ( MATMUL( cps, uinf ) )
			
				! DO i=1, n
				! 	cps(i,:) = pan(t,i)%n
				! ENDDO
				! pan(t,:)%vn = MATMUL( Ap(4,:,:), sigp(t,:) ) + &
				! 			& SUM( Bp(4,:,:), DIM=2 )* gam(t) + &
				! 			& Bp_1(4,:)* gam_w(t) + &
				! 			& MATMUL( Cp(1,:,:), &
				! 			& ccl ) + &
				! 			! & ( MATMUL( cps, uinf ) - pan(t,:)%vn )
				! 			& ( MATMUL( cps, uinf ) )
			
			! Wake Midpoint
			x_w(t,1) = pan(t,1)%a(1,1) + r_w(t)* cos(ang_w(t))
			x_w(t,2) = pan(t,1)%a(1,2) + r_w(t)* sin(ang_w(t))

			x_vor(t+1,t-1,1) =  pan(t,1)%a(1,1)+ 5d-1* r_w(t)* cos(ang_w(t))+ u_w(t,1)* dt
			x_vor(t+1,t-1,2) =  pan(t,1)%a(1,2)+ 5d-1* r_w(t)* sin(ang_w(t))+ u_w(t,2)* dt
			
			! Influence Coefficients of Vortices
			CALL influ_ab (t-1, x_vbuf, n, pan(t,:), Avor, Bvor)
			
			! Influence Coefficient of Wake B_1
			CALL influ_b_1 (t-1, x_vbuf, x_w, pan, Avor_1, Bvor_1)
			
			! Influence Coefficient of Shed Voritices
			IF (t < 3) THEN
				Cvor = 0d0
			ELSE
				CALL influ_c (t-1, x_vbuf, lim, x_vbuf, pan(t,:), Cvor)
			ENDIF

			! Vorticies Velocities
			
			u_vor(t,1:t-1,1) = MATMUL( Avor(1,:,:), sigp(t,:) ) + &
						& SUM( Bvor(1,:,:), DIM=2 )* gam(t) + &
						& Bvor_1(1,:)* gam_w(t) + &
						& MATMUL( Cvor(1,:,:), ccl ) + uinf(1)
			
			u_vor(t,1:t-1,2) = MATMUL( Avor(2,:,:), sigp(t,:) ) + &
						& SUM( Bvor(2,:,:), DIM=2 )* gam(t) + &
						& Bvor_1(2,:)* gam_w(t) + &
						& MATMUL( Cvor(2,:,:), ccl ) + uinf(2)
			
			! Vorticies Coordinates
			DO k =1, t-2
				x_vor(t+1,k,:) = x_vor(t,k,:) + u_vor(t,k,:)*dt
			ENDDO
			
			DEALLOCATE( x_vbuf, Avor, Bvor, Avor_1, Bvor_1, Cvor )

		ENDIF
		
	ENDDO												! Time Loop end -------------------
	
	print*, "Writing Files"
	DO k = 1, 2
		link = '_'//trim(adjustl(achar(k+119)))
		CALL output2d( pan(:,:)%cp(k), tn+1, n, 'cp'//trim(adjustl(link)) )
		CALL output2d( pan(:,:)%a(1,k), tn+1, n, 'pana'//trim(adjustl(link)) )
		CALL output2d( pan(:,:)%t(k), tn+1, n, 'tang'//trim(adjustl(link)) )
		CALL output2d( pan(:,:)%n(k), tn+1, n, 'norm'//trim(adjustl(link)) )
	ENDDO

	! DO k = 3,tn+2
		! Write( link, '(i10)' ) k
		! link = '-'//trim(adjustl(link))
		! CALL output2d( x_vor(k,:,:), tn-1, 2, 'vort'//trim(adjustl(link)) )	
	! ENDDO

	! DO k = 3,tn+2
	! 	Write( link, '(i10)' ) k
	! 	link = '-'//trim(adjustl(link))
	! 	CALL output2d( u_vor(k,:,:), tn-1, 2, 'vort_u'//trim(adjustl(link)) )
	! ENDDO

	panbuf(:,1) = pan(1,:)%a(1,1)
	panbuf(:,2) = pan(1,:)%a(1,2)
	
	CALL output2d( x_w, tn+1, 2, 'wakepanel' )

	CALL output2d( pan(:,:)%vn, tn+1, n, 'vn' )
	CALL output2d( pan(:,:)%vt, tn+1, n, 'vt' )
	CALL output1d((/ (dt*t, t = 0, tn) /), tn+1, "Time")
	CALL output1d((/ (dl*i, i = 1, n) /), n, "Panels")
	
	CALL output2d( panbuf, n, 2, 'pan%a')

END PROGRAM project