
MODULE general
IMPLICIT NONE

! ------------------------------------------------------------	
INTEGER :: i, j

! ------------------------------------------------------------	
CONTAINS

! ------------------------------------------------------------	
! ---------------*** Basic Functions ***----------------------
! ------------------------------------------------------------	

! ------------------------------------------------------------	
FUNCTION cross(a, b)
  REAL (KIND = 8), DIMENSION(3) :: cross
  REAL (KIND = 8), DIMENSION(3), INTENT(IN) :: a, b

  cross(1) = a(2) * b(3) - a(3) * b(2)
  cross(2) = a(3) * b(1) - a(1) * b(3)
  cross(3) = a(1) * b(2) - a(2) * b(1)
END FUNCTION cross
! ------------------------------------------------------------

! ------------------------------------------------------------	
FUNCTION aprox(a)
  REAL (KIND = 8), PARAMETER :: endit = 1d-6
  REAL (KIND = 8), INTENT(IN) :: a
  REAL(KIND=8) :: aprox
  
  IF (ABS(a) < endit) THEN
	aprox = 0d0
  ELSE
	aprox = a
  ENDIF

END FUNCTION aprox
! ------------------------------------------------------------

! ------------------------------------------------------------
FUNCTION mag(a)
  
  REAL (KIND = 8) :: mag
  REAL (KIND = 8), DIMENSION(2), INTENT(IN) :: a

  mag = SQRT( DOT_PRODUCT(a,a) )
  
END FUNCTION mag
! ------------------------------------------------------------

! ------------------------------------------------------------
SUBROUTINE output1d(x, n, string)

	INTEGER, INTENT(IN) :: n
	REAL (KIND = 8), DIMENSION (n), INTENT(IN) :: x
	CHARACTER(*), INTENT(IN) :: string
	
	OPEN(UNIT = 1, FILE = TRIM('../outputs/'// TRIM(string) //'.dat' ))

	DO i=1, n
		write(1,*) x(i)
	ENDDO
	
	close(UNIT = 1)
	
END SUBROUTINE output1d
! ------------------------------------------------------------

! ------------------------------------------------------------
SUBROUTINE output2d(x, m, n, string)

	INTEGER, INTENT(IN) :: m, n
	REAL (KIND = 8), DIMENSION (:,:),INTENT(IN) :: x
	CHARACTER(*), INTENT(IN) :: string
	REAL (KIND = 8), DIMENSION (m,n) :: check
	
	! check = ( maxval(x,dim=1) - minval(x,dim=1) )
	! IF ( check > 0.001) THEN
		OPEN(UNIT = 1, FILE = TRIM('../outputs/'// TRIM(string) //'.dat' ))
	! ENDIF
	
	! check = ( maxval(x,dim=2)  - minval(x,dim=2) )
	! IF ( check > 0.001) THEN
		IF (n > 2) THEN 
			OPEN(UNIT = 2, FILE = TRIM('../outputs/transp/'// TRIM(string) //'_inv.dat' ))
		ENDIF
	! ENDIF
	
	DO i=1,m
		write(1,*) ( x(i,j), j=1,n )
	ENDDO
	
	DO i=1,n
		write(2,*) ( x(j,i), j=1,m )
	ENDDO
	
	close(UNIT = 1)
	close(UNIT = 2)
	
	
END SUBROUTINE output2d
! ------------------------------------------------------------

! ------------------------------------------------------------
! Solving Equation of the Ax = B matrix form (Here B in x)
SUBROUTINE solequ (n, A, x)
	INTEGER, INTENT(IN) :: n
	REAL (KIND=8), DIMENSION(n,n), INTENT(IN) :: A
	REAL (KIND=8), DIMENSION(n), INTENT(INOUT) :: x
	REAL (KIND=8), DIMENSION(n,n) :: A_temp
	REAL (KIND=8), DIMENSION(n) :: B, B_
	REAL (KIND=8), DIMENSION(n) :: ipiv
	REAL (KIND=8) :: info
	
	A_temp = A
	B = x
	CALL dgesv(n, int(1), A_temp, n, ipiv, x, n, info)
	! A_temp is destroyed
	! ===========================================================
	! Message
	! print*, "PIVOT = ", IPIV
	IF(INFO == 0) THEN
		! print*, "Successful Inversion"
	ELSEIF (INFO < 0) THEN
		print*, "Inversion Failed:", i, "th argument had an illegal value"
	ELSE
		print*, "Inversion Failed: the factor U is exactly singular", &
				& ", so the solution could not be computed"
	ENDIF
	! ===========================================================
	! Inversion Verification
	
	B_ = MATMUL(A, x) ! = B
	DO i = 1, n
		IF (ABS( (B(i) - B_(i)) ) > 1d-6) THEN
			print*, "Verfication error at ", i, ":", &
				& B(i), "-", B_(i), "=", B(i)-B_(i)
			print*, "Inversion Error"
			EXIT
		ENDIF
	ENDDO
	IF (i == n+1) THEN
		! print*, "Inversion Verfied"
	ENDIF

END SUBROUTINE solequ
! ------------------------------------------------------------

END MODULE general
