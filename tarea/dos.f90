!program dos19
!    implicit none
!    REAL :: b,a,r
!    WRITE (*,*) "Ingresa la base del logaritmo seguida del argumento"
!    READ (*,*) b,a
!    r=LOG(a)/LOG(b)
!    WRITE (*,*) "El resultado es",r
!end program

!#3-5
!program tres5
!    implicit none
!    REAL :: w,c
!
!    WRITE (*,*) "Write the package weight in pounds"
!    READ (*,*) w
!    c=0
!    IF (w<=100) THEN
!        IF (w>2) THEN
!            c=c+12+4*(w-2)
!        ELSE
!            c=c+12
!        END IF
!        IF (w>70) THEN
!            c=c+10
!        END IF
!        WRITE (*,*) "The cost of mailing the package is: $",c
!    ELSE
!        WRITE (*,*) "No package over 100 pounds will be accepted"
!    END IF
!end program tres5

!#3-7
!PROGRAM tres7
!    IMPLICIT NONE
!    REAL::x,y,fun
!
!    WRITE (*,*) 'Enter the values of x and y'
!    READ (*,*) x,y
!
!    ge0: IF (x>=0) THEN
!        gy: IF (y>=0) THEN
!            fun=x+y
!            ELSE IF (y<0) THEN gy
!            fun=x+y**2
!            END IF gy
!         ELSE IF (x<0) THEN ge0
!         gy2: IF (y>=0) THEN
!              fun=x**2+y
!              ELSE IF (y<0) THEN gy2
!              fun=x**2+y**2
!              END IF gy2
!         END IF ge0
!     WRITE (*,*) 'El valor de la funci�n f(x,y) es ',fun
!END PROGRAM tres7

!#3-12
!PROGRAM tres12
!    IMPLICIT NONE
!    REAL::n1,n2,ia,ra
!
!    DO
!    WRITE (*,*) 'The indices of refraction n1 and n2 are:'
!    READ (*,*) n1,n2
!    IF (n1>n2) THEN
!        WRITE(*,*) 'All the light is reflected back'
!    ELSE
!        WRITE(*,*) 'The angle of incidence is(in degrees):'
!        READ(*,*) ia
!        ia=ia*3.141592/180
!        ra=ASIN(n1/n2*SIN(ia))
!        ra=ra*180/3.141592
!        WRITE(*,*)'The angle of refraction is(in degrees):',ra
!    END IF
!    END DO
!END PROGRAM tres12


!PROGRAM piloto
! REAL,DIMENSION(3)::a,r
! a=(/1,2,3/)
! r=(/3,4,5/)
! IF (a(3)==(r(i),i=1,3)) THEN
!    WRITE(*,*)'work'
! END IF
!END PROGRAM piloto




