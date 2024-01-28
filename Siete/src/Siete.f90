!REAL FUNCTION f(x)
!    IMPLICIT NONE
!    REAL,INTENT(IN)::x
!    f=x**2
!END FUNCTION
!
!REAL FUNCTION g(x)
!    IMPLICIT NONE
!    REAL,INTENT(IN)::x
!    g=EXP(x)
!END FUNCTION
!
!REAL FUNCTION h(x)
!    IMPLICIT NONE
!    REAL,INTENT(IN)::x
!    h=SIN(x)
!END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!
!SUBROUTINE compuesta(f,g,h,x0,res)
!    IMPLICIT NONE
!    EXTERNAL::f,g,h
!    REAL,INTENT(IN)::x0
!    REAL,INTENT(OUT)::res
!    REAL::hx,gh,fcomp
!    CALL h(x0,hx)
!    CALL g(hx,gh)
!    CALL f(gh,fcomp)
!    res=fcomp
!    RETURN
!END SUBROUTINE
!
!PROGRAM trescomp
!    IMPLICIT NONE
!    EXTERNAL::f,g,h
!    REAL::x,fcomp
!    WRITE(*,*)'Introduzca x'
!    READ(*,*)x
!    CALL compuesta(f,g,h,x,fcomp)
!    WRITE(*,*)'f(g(h(',x,')=',fcomp
!END PROGRAM
!
!SUBROUTINE f(x,y)
!    REAL,INTENT(IN)::x
!    REAL,INTENT(OUT)::y
!    y=x**2
!    RETURN
!END SUBROUTINE
!
!SUBROUTINE g(x,y)
!    REAL,INTENT(IN)::x
!    REAL,INTENT(OUT)::y
!    y=EXP(x)
!    RETURN
!END SUBROUTINE
!
!SUBROUTINE h(x,y)
!    REAL,INTENT(IN)::x
!    REAL,INTENT(OUT)::y
!    y=SIN(x)
!    RETURN
!END SUBROUTINE

!PROGRAM pppp
!
!REAL,ALLOCATABLE,DIMENSION(:,:)::a,b,r
!REAL,ALLOCATABLE,DIMENSION(:)::vf,vc
!INTEGER::af,ac,bf,bc,k,l,i,j
!WRITE(*,*)'Se realizará el producto matricial AxB'
!WRITE(*,*)'Ingrese la forma de la matriz A'
!READ(*,*)af,ac
!WRITE(*,*)'Ingrese la forma de la matriz B'
!READ(*,*)bf,bc
!
!IF (ac==bf) THEN
!    ALLOCATE(a(af,ac),b(bf,bc),vf(ac),vc(bf),r(af,bc))
!    DO k=1,af
!        WRITE(*,*)'Ingresa la fila',k,'de la matriz A'
!        READ(*,*)(a(k,l),l=1,ac,1)
!    END DO
!    DO k=1,bf
!        WRITE(*,*)'Ingresa la fila',k,'de la matriz B'
!        READ(*,*)(b(k,l),l=1,bc,1)
!    END DO
!    DO i=1,af
!        DO j=1,bc
!            vf=a(i,:)
!            vc=b(:,j)
!            r(i,j)=DOT_PRODUCT(vf,vc)
!        END DO
!    END DO
!    WRITE(*,*)'El resultado del producto es:'
!    DO k=1,ac
!        WRITE(*,*)(r(k,:))
!    END DO
!ELSE
!    WRITE(*,*)'Este tipo de matrices no se puede multiplicar'
!END IF
!END PROGRAM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE maxmin(x0,xn,step,f,xmin,xmax,vmin,vmax)
    REAL,INTENT(IN)::x0,xn
    INTEGER,INTENT(IN)::step
    REAL::f
    REAL,INTENT(OUT)::xmin,xmax,vmin,vmax
    REAL::m
    INTEGER,DIMENSION(1)::lmin,lmax
    REAL,DIMENSION(step+1)::cx,cex
    m=(xn-x0)/REAL(step)
    cx(1)=x0
    cx(step+1)=xn
    DO i=2,step,1
        cx(i)=cx(i-1)+m
    END DO
    DO i=1,step+1
        cex(i)=f(cx(i))
    END DO
    vmin=MINVAL(cex)
    vmax=MAXVAL(cex)
    lmin=MINLOC(cex)
    lmax=MAXLOC(cex)
    xmin=cx(lmin(1))
    xmax=cx(lmax(1))
    RETURN
END SUBROUTINE
PROGRAM siete21again
EXTERNAL f
REAL::x0,xn,f
REAL::xmin,xmax,vmin,vmax
INTEGER::step
WRITE(*,*)'Enter the first value and then the last value:'
READ(*,*)x0,xn
WRITE(*,*)'Enter the number of steps'
READ(*,*)step
CALL maxmin(x0,xn,step,f,xmin,xmax,vmin,vmax)
WRITE(*,*)'The max_value in the given range is',vmax,'found at x=',xmax
WRITE(*,*)'The min_value in the given range is',vmin,'found at x=',xmin
END PROGRAM
REAL FUNCTION f(x)
REAL,INTENT(IN)::x
f=x**2
END FUNCTION





