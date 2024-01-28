!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#7-16
!PROGRAM siete16
! REAL::p,k,t,lam
! WRITE(*,*)'Ingresa k (número de eventos)'
! READ(*,*)k
! WRITE(*,*)'Ingresa el intervalo de tiempo'
! READ(*,*)t
! WRITE(*,*)'Ingresa Lambda (número de eventos esperados por unidad de tiempo)'
! READ(*,*)lam
! p=probabilidad(k,t,lam)
! WRITE(*,*)'La probabilidad de que ocurran',k,'eventos en dicho intervalo de tiempo es',p
!END PROGRAM
!
!REAL FUNCTION probabilidad(k,t,lam)
! REAL,INTENT(IN)::k,t,lam
! REAL,EXTERNAL::factorial
! probabilidad=(EXP(-lam*t))*((lam*t)**k/(factorial(INT(k))))
! RETURN
!END FUNCTION
!
!REAL FUNCTION factorial(n)
! INTEGER,INTENT(IN)::n
! INTEGER::cp
! cp=1
! DO i=1,n
!    cp=cp*i
! END DO
! factorial=REAL(cp)
!END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#7-21
!REAL FUNCTION f(x)
!    REAL,INTENT(IN)::x
!    f=x**3-5*x**2+5*x+2
!    RETURN
!END FUNCTION
!
!SUBROUTINE maxmin(f,xi,xf,n,xmin,fmin,xmax,fmax)
!    REAL,EXTERNAL::f
!    REAL,INTENT(IN)::xi,xf
!    INTEGER,INTENT(IN)::n
!    REAL,INTENT(OUT)::xmin,fmin,xmax,fmax
!    REAL,DIMENSION(n)::x,fx
!
!    x(1)=xi
!    x(n)=xf
!    DO i=2,n-1
!        x(i)=x(i-1)+((xf-xi)/(REAL(n-1)))
!    END DO
!    DO i=1,n
!        fx(i)=f(x(i))
!    END DO
!    fmin=fx(1)
!    xmin=x(1)
!    fmax=fx(1)
!    xmax=x(1)
!    DO i=2,n
!        IF(fx(i)<fmin) THEN
!            fmin=fx(i)
!            xmin=x(i)
!        END IF
!        IF(fx(i)>fmax) THEN
!            fmax=fx(i)
!            xmax=x(i)
!        END IF
!    END DO
!    RETURN
!END SUBROUTINE
!
!PROGRAM fmaxmin
!
!REAL,EXTERNAL::f
!REAL::xi,xf,xmin,fmin,xmax,fmax
!INTEGER::n
!REAL,ALLOCATABLE,DIMENSION(:)::x,fx
!WRITE(*,*)'Ingresa el rango [xi,xf]'
!READ(*,*)xi,xf
!WRITE(*,*)'Ingresa el número de pasos'
!READ(*,*)n
!ALLOCATE(x(n),fx(n))
!CALL maxmin(f,xi,xf,n,xmin,fmin,xmax,fmax)
!WRITE(*,*)'El maximo valor encontrado de la función es',fmax,',encontrado en x=',xmax
!WRITE(*,*)'El minimo valor encontrado de la función es',fmin,',encontrado en x=',xmin
!END PROGRAM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#7-21 alternative
!REAL FUNCTION f(x)
!    REAL,INTENT(IN)::x
!    f=x**3-5*x**2+5*x+2
!    RETURN
!END FUNCTION
!
!SUBROUTINE maxmin(f,xi,xf,n,xmin,fmin,xmax,fmax)
!    REAL,EXTERNAL::f
!    REAL,INTENT(IN)::xi,xf
!    INTEGER,INTENT(IN)::n
!    REAL,INTENT(OUT)::xmin,fmin,xmax,fmax
!    REAL,DIMENSION(n)::x,fx
!    INTEGER,DIMENSION(1)::imin,imax
!    x(1)=xi
!    x(n)=xf
!    DO i=2,n-1
!        x(i)=x(i-1)+((xf-xi)/(REAL(n-1)))
!    END DO
!    DO i=1,n
!        fx(i)=f(x(i))
!    END DO
!    fmin=MINVAL(fx)
!    imin=MINLOC(fx)
!    xmin=x(imin(1))
!    fmax=MAXVAL(fx)
!    imax=MAXLOC(fx)
!    xmax=x(imax(1))
!    RETURN
!END SUBROUTINE
!PROGRAM fmaxmin
!REAL,EXTERNAL::f
!REAL::xi,xf,xmin,fmin,xmax,fmax
!INTEGER::n
!REAL,ALLOCATABLE,DIMENSION(:)::x,fx
!WRITE(*,*)'Ingresa el rango [xi,xf]'
!READ(*,*)xi,xf
!WRITE(*,*)'Ingresa el número de pasos'
!READ(*,*)n
!ALLOCATE(x(n),fx(n))
!CALL maxmin(f,xi,xf,n,xmin,fmin,xmax,fmax)
!WRITE(*,*)'El maximo valor encontrado de la función es',fmax,',encontrado en x=',xmax
!WRITE(*,*)'El minimo valor encontrado de la función es',fmin,',encontrado en x=',xmin
!END PROGRAM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#7-26,7-27
MODULE LeastSquaresmod
CONTAINS
SUBROUTINE LinLeastsquares(x,y,m,b)
    REAL,INTENT(IN),DIMENSION(:)::x,y
    REAL,INTENT(OUT)::m,b
    REAL::xmean,ymean
    xmean=SUM(x)/REAL(SIZE(x))
    ymean=SUM(y)/REAL(SIZE(y))
    m=(SUM(x*y)-SUM(x)*ymean)/(SUM(x**2)-SUM(x)*xmean)
    b=ymean-m*xmean
    RETURN
END SUBROUTINE
SUBROUTINE Corrcoefficient(x,y,r,n)
    REAL,INTENT(IN),DIMENSION(:)::x,y
    REAL,INTENT(OUT)::r
    n=REAL(n)
    r=(n*SUM(x*y)-SUM(x)*SUM(y))/SQRT((n*SUM(x**2)-(SUM(x))**2)*(n*SUM(y**2)-(SUM(y))**2))
    RETURN
END SUBROUTINE
END MODULE

PROGRAM Leastsquares
USE LeastSquaresmod
IMPLICIT NONE
REAL,ALLOCATABLE,DIMENSION(:)::x,y
REAL::m,b,r
INTEGER::n,i
WRITE(*,*)'¿Cuantos datos(puntos) se van a ingresar?'
READ(*,*)n
ALLOCATE(x(n),y(n))
WRITE(*,*)'Ingresa la coordenada en x seguida de su correspondiente coordenada y...'
DO i=1,n
    WRITE(*,*)'x='
    READ(*,*)x(i)
    WRITE(*,*)'y='
    READ(*,*)y(i)
END DO
CALL LinLeastsquares(x,y,m,b)
CALL Corrcoefficient(x,y,r,n)
WRITE(*,*)'Para este conjunto de datos, segun la ecuacion y=mx+b'
WRITE(*,*)'m=',m,'y b=',b
WRITE(*,*)'Ademas, el coeficiente de correlacion es',r
END PROGRAM

