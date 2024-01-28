MODULE Matcomplx
CONTAINS
SUBROUTINE Modulos(mc,mm,n,m)
    COMPLEX(8),INTENT(IN),DIMENSION(:,:)::mc
    REAL(8),INTENT(OUT),DIMENSION(:,:)::mm
    DO i=1,n
        DO j=1,m
            mm(i,j)=ABS(mc(i,j))
        END DO
    END DO
    RETURN
END SUBROUTINE
SUBROUTINE MinIndices(mm,in,inval)
    REAL(8),INTENT(IN),DIMENSION(:,:)::mm
    INTEGER,INTENT(OUT),DIMENSION(2)::in
    REAL(KIND=8),INTENT(OUT)::inval
    in=MINLOC(mm)
    inval=MINVAL(mm)
    RETURN
END SUBROUTINE
END MODULE

PROGRAM main
USE Matcomplx
COMPLEX(8),ALLOCATABLE,DIMENSION(:,:)::mc
REAL(8),ALLOCATABLE,DIMENSION(:,:)::mmod
INTEGER,DIMENSION(2)::in
COMPLEX(KIND=8)::c
INTEGER::n,m
REAL(KIND=8)::a,b,inval
WRITE(*,*)'Ingresa el orden de la matriz'
READ(*,*)n,m
WRITE(*,*)'Ingresa la parte real y luego la compleja del numero'
ALLOCATE(mc(n,m),mmod(n,m))
DO i=1,n
    DO j=1,m
        WRITE(*,*)'Ingresa la coordenada (',i,',',j,')'
        READ(*,*)a,b
        c=CMPLX(a,b,8)
        mc(i,j)=c
    END DO
END DO
CALL Modulos(mc,mmod,n,m)
CALL MinIndices(mmod,in,inval)
WRITE(*,*)'La matriz de complejos es:'
DO i=1,n
    WRITE(*,*)mc(i,:)
END DO
WRITE(*,*)'La matriz de sus modulos es:'
DO i=1,n
    WRITE(*,*)mmod(i,:)
END DO
WRITE(*,*)'El valor del modulo mas pequeño es:',inval
WRITE(*,*)'Su ubicacion es la fila',in(1),'columna',in(2)
END PROGRAM



