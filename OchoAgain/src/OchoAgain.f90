MODULE cplx
CONTAINS
SUBROUTINE modulos(matriz, mmod)
    COMPLEX(8),INTENT(IN),DIMENSION(:,:)::matriz
    REAL(8),INTENT(OUT),DIMENSION(:,:)::mmod
    INTEGER::row,col
    row=SIZE(matriz,1)
    col=SIZE(matriz,2)
    DO i=1,row
        DO j=1,col
            mmod(i,j)=ABS(matriz(i,j))
        END DO
    END DO
    RETURN
END SUBROUTINE
SUBROUTINE minimos(matriz,mmod,inmin,vmin)
    COMPLEX(8),INTENT(IN),DIMENSION(:,:)::matriz
    REAL(8),INTENT(IN),DIMENSION(:,:)::mmod
    INTEGER,INTENT(OUT),DIMENSION(2)::inmin
    REAL(8),INTENT(OUT)::vmin
    INTEGER::row,col
    inmin=MINLOC(mmod)
    vmin=MINVAL(mmod)
    row=inmin(1)
    col=inmin(2)
    WRITE(*,*)'El modulo mas peque�o es igual a',vmin
    WRITE(*,*)'Se encuentra en la posici�n (',inmin(1),',',inmin(2),')'
    WRITE(*,*)'Corresponde al complejo',matriz(row,col)
END SUBROUTINE
END MODULE

PROGRAM ochoA
USE cplx
REAL(8)::rp,ip,vmin
COMPLEX(8),ALLOCATABLE,DIMENSION(:,:)::m
REAL(8),ALLOCATABLE,DIMENSION(:,:)::mm
INTEGER,DIMENSION(2)::indice
INTEGER::row,col
WRITE(*,*)'Ingresa la forma de la matriz compleja'
READ(*,*)row,col
ALLOCATE(m(row,col),mm(row,col))
WRITE(*,*)'Ingresa parte real y luego parte imaginaria del complejo'
DO i=1,row
    DO j=1,col
        WRITE(*,*)'Ingresa la entrada (',i,',',j,') de la matriz'
        READ(*,*)rp,ip
        m(i,j)=CMPLX(rp,ip,8)
    END DO
END DO
CALL modulos(m,mm)
CALL minimos(m,mm,indice,vmin)
END PROGRAM
