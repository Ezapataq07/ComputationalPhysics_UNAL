PROGRAM NueveActividad
REAL,DIMENSION(3,3)::m1,m2,p,p1
OPEN(UNIT=1,FILE='M1.txt')
DO i=1,3
    WRITE(*,10)'Ingresa la fila',i,'de la matriz M1'
    READ(*,*)(m1(i,j),j=1,3)
    WRITE(1,11)(m1(i,j),j=1,3)
END DO
CLOSE(1)
OPEN(UNIT=1,FILE='M2.txt')
DO i=1,3
    WRITE(*,10)'Ingresa la fila',i,'de la matriz M1'
    READ(*,*)(m2(i,j),j=1,3)
    WRITE(1,11)(m2(i,j),j=1,3)
END DO
CLOSE(1)
p=MATMUL(m1,m2)
OPEN(UNIT=1,FILE='M1M2.txt')
DO i=1,3
    WRITE(1,11)(p(i,j),j=1,3)
END DO
CLOSE(1)
p1=MATMUL(m2,m1)
OPEN(UNIT=1,FILE='M2M1.txt')
DO i=1,3
    WRITE(1,11)(p(i,j),j=1,3)
END DO
CLOSE(1)
10 FORMAT(A15,1X,I1,1X,A15)
11 FORMAT(3(1X,F7.2))
WRITE(*,*)'Listo calisto &
     R'
END PROGRAM
