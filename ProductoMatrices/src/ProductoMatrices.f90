PROGRAM PrdtM
    REAL,ALLOCATABLE,DIMENSION(:,:)::mm,mn,p
    REAL,ALLOCATABLE,DIMENSION(:)::vf,vc
    INTEGER::m1,m2,n1,n2,i,j,k
    REAL::ac
    WRITE(*,*)'Ingresa el rango de la matriz 1'
    READ(*,*)m1,m2
    WRITE(*,*)'Ingresa el rango de la matriz 2'
    READ(*,*)n1,n2
    IF (m2==n1) THEN
        ALLOCATE(mm(m1,m2),mn(n1,n2),p(m1,n2),vf(m2),vc(n1))
        DO i=1,m1
            WRITE(*,*)'Ingresa la fila',i,'de la matriz 1'
            READ(*,*) (mm(i,j),j=1,m2)
        END DO
        DO i=1,n1
            WRITE(*,*)'Ingresa la fila',i,'de la matriz 2'
            READ(*,*) (mn(i,j),j=1,n2)
        END DO
        DO i=1,m1
            DO j=1,n2
                vf=mm(i,:)
                vc=mn(:,j)
                ac=0
                DO k=1,m2
                    ac=ac+vf(k)*vc(k)
                END DO
                p(i,j)=ac
            END DO
        END DO
        WRITE(*,*)'El producto matriz 1 x matriz 2 es'
        DO k=1,m1
            WRITE(*,*) (p(k,j),j=1,n2)
        END DO
    ELSE
        WRITE(*,*)'Las matrices ingresadas no se pueden multiplicar'
    END IF
END PROGRAM
