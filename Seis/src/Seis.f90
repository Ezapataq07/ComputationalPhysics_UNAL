!#6-10
!PROGRAM seis10
!    REAL,DIMENSION(2)::polar,rect
!    REAL::tetha
!
!    WRITE(*,*) 'Ingresa las coordenadas polares del vector, magnitud seguida del ángulo(en grados)..'
!    READ(*,*)polar(1),polar(2)
!    tetha=polar(2)*(3.141592/180.0)
!    rect(1)=polar(1)*COS(tetha)
!    rect(2)=polar(1)*SIN(tetha)
!    WRITE(*,*)'Las coordenadas rectangulares del vector son: (',rect(1),',',rect(2),')'
!END PROGRAM

!#6-11
!PROGRAM seis11
!    REAL,DIMENSION(2)::rect,polar
!    WRITE(*,*) 'Ingresa las coordenadas rectangulares del vector..'
!    READ(*,*)rect(1),rect(2)
!    polar(1)=SQRT(rect(1)**2+rect(2)**2)
!    polar(2)=(ATAN(rect(2)/rect(1)))*180/3.141592
!    WRITE(*,*)'Las coordenadas polares del vector son: (',polar(1),',',polar(2),'°)'
!END PROGRAM seis11

!#6-19
PROGRAM seis19
    INTEGER,ALLOCATABLE,DIMENSION(:)::a,b,r
    INTEGER,ALLOCATABLE,DIMENSION(:)::u,in
    INTEGER::cu=0,ci=0,m=0,o=0
    !#Leer los datos
    WRITE(*,*)'Ingresa la dimensión del arreglo A y luego la del arreglo B...'
    READ(*,*)k,l
    ALLOCATE(a(k),b(l),r(l+k))
    WRITE(*,*)'Ingresa el arreglo A'
    READ(*,*)(a(i),i=1,k)
    WRITE(*,*)'Ingresa el arreglo B'
    READ(*,*)(b(j),j=1,l)
    !#Busquemos las dimensiones de la union y la intersección
    DO i=1,k
        DO j=1,l
            IF (a(i)==b(j)) THEN
                ci=ci+1
            END IF
        END DO
    END DO
    DO i=1,k
        m=0
        DO n=1,(l+k)
            IF ((a(i)/=r(n)).OR.(a(i)==0)) THEN
                m=m+1
            END IF
        END DO
        IF (m==(l+k)) THEN
            cu=cu+1
            r(cu)=a(i)

            DO j=1,l
                IF (a(i)/=b(j)) THEN
                    o=0
                    DO n=1,(l+k)
                        IF (b(j)/=r(n)) THEN
                            o=o+1
                        END IF
                    END DO
                    IF (o==(l+k)) THEN
                        cu=cu+1
                        r(cu)=b(j)
                    END IF
                END IF
            END DO
        END IF
    END DO
    ALLOCATE(u(cu),in(ci))
    ci=0
    DO i=1,k
        DO j=1,l
            IF (a(i)==b(j)) THEN
                ci=ci+1
                in(ci)=a(i)
            END IF
        END DO
    END DO
    DO i=1,cu
        u(i)=r(i)
    END DO
    WRITE(*,*)'La union entre A y B es:',u
    WRITE(*,*)'La intersección entre A y B es:',in
 END PROGRAM seis19
