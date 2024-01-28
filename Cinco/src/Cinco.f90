PROGRAM uen
INTEGER,ALLOCATABLE,DIMENSION(:)::a,b,ru,rin
INTEGER,ALLOCATABLE,DIMENSION(:)::u,in
INTEGER::la,lb,lr,lu,lin,cu
WRITE(*,*)'Ingrese el grado del arreglo A y luego el del arreglo B'
READ(*,*)la,lb
ALLOCATE(a(la),b(lb),ru(la+lb),rin(la+lb))
WRITE(*,*)'Ingrese el arreglo A'
READ(*,*)(a(i),i=1,la)
WRITE(*,*)'Ingrese el arreglo B'
READ(*,*)(b(i),i=1,lb)
!Union
lu=0
li=0
DO n=1,la
    ru(n)=a(n)
    lu=lu+1
END DO
DO i=1,lb
    cu=0
    DO j=1,la
        IF (b(i)/=a(j)) THEN
            cu=cu+1
        END IF
    END DO
    IF (cu==3) THEN
        lu=lu+1
        ru(lu)=b(i)
    ELSE
        li=li+1
        rin(li)=b(i)
    END IF
END DO
ALLOCATE(u(lu),in(li))
DO k=1,lu
    u(k)=ru(k)
END DO
DO k=1,li
    in(k)=rin(k)
END DO
WRITE(*,*)'La union es:',u
WRITE(*,*)'La intersección es:',in
!Intersección
END PROGRAM

