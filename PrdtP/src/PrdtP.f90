program prp
    INTEGER::n
    REAL:: p,e1,e2,a

    WRITE(*,*)'Ingresa el grado n de los vectores'
    READ(*,*)n
    a=0
    DO i=1,n
        WRITE(*,*)'Ingresa la componente',i,'del vector 1 y luego la del vector 2'
        READ (*,*)e1,e2
        p=e1*e2
        a=a+p
    END DO
    WRITE(*,*)'El producto punto entre los vectores es',a
end program prp
