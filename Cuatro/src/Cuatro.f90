PROGRAM binary
    CHARACTER(20)::b,t
    REAL::a
    LOGICAL::f
    INTEGER::i

    f=.TRUE.
    WRITE(*,*) 'Enter the binary number..'
    READ(*,*) b
    a=0
    DO i=1,LEN(TRIM(b))
        t=b(i:i)
        IF (t=='1') THEN
            a=a+2**(LEN(TRIM(b))-i)
        ELSE IF (t=='0') THEN
            a=a
        ELSE
            WRITE(*,*)'Incorrect input'
            f=.FALSE.
        END IF
    END DO
    IF (f.NEQV..FALSE.) THEN
        WRITE(*,*) 'The decimal number is:',a
    END IF
END PROGRAM binary

!Binario con arreglos

