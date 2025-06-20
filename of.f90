!"wc -l "//TRIM(ADJUSTL(filename))//" > numlinefile"
PROGRAM organiza_file_plot_math_or_gnup
!USE IFPORT
IMPLICIT NONE
INTEGER(4):: xnod, ynod, znod
REAL(8):: x, y, z, r, a, theta, phi, rgbpar
REAL(8):: rc1, rc2, rc3, rct, rcp, pi
INTEGER(4):: i, j, k, cont, contdw
INTEGER(4):: idwlb, idwub
INTEGER(4):: c1, c2, c3
INTEGER(4):: nlines, ioerror
CHARACTER(15):: hshtg
CHARACTER(15):: filename
CHARACTER(80):: cmdfile, cmdnumline
LOGICAL(4):: onlyredblue
a=0.1d0
rgbpar=0.5d0
pi=4.d0*datan(1.d0)
idwlb=400
idwub=600
onlyredblue=.TRUE.

cmdfile="ls | grep -i *.ovf > filename"
CALL execute_command_line(cmdfile)
OPEN(10,FILE="filename",STATUS='OLD')
READ(10,*)filename
!WRITE(*,*)filename
CLOSE(10)
cmdnumline="wc -l "//TRIM(ADJUSTL(filename))//" > numlinefile"
CALL execute_command_line(cmdnumline)
OPEN(11,FILE="numlinefile",STATUS='OLD')
READ(11,*)nlines
!WRITE(*,*)nlines
CLOSE(11)

xnod=986
ynod=126
znod=30
OPEN(1,FILE=filename,STATUS="OLD")
OPEN(2,FILE="posxyzf.dat")
OPEN(3,FILE="fini.dat")
OPEN(4,FILE="fend.dat")
OPEN(5,FILE="posxyzrgb.dat")
OPEN(7,FILE="dwcont")
OPEN(8,FILE="dw.dat")
OPEN(9,FILE="posxyzRedBlueTP.dat")
cont=0
contdw=0
DO
        READ(1,*,iostat=ioerror)hshtg
        IF(ioerror.NE.0)        EXIT
        hshtg=TRIM(ADJUSTL(hshtg))
        !write(*,*)hshtg
        !IF(hshtg.EQ."#")        CYCLE
        IF(hshtg.EQ."#")        THEN
                cont=cont+1
                WRITE(*,*) cont, hshtg
                CYCLE
        ENDIF
        !
        BACKSPACE(1)
        DO k=0, znod-1
        DO j=0, ynod-1
        DO i=0, xnod-1
                READ(1,*)x, y, z
                IF(x.NE.0.d0)   THEN
                        WRITE(2,'(3I4,3F14.8)')i, j, k, x, y, z
                        c1=DINT(1.28d2*(x+1.d0))
                        c2=DINT(1.28d2*(y+1.d0))
                        c3=DINT(1.28d2*(z+1.d0))
                        rc1=rgbpar*(x+1.d0)
                        rc2=rgbpar*(y+1.d0)
                        rc3=rgbpar*(z+1.d0)
                        !WRITE(5,'(6I4)')i, j, k, c1, c2, c3
                        WRITE(5,'(3I4,3F14.8)')i, j, k, rc1, rc2, rc3
                        r=DSQRT(x*x+y*y+z*z)
                        theta=DACOS(z/r)
                        phi=DATAN2(y,x)
                        rcp=(phi+pi)/(2.d0*pi)
                        rct=theta/pi
                        WRITE(9,'(3I4,3F14.8)')i, j, k, rcp, 0.d0, rct
                        IF((i.GT.idwlb).AND.(i.LT.idwub))       THEN
                                contdw=contdw+1
                                IF(onlyredblue) THEN
                                        WRITE(8,'(3I4,3F14.8)')i, j, k, rcp, 0.d0, rct
                                ELSE
                                        WRITE(8,'(3I4,3F14.8)')i, j, k, rc1, rc2, rc3
                                ENDIF        
                        ENDIF
                        x=a*r*DSIN(theta)*DCOS(phi)
                        y=a*r*DSIN(theta)*DSIN(phi)
                        z=a*r*DCOS(theta)
                        WRITE(3,*) i-x, j-y, k-z
                        WRITE(4,*) i+x, j+y, k+z
                ENDIF
        ENDDO
        ENDDO
        ENDDO
        !
ENDDO
WRITE(7,*)contdw
!
CLOSE(9)
CLOSE(8)
CLOSE(7)
CLOSE(5)
CLOSE(4)
CLOSE(3)
CLOSE(2)
CLOSE(1)

cmdnumline="wc -l fini.dat > totnumspins"
CALL execute_command_line(cmdnumline)

STOP
END PROGRAM organiza_file_plot_math_or_gnup
