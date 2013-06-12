C THIS IS A DUMMY PACKAGE FOR ISOLVER 4 AND 5. TO use solver 4 or 5 this package should
C be replaced by dodeint subroutine.
c&&&&&&

      SUBROUTINE dodeint(ystart,nvar,isolver,x1,x2,eps,h1,hmin,nok,
     &                   nbad,fex,jac)

c*    This is a modified double precision version of adaptive stepsize
c*    controlled Runge-Kutta and bsstiff solver driver
c*    For details see Numerical Recipes, Press et al.1992.
      IMPLICIT NONE
      INTEGER nbad,nok,nvar,KMAXX,MAXSTP,NMAX,isolver
      INTEGER i,kmax,kount,nstp
      DOUBLE PRECISION eps,h1,hmin,x1,x2,TINY
      DOUBLE PRECISION ystart(nvar)
      EXTERNAL fex,jac
      PARAMETER (MAXSTP=2000,NMAX=100,KMAXX=1,TINY=1.e-30)
      DOUBLE PRECISION dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),
     *y(NMAX),yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp

        write (*,*) "Solver 4 and 5 are not available..."
        write (*,*)
        write (*,*) "To use solver 4 and 5, users are suggested to ask
     & for  permission to use the codes. Once permission is obtained to 
     & use the solver codes, codes for these  solver  routines  may  be 
     & obtained from the owner of code (i.e. from Cambridge  University 
     & Press) or can contact RT3D developer group."

C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop

      RETURN
      END
