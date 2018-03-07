###########################################################################
# 
#  Makefile for LOADEST
#  --------------------
#  Version: MOD48 (March 2013)
#
#  makefile - used to create LOADEST executable under Linux
#
#                                            -- R.L. Runkel, March 2013
# 
###########################################################################
#
#  compiler setup
#
#  As set below, the default compiler is gfortran, with no command line
#  options. If these defaults are adequate, simply type 'make' to create the
#  executable.  If not, the defaults can be overridden on the command line.
#  For example, the LOADEST executable for distribution is created by
#  passing FORTRAN compilation flags to the makefile:
#
#    make FC=pgfortran  FFLAGS="-fast -tp px -Bstatic -Mdclchk"
#
#  under Linux (Portland Group Fortran 12.5-0).
#
###########################################################################
FC = gfortran
###########################################################################
#
#   LOADEST Object files
#
###########################################################################
OBJ = main.o amlload.o amlload2.o amlpval.o amlreg.o amlstat.o \
      amlstat2.o calbet.o calcpi.o calibr.o center.o clines.o \
      closef.o corr.o d1mach.o dassst.o dasum.o daxpy.o dcopy.o \
      dcsevl.o ddeflt.o ddot.o ddupdu.o dgefa.o dgqtst.o dhumit.o \
      dhumsl.o dlimit.o dlitvm.o dlivmu.o dlngam.o dlogam.o dlsqrt.o \
      dlsvmn.o dnrm2.o dparck.o dscal.o dslvmu.o dswap.o dvdflt.o \
      dvscpy.o erf.o erfc.o error1.o error2.o error3.o error4.o \
      error5.o error6.o error7.o estcal.o estload.o expon.o expvar.o \
      facinv.o getdtime.o getint.o getline.o heading.o idamax.o \
      init.o init2.o init3.o initds.o inputc.o inpute.o inputh.o \
      inputh2.o inputh3.o inputh3b.o inputh4.o jknife.o jroger.o \
      l1norm.o ladload.o ladload2.o ladreg.o ldainit.o load95ci.o \
      loadsep.o loglk.o matcalc.o matinv.o matmlt.o mean.o mkfname.o \
      mleload.o mleload2.o mlevar.o mvuephi.o openfiles.o openin.o \
      outaml.o outeqn.o outeqn2.o outest.o outhead.o outind.o \
      outres.o outsumm.o outsumm2.o outsumm3.o outsummc.o pctl.o \
      pparrange.o ppcctest.o pplot.o pporder.o pred.o ptile.o \
      pvalue.o qnorm01.o setnpar.o setupc.o setxl.o setxl2.o \
      setxl3.o sort.o square.o tac_load.o tacit_calc.o tacit_d.o \
      tacit_g.o tacit_h.o tacit_l.o tacit_loads_nc.o tacit_r.o \
      tacit_test.o transf.o update.o zscore.o 
###########################################################################
#
#  LOADEST
#
###########################################################################
loadest:	$(OBJ)
		$(FC) $(FFLAGS) -o $@ $(OBJ)
		mv -f $@.exe ../$@.exe
clean:
	rm -f ../loadest.exe
	rm -f *.o
