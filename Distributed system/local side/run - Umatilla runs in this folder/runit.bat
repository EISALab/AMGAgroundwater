del trace.txt
del ga.restart
umatilla %1 %2.txt

mkdir "seed%1-%2"
cd "seed%1-%2"

copy ..\ga.out .
copy ..\ga.stat .
copy ..\gann.out .
copy ..\gacount.out .
copy ..\seed.txt .
copy ..\time.txt .
copy ..\ga.gen .
copy ..\umatrain.dat .
copy ..\umatest.dat .
copy ..umacosttr.dat .
copy ..\umacostts.dat .
copy ..\modelerr.dat .
copy ..\pop.stat .
cd ..
