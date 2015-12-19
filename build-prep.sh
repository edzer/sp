# run before R CMD build sp
VERSION=`awk '/Version:/{print $2}' DESCRIPTION`
sed -e 's/@VERSION@/'$VERSION'/' src/sp.h.in > src/sp.h
cp -p src/sp.h inst/include
cp -p src/sp_xports.c inst/include
grep SP_VERSION inst/include/sp.h
