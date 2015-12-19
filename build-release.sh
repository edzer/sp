sh ./build-prep.sh
svn up
svn2cl
cp ChangeLog inst
svn commit -m tidy inst/ChangeLog
svn up
