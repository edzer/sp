sh ./build-prep.sh
git2cl > xx
cat xx ChangeLog > inst/ChangeLog
rm xx
git commit -m 'update ChangeLog' inst/ChangeLog
git push inst/ChangeLog
