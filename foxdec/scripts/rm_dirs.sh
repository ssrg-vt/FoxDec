#delete only all dir and don't touch files
#!/bin/bash

for dir in `ls -l | grep ^d | awk '{print $9}'`
do
  echo "going to delete $dir " `rm -rf $dir`
done
