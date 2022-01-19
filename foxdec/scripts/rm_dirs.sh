#delete only all dir and don't touch files
#!/bin/bash


echo -n "This will remove all your current subdirectories. Are you sure? (Y/N)"
read answer
if [ "$answer" != "${answer#[Yy]}" ]
then
  for dir in `ls -l | grep ^d | awk '{print $9}'`
  do
    echo "Deleting $dir " `rm -rf $dir`
  done
fi
