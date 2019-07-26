#!/bin/bash

# tweak the variables

DOWNLOAD=~/Downloads/rubydocs/

mkdir -p fixtures

for fn in `ag -l -G '\.html$' 'class-index' $DOWNLOAD`; do
  echo "Processing $fn"
  if echo $fn | grep -q "_core/"; then
    yarrindexer -C -i $fn | sort > "fixtures/core_class_index.txt"
    yarrindexer -i $fn | sort > "fixtures/core_method_index.txt"
  else
    subpath=`echo $fn | sed "s%.*/libdoc/\(.*\)/rdoc/.*%\1%"`
    dir="fixtures/`dirname $subpath`"
    mkdir -p $dir
    yarrindexer -C -i $fn | sed "s/ →.*//" | sort -u > "$dir/`basename $subpath`_class_index.txt"
    yarrindexer -i $fn | sed "s/ →.*//" | sort -u > "$dir/`basename $subpath`_method_index.txt"
  fi
done
