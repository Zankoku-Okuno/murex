#! /bin/bash
egrep -r --exclude issues.sh --exclude-dir .hsenv 'TODO|FIXME|STUB' . | less
