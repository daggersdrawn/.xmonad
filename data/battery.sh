#!/bin/bash
PERCENT=`acpi -b | sed 's/\([^:]*\): \([^,]*\), \([0-9]*\)%.*/\3/'`
STATUS=`acpi -b | sed 's/\([^:]*\): \([^,]*\), \([0-9]*\)%.*/\2/'`

if [ $STATUS == "Charging" ]; then
    if [ $PERCENT -ge 99 ]; then
    	echo -n "ac: max"
    else
    	echo -n "ac: $PERCENT%"
    fi
else
  	echo -n "bat: $PERCENT%"
fi

echo ""
