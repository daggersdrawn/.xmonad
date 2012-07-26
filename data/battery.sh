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
  if [ ! $STATUS contains "No support" ]; then
    echo -n "bat: $PERCENT%"
  fi
fi

echo ""
