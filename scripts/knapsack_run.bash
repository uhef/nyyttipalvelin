#!/bin/bash
if [ -z $1 ] 
then
	echo "Usage: $0 <json file>"
	exit 2
fi
time curl -d@$1 http://localhost:8080/
