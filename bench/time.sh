START=$(date +%s.%N)
# do something #######################

"$@" #> /dev/null
EXIT_CODE=$?

#######################################
END=$(date +%s.%N)
# DIFF=$( echo "scale=3; (${END} - ${START})*1000/1" | bc )
DIFF=$( echo "(${END} - ${START}) * 1000" | bc)
echo $DIFF
exit $EXIT_CODE