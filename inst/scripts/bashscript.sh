while true
  do
    echo "$(date '+%Y-%m-%d, %H:%M:%S,') $(smem -t -k -c pss -P 4.1.1 | tail -n 1)" | tee -a logfile2
  sleep 2
done
#watch -t -n 10 "(date '+%Y-%m-%d, %H:%M:%S,' ; smem -t -k -c pss -P 4.1.1 | tail -n 1) | tee -a logfile"
