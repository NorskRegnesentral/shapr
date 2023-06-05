#!/bin/bash

#Create array of inputs - space separator
#MJ: Define all input vectors here
p_vec=(10 3 4 5 6 7 8 9 10)
n_train_vec=(1000 10000)
n_test_vec=(100 10 20 100)
n_batches_vec=(1 2 4 8 16 24 32)
n_cores_vec=(1 2 4 8 16 24 32)
approach_vec=("empirical" "gaussian" "ctree")


## get length of $distro array
len_p_vec=${#p_vec[@]}
len_n_train_vec=${#n_train_vec[@]}
len_n_test_vec=${#n_test_vec[@]}
len_n_batches_vec=${#n_batches_vec[@]}
len_n_cores_vec=${#n_cores_vec[@]}
len_approach_vec=${#approach_vec[@]}


## Use bash for loop
for (( i1=0; i1<$len_p_vec; i1++ )); do
for (( i2=0; i2<$len_n_train_vec; i2++ )); do
for (( i3=0; i3<$len_n_test_vec; i3++ )); do
for (( i4=0; i4<$len_n_batches_vec; i4++ )); do
for (( i5=0; i5<$len_n_cores_vec; i5++ )); do
for (( i6=0; i6<$len_approach_vec; i6++ )); do
# CURRENT STARTS
    Rscript --verbose Rscript_test_shapr.R ${p_vec[$i1]} ${n_train_vec[$i2]} ${n_test_vec[$i3]} ${n_batches_vec[$i4]} ${n_cores_vec[$i5]} ${approach_vec[$i6]}
# CURRENT ENDS
done; done; done; done; done; done


# SOMETHING LIKE THIS???
# #START
run=true
new=true
while run
  do
    if (("$new")); then
      Rscript --verbose Rscript_test_shapr.R ${p_vec[$i1]} ${n_train_vec[$i2]} ${n_test_vec[$i3]} ${n_batches_vec[$i4]} ${n_cores_vec[$i5]} ${approach_vec[$i6]} &
      new=false
    else
      echo "$(date '+%Y-%m-%d, %H:%M:%S,') $(smem -t -k -c pss -P 4.1.1 | tail -n 1)" | tee -a logfile2
      sleep 2
    fi
    if (___RSCRIPT IS DONE___); then
      run=false
    fi
  done
## END



while true
  do
    echo "$(date '+%Y-%m-%d, %H:%M:%S,') $(smem -t -k -c pss -P 4.1.1 | tail -n 1)" | tee -a logfile2
  sleep 2
done
