#!/bin/bash

#Create array of inputs - space separator
#MJ: Define all input vectors here
script_name="Rscript_test_shapr.R"
logfile_bash="memory_log.csv"
logfile_Rscript="timing_log.csv"


p_vec=(2 3 4 5) # 6 7 8 9 10)
n_train_vec=(1000 10000)
n_test_vec=(20) #(10 20 100)
n_batches_vec=(12) #(1 2 4 8 16 24 32)
n_cores_vec=(2 12) #(1 2 4 8 16 24 32)
approach_vec= gaussian #("empirical" "gaussian" "ctree")
multicore_method_vec= ("multisession" "multicore")


## get length of $distro array
len_p_vec=${#p_vec[@]}
len_n_train_vec=${#n_train_vec[@]}
len_n_test_vec=${#n_test_vec[@]}
len_n_batches_vec=${#n_batches_vec[@]}
len_n_cores_vec=${#n_cores_vec[@]}
len_approach_vec=${#approach_vec[@]}
len_multicore_method_vec=${#multicore_method_vec[@]}


## Use bash for loop
for (( i1=0; i1<$len_p_vec; i1++ )); do
for (( i2=0; i2<$len_n_train_vec; i2++ )); do
for (( i3=0; i3<$len_n_test_vec; i3++ )); do
for (( i4=0; i4<$len_n_batches_vec; i4++ )); do
for (( i5=0; i5<$len_n_cores_vec; i5++ )); do
for (( i6=0; i6<$len_approach_vec; i6++ )); do
for (( i7=0; i7<$len_multicore_method_vec; i7++ )); do
running_processes=1
start_new_script=1
while [[ $running_processes == 1 ]]
  do
    echo "$(date '+%Y-%m-%d, %H:%M:%S,') $(smem -t -c pss -P 4.1.1 | tail -n 1), ${p_vec[$i1]}, ${n_train_vec[$i2]}, ${n_test_vec[$i3]}, ${n_batches_vec[$i4]}, ${n_cores_vec[$i5]}, ${approach_vec[$i6]}, ${multicore_method_vec[$i7]}, $logfile_Rscript" | tee -a $logfile_bash
    sleep 1
    if [[ $start_new_script == 1 ]]
      then
        Rscript --verbose $script_name ${p_vec[$i1]} ${n_train_vec[$i2]} ${n_test_vec[$i3]} ${n_batches_vec[$i4]} ${n_cores_vec[$i5]} ${approach_vec[$i6]} $logfile_Rscript &
        start_new_script=0
      fi
    running_processes=$(pgrep -f $script_name -a -c)
  done
done; done; done; done; done; done
