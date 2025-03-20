printf "array size: 5\nthread count: 1\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 5 Null 1 100000000; done;
printf "\n\n\narray size: 5\nthread count: 8\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 5 Null 8 100000000; done;
printf "\n\n\narray size: 5\nthread count: 40\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 5 Null 40 100000000; done;
printf "\n\n\narray size: 100\nthread count: 1\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 100 Null 1 100000000; done;
printf "\n\n\narray size: 100\nthread count: 8\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 100 Null 8 100000000; done;
printf "\n\n\narray size: 100\nthread count: 40\n\n";
for i in {1..5}; do time timeout 3600 java UnsafeMemory Platform 100 Null 40 100000000; done





	     
