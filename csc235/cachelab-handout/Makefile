#
# Student makefile for Cache Lab
# Note: requires a 64-bit x86-64 system
#
CC = g++
CFLAGS = -g -Wall -Werror

all: csim
	# Generate a handin tar file each time you compile
	-tar -cvf ${USER}-cachelab-handin.tar  csim.cpp

csim: csim.cpp
	$(CC) $(CFLAGS) -o csim csim.cpp

${USER}-cachelab-handin.tar: csim.cpp
	-tar -cf ${USER}-cachelab-handin.tar csim.cpp

.PHONY: submit
submit: ${USER}-cachelab-handin.tar
	chmod 666 ${USER}-cachelab-handin.tar
	cp ./${USER}-cachelab-handin.tar ~schwesin/inbox

#
# Clean the src directory
#
.PHONY: clean
clean:
	rm -rf *.o
	rm -f *.tar
	rm -f csim
	rm -f .csim_results
