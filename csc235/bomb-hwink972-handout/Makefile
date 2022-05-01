
.PHONY: submit
submit: ${USER}-bomblab-handin.tar
	chmod 666 ${USER}-bomblab-handin.tar
	cp ./${USER}-bomblab-handin.tar ~schwesin/inbox

${USER}-bomblab-handin.tar: sol.txt bomb
	-tar -cf ${USER}-bomblab-handin.tar sol.txt bomb
