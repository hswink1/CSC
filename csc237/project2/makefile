COMPILER  = g++ --std=gnu++0x

DEBUG  = -g

EXE  = app

HDRS  = Term.h\
	Node.h\
	TermArrayList.h\
	TermVectorList.h\
	TermDblLinkList.h\
	TermList.h\

OBJS  = Term.o\
	TermArrayList.o\
	TermVectorList.o\
	TermDblLinkList.o\
	app.o\

SRC  = 	Term.cpp\
	TermArrayList.cpp\
	TermVectorList.cpp\
	TermDblLinkList.cpp\
	app.cpp\


##########################################################################
$(EXE): $(OBJS) $(HDRS) $(SRC)
	$(COMPILER) -o $(EXE) $(OBJS) $(DEBUG)

Term.o:  Term.cpp Term.h
	$(COMPILER) -c Term.cpp $(DEBUG)

TermArrayList.o:  Term.h TermArrayList.cpp TermArrayList.h TermList.h
	$(COMPILER) -c TermArrayList.cpp $(DEBUG)

TermVectorList.o:  Term.h TermVectorList.cpp TermVectorList.h TermList.h
	$(COMPILER) -c TermVectorList.cpp $(DEBUG)

TermDblLinkList.o: Term.h Node.h TermDblLinkList.cpp TermDblLinkList.h TermList.h
	$(COMPILER) -c TermDblLinkList.cpp $(DEBUG)

app.o:  TermArrayList.h TermList.h app.cpp
	$(COMPILER) -c app.cpp $(DEBUG)


clean:
	del *.o $(EXE)
