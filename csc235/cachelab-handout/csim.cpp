/************************************************************/
/* Contributor: Henry Winkleman 							              */
/* Edit Date: November 16, 2020 	   		  		              */
/* Due Date: November 21, 2020 		  				              	*/
/* Course: CSC235-020 										                  */
/* Professor Name: Dr. Schwesinger						              */
/* Assignment: #5 									                      	*/
/* Filename: csim.cpp 						                     	    */
/* Purpose: This program acts as a simulation of cache      */
/* memory. It will intake data given to it through a fine   */
/* and determine how many cache hits and misses were        */
/* performed.                                               */
/************************************************************/
#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <iostream>
#include <fstream>
using namespace std;

/* Globals set by command line args */
int verbosity = 0; /* print trace if set */
int s = 0; /* set setIndex bits */
int b = 0; /* block offset bits */
int E = 0; /* associativity */
char* trace_file = NULL; /* file name */

//Counters
int hitCount = 0;
int missCount = 0;
int evictionCount = 0;

//Definitions
typedef long long unsigned int memR;//Large int storage for memory addresses.
                                    //Naming: memR = memory reference.
typedef struct cBlock { //Struct that functions as simulated cache block.
    bool used = false;  //Flag for if storage is in use.
    memR address = 0;   //Held memory address.
    int recency = 0;    //Incremental recency in set.
} cacheBlock;

//Prototypes
void printUsage(char*[]);
cBlock** cacheConstructor();
void readFile(cBlock**);
void cacheOperation(int, cBlock**);
void evictCache(cBlock**, memR, const int&);
void cacheDeconstructor(cBlock**);
void printSummary(int, int, int);


//MAIN PROGRAM
int main(int argc, char* argv[]){
    char c;

    while( (c=getopt(argc,argv,"s:E:b:t:vh")) != -1){
        switch(c){
        case 's':
            s = atoi(optarg);
            break;
        case 'E':
            E = atoi(optarg);
            break;
        case 'b':
            b = atoi(optarg);
            break;
        case 't':
            trace_file = optarg;
            break;
        case 'v':
            verbosity = 1;
            break;
        case 'h':
            printUsage(argv);
            exit(0);
        default:
            printUsage(argv);
            exit(1);
        }
    }

    /* Make sure that all required command line args were specified */
    if (s == 0 || E == 0 || b == 0 || trace_file == NULL) {
        printf("%s: Missing required command line argument\n", argv[0]);
        printUsage(argv);
        exit(1);
    }

    cBlock** cacheHolder=cacheConstructor();  //Create simulated cache.
    readFile(cacheHolder);  //Read file into the cache, also runs hit/miss.
    cacheDeconstructor(cacheHolder);  //Delete the cache.
    printSummary(hitCount, missCount, evictionCount); //Print results.
    return 0;
}

/*********************************************************************
*   Function name:  printUsage
*   Description:   	Displays command line argument format to the user.
*   Parameters:  		argv[] - Command line values.
*
*   Return Value: 	N/A
*   Other: Default function for the project.
*********************************************************************/
void printUsage(char* argv[]){
  printf("Usage: %s [-hv] -s <num> -E <num> -b <num> -t <file>\n", argv[0]);
  printf("Options:\n");
  printf("  -h         Print this help message.\n");
  printf("  -v         Optional verbose flag.\n");
  printf("  -s <num>   Number of set setIndex bits.\n");
  printf("  -E <num>   Number of lines per set.\n");
  printf("  -b <num>   Number of block offset bits.\n");
  printf("  -t <file>  Trace file.\n");
  printf("\nExamples:\n");
  printf("  linux>  %s -s 4 -E 1 -b 4 -t traces/yi.trace\n", argv[0]);
  printf("  linux>  %s -v -s 8 -E 2 -b 4 -t traces/yi.trace\n", argv[0]);
  exit(0);
}

/*********************************************************************
*   Function name:  cacheConstructor
*   Description:   	Declares and allocates a 2D array of cache blocks.
*   Parameters:  		N/A
*
*   Return Value: 	cBlock** - The 2D array.
*********************************************************************/
cBlock** cacheConstructor(){
  //Calculate number of sets based on set setIndex bits.
  const int S = pow(2, s);

  //Initialize the 2D array for cache blocks.
  cBlock** cacheHolder = new cBlock*[S]; //Create sets to be used.
  for(int i=0; i<S; i++){           //For all sets,
    cacheHolder[i] = new cBlock[E]; //Create E blocks.
	}
  return cacheHolder; //Return 2D array to be used in main().
}

/*********************************************************************
*   Function name:  readFile
*   Description:   	Opens the trace given and parses the strings to
*   add them to the cache blocks.
*   Parameters:  		cacheHolder - Simulated cache.
*
*   Return Value: 	N/A
*********************************************************************/
void readFile(cBlock** cacheHolder){
  ifstream ifs;       //Filestream to read the file.
  char operation;     //Holds the operation for each line.
  string holder;      //Holds the string of the address.
  int size;           //Holds the size given.
  memR tempAddress=0; //Holds the address in proper form.

  ifs.open(trace_file); //File attempts to open.
  if(!ifs){ //If opening fails,
    cout<<"File has failed to open. Exiting program."<<endl; //States failure.
    exit(0); //Closes program.
  }

  while(ifs>>operation){  //While the ifstream isn't empty, extract first char
    //If the char is a data operation,
    if(operation=='L'||operation=='S'||operation=='M'){
    	getline(ifs, holder, ','); //Extract following address.
      tempAddress=stoll(holder, NULL, 16); //Convert string into long long.
      ifs>>size;  //Extract following size for printing purpose.
      if(operation=='L'||operation=='M') //Data load operation?
  		  cacheOperation(tempAddress, cacheHolder);
			if(operation=='S'||operation=='M') //Data store operation?
				cacheOperation(tempAddress, cacheHolder);

			//If verbosity is set, print what was extracted.
      if(verbosity)
       	cout<<operation<<' '<<holder<<','<<size<<endl;
    }
  }
  ifs.close();  //Close the ifstream.
}

/*********************************************************************
*   Function name:  cacheOperation
*   Description:   	Performs cache hits and misses. Calls eviction if
*   necessary.
*   Parameters:  		tempAddress - address given to operate with,
*   cacheHolder - simulated cache.
*
*   Return Value: 	N/A
*   Notes: I chose to go with an incremental form of recency since
*   I had decided to not do a linked-list from the start, and going
*   with a decreasing recency counter would require me to know either
*   the max operations that would occur (to set recency to the highet
*   bound) or to change recency in every block everytime there was an
*   operation. In hindsight I think a linked list would have been more
*   efficient for this use.
*
*********************************************************************/
void cacheOperation(int tempAddress, cBlock** cacheHolder){
	int mostRecent=0;		//Holds most recent value.
  bool wasHit=false;  //Flag for hit.

  memR indexExtraction = tempAddress >> b; //Shift the block offset out of the bitstring.
  memR tagExtraction = tempAddress >> (s+b); //Shift until tag is left.
  //Use bitwise 'and' to parse from tag, leaving bitstring with only setIndex.
	memR setIndex = indexExtraction & ((1<<s)-1);
	memR tag = tagExtraction & ((1<<(64-s-b))-1); //Use bitwise 'and' to parse only tag

	//Look for most recent block.
	for(int i=0; i<E; i++){  //For all used indexes,
		if(cacheHolder[setIndex][i].used == true && //Change recency if greater one is
    cacheHolder[setIndex][i].recency > mostRecent){//found.
			mostRecent = cacheHolder[setIndex][i].recency;
		}
	}

	//For all blocks,
	for(int i=0; i<E; i++){
    //If a used block shares stored tag address,
		if(!wasHit && cacheHolder[setIndex][i].used == true &&
    cacheHolder[setIndex][i].address == tag){
			cacheHolder[setIndex][i].recency = mostRecent+1; //Increase recency.
      wasHit=true;  //Flag as hit.
			hitCount++;   //Increase hit counter.
		}
	}

  while(!wasHit){ //If there was no hit,
    for(int i=0; i<E; i++){ //Look for an empty block
    	if(!wasHit && cacheHolder[setIndex][i].used == false){  //Store data in block
        cacheHolder[setIndex][i].used = true;      //Mark as used.
    		cacheHolder[setIndex][i].address = tag;  //Store address.
    		cacheHolder[setIndex][i].recency = mostRecent+1;//Call most recent.
        wasHit=true;  //Flag as stored.
    		missCount++;  //Increase misses.
      }
    }
    if(!wasHit) //If no empty block was found to store,
      evictCache(cacheHolder, setIndex, mostRecent);  //Evict a block.
  }
}


/*********************************************************************
*   Function name:  evictCache
*   Description:   	Clears a block to be used.
*   Parameters:  		tempAddress - address given to operate with,
*   cacheHolder - simulated cache, mostRecent - highest recency by increment
*
*   Return Value: 	N/A
*********************************************************************/
void evictCache(cBlock** cacheHolder, memR setIndex, const int& mostRecent){
  int leastRecent = mostRecent;

  for(int i=0; i<E; i++){ //For all blocks,
    if(cacheHolder[setIndex][i].recency < leastRecent)//If it is less recent,
      leastRecent = cacheHolder[setIndex][i].recency; //Mark as least recent.
  }
  for(int i=0; i<E; i++){ //Cycle through the set to find least recent again.
    if(cacheHolder[setIndex][i].recency == leastRecent){
      cacheHolder[setIndex][i].used = false; //Mark as cleared.
      evictionCount++;    //Increments evictions.
    }
  }
}

/*********************************************************************
*   Function name:  cacheDeconstructor
*   Description:   	Calls the deconstructer to free both arrays.
*   Parameters:  		cacheHolder - The simulated cache.
*
*   Return Value: 	N/A
*********************************************************************/
void cacheDeconstructor(cBlock** cacheHolder){
	for(int i = 0; i < E; i++){ //For all sets,
		delete(cacheHolder[i]);    //Delete the blocks.
	}
	delete(cacheHolder); //Delete all the sets.
}

/*********************************************************************
*   Function name:  printSummary
*   Description:   	This function provides a standard way for your cache
*               simulator to display its final hit and miss statistics.
*   Parameters:  		hits - counter of hits, misses - counter for misses,
*                   evictions - counter of evictions.
*
*   Return Value: 	N/A
*   Other: Default function for the project.
*********************************************************************/
void printSummary(int hits, int misses, int evictions){
  printf("hits:%d misses:%d evictions:%d\n", hits, misses, evictions);
  FILE* output_fp = fopen(".csim_results", "w");
  assert(output_fp);
  fprintf(output_fp, "%d %d %d\n", hits, misses, evictions);
  fclose(output_fp);
}
