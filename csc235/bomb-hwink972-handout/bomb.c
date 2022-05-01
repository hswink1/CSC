#include <stdio.h>
#include <stdlib.h>
#include "support.h"
#include "phases.h"


FILE *infile;

int main(int argc, char *argv[])
{
    char *input;


    /* When run with no arguments, the bomb reads its input lines
     * from standard input. */
    if (argc == 1) {
        infile = stdin;
    } 

    /* When run with one argument <file>, the bomb reads from <file>
     * until EOF, and then switches to standard input. Thus, as you
     * defuse each phase, you can add its defusing string to <file> and
     * avoid having to retype it. */
    else if (argc == 2) {
        if (!(infile = fopen(argv[1], "r"))) {
            printf("%s: Error: Couldn't open %s\n", argv[0], argv[1]);
            exit(8);
        }
    }

    /* You can't call the bomb with more than 1 command line argument. */
    else {
        printf("Usage: %s [<input_file>]\n", argv[0]);
        exit(8);
    }

    /* Do all sorts of secret stuff that makes the bomb harder to defuse. */
    initialize_bomb();

    printf("Welcome to the binary bomb. You have 5 phases to defuse.\n");

    /* Phase 1 */
    input = read_line();
    phase_1(input);
    phase_defused();
    printf("Phase 1 defused.\n");

    /* Phase 2 */
    input = read_line();
    phase_2(input);
    phase_defused();
    printf("Phase 2 defused.\n");

    /* Phase 3 */
    input = read_line();
    phase_3(input);
    phase_defused();
    printf("Phase 3 defused.\n");

    /* Phase 4 */
    input = read_line();
    phase_4(input);
    phase_defused();
    printf("Phase 4 defused.\n");

    /* Phase 5 */
    input = read_line();
    phase_5(input);
    phase_defused();

    return 0;
}
