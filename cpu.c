#include <stdint.h>
#include <stdbool.h>

//Return '1' if the bit value at position y within x is '1' and '0' if it's 0 by ANDing x with a bit mask where the bit in y's position is '1' and '0' elsewhere and comparing it to all 0's.  Returns '1' in least significant bit position if the value of the bit is '1', '0' if it was '0'.

#define READ(x,y) ((0u == (x & (1<<y)))?0u:1u)


struct decodeResults{
	int32_t selA,selB,selD,dataImm,

struct aluResults{
	int32_t aluOut;
	bool memWrite, branch;
};

//Binary to Decimal converter
int BTD(int binary_val){
	int decimal_val = 0, base = 1, rem;
        while (num > 0)
    {
        rem = num % 10;
        decimal_val = decimal_val + rem * base;
        num = num / 10 ;
        base = base * 2;
    }
	return decimal_val;


aluResults alu (int32_t A, int32_t B, int OP, int imm, int32_t pc) {
    struct aluResults results;
       
	switch ( OP ) {
	   case 0 :
		   results.aluOut = A + B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
	   case 1 : 
		   results.aluOut = A - B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
   	   case 2 : 
		   results.aluOut = A & B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
           case 3 : 
		   results.aluOut = A | B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
	   case 4 : 
		   results.aluOut = A ^ B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
	   case 5 : 
		   if( A < B) {
			   results.aluOut = 1;
		   } else results.aluOut = 0;
		   results.branch = false;
		   results.memWrite = false;
		   break;

	   case 6 : 
		   if((uint32_t)A < (uint32_t)B) {
			   results.aluOut = 1;
		   } else results.aluOut = 0;
		   results.branch = false;
		   results.memWrite = false;
		   break;
 	   case 7 : 
		   int shiftValue(
		   results.aluOut = A ^ B;
		   results.branch = false;
		   results.memWrite = false;
		   break;
 	   

int main () {
	int32_t registers[32];
	int32_t instMemory[1024];
	int32_t memory[8192];
	int32_t currInst = 0;

	while(true){
		decode(instMemory[currInst]
	
