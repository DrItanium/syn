#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include "iris16.h"
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include "iris16_asm.tab.h"

int main(int argc, char* argv[]) {
	FILE* input = 0;
	std::string line("v.obj");
	std::ostream* output = 0;
	bool closeOutput = false,
		 closeInput = false,
		 errorfree = true;
   int last = argc - 1,
   	   i = 0;
   /* make sure these are properly initialized */
   last = argc - 1;
   errorfree = 1;
   i = 0;
   if(argc > 1) {
      for(i = 1; errorfree && (i < last); ++i) {
		 std::string tmpline(argv[i]);
         if(tmpline.size() == 2 && tmpline[0] == '-') {
            switch(tmpline[1]) {
			   case 'o':
			   		++i;
			   		line = argv[i];
			   		break;
               case 'h':
               default:
                  errorfree = false;
                  break;
            }
         } else {
            errorfree = false;
            break;
         }
      }
      if(errorfree) {
         if(i == last) {
			std::string tline(argv[last]);
			if(tline.size() == 1 && tline[0] == '-') {
				input = stdin;
				closeInput = false;
			} else if (tline.size() >= 1) {
				if ((input = fopen(tline.c_str(), "r")) != NULL) {
					closeInput = true;
				} else {
					std::cerr << "Couldn't open " << tline << " for reading!" << std::endl;
					exit(1);
				}
			}
            /* open the output */
            if(line.size() == 1 && line[0] == '-') {
			   output = &std::cout; 
			   closeOutput = false;
            } else {
			   output = new std::ofstream(line.c_str(), std::ofstream::out | std::ofstream::binary);
			   closeOutput = true;
            }
         } else {
			 std::cerr << "no file provided" << std::endl;
         }
      } else {
	  	
	  }
   }
   if(output && input) {
      initialize(output, closeOutput, input);
      do {
         yyparse();
      } while(!feof(yyin));
      resolve_labels();
	  if (closeOutput) {
	  	static_cast<std::ofstream*>(output)->close();
	  	delete output;
	  	output = 0;
	  	state.output = 0;
	  }
	  if(closeInput) {
	  	fclose(input);
		input = 0;
	  }
   } else {
      usage(argv[0]);
   }
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " [-o <file>] <file>" << std::endl;
}
void add_label_entry(const std::string& c, word addr) {
   if (state.labels.count(c) != 0) {
		yyerror("Found a duplicate label!");
		exit(1);
   } else {
	 state.labels[c] = addr;
   }
}

void persist_dynamic_op(void) {
   state.dynops.push_back(curri);
}

void save_encoding(void) {
   if(curri.hassymbol) {
      persist_dynamic_op();
   } else {
      write_dynamic_op(&curri); 
   }
}
void write_dynamic_op(dynamicop* dop) {
   /* ((instruction & ~mask) | (value << shiftcount)) */
   /* little endian build up */
   char* buf = new char[8];
   buf[0] = 0;
   buf[1] = (char)dop->segment;
   buf[2] = (char)(dop->address & 0x00FF);
   buf[3] = (char)((dop->address & 0xFF00) >> 8);
   switch(dop->segment) {
   		case Segment::Code:
			buf[4] = (char)iris::encodeBits<byte, byte, 0b11111000, 3>(
								iris::encodeBits<byte, byte, 0b00000111, 0>((byte)0, dop->group),
								dop->op);
			buf[5] = (char)dop->reg0;
			buf[6] = (char)dop->reg1;
			buf[7] = (char)dop->reg2;
			break;
		case Segment::Data:
			buf[4] = (char)dop->reg1;
			buf[5] = (char)dop->reg2;
			buf[6] = 0;
			buf[7] = 0;
			break;
		default:
			std::cerr << "panic: unknown segment " << (byte)dop->segment << std::endl;
			exit(1);
   }
   state.output->write(buf, 8);
   delete[] buf;
}
void yyerror(const char* s) {
   printf("%d: %s\n", yylineno, s);
   exit(-1);
}
void resolve_labels() {
   /* we need to go through the list of dynamic operations and replace
      the label with the corresponding address */
   for(std::vector<dynamicop>::iterator it = state.dynops.begin(); it != state.dynops.end(); ++it) {
   		if (!resolve_op(&(*it))) {
			std::cerr << "panic: couldn't find label " << it->symbol << std::endl;
			exit(1);
		} else {
			write_dynamic_op(&(*it));
		}
   }
}
bool resolve_op(dynamicop* dop) {
   if(state.labels.count(dop->symbol) == 1) {
		word addr = state.labels[dop->symbol];
		dop->reg1 = iris::decodeBits<word, byte, 0x00FF>(addr);
		dop->reg2 = iris::decodeBits<word, byte, 0xFF00, 8>(addr);
		return true;
   }
   return false;
}

void initialize(std::ostream* output, bool close, FILE* input) {
   yyin = input;
   state.segment = Segment::Code;
   state.data_address = 0;
   state.code_address = 0;
   state.output = output;
   state.closeOutput = close;
   curri.segment = Segment::Code;
   curri.address = 0;
   curri.group = 0;
   curri.op = 0;
   curri.reg0 = 0;
   curri.reg1 = 0;
   curri.reg2 = 0;
   curri.hassymbol = 0;
}
