/* img.c - the iris image creator */
#include <cstdlib>
#include <cstdio>
#include <string>
#include <cstdint>
#include <fstream>
#include "target/iris16/iris.h"


enum class Segment  {
	Code, 
	Data,
	Count,
};

static void usage(char* arg0);
static void execute(std::istream& file);
iris16::Core proc;

int main(int argc, char* argv[]) {
	std::string line("v.img");
	std::istream* input = 0;
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
				input = &std::cin;
				closeInput = false;
			} else if (tline.size() >= 1) {
				input = new std::ifstream(tline.c_str(), std::ifstream::binary | std::ifstream::in);
				closeInput = true;
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
	  proc.initialize();
	  execute(*input);
	  proc.dump(*output);
	  proc.shutdown();
	  if (closeInput) {
			static_cast<std::ifstream*>(input)->close();
			delete input;
			input = 0;
	  }
	  if (closeOutput) {
		  static_cast<std::ofstream*>(output)->close();
		  delete output;
		  output = 0;
	  }
   } else {
      usage(argv[0]);
   }
}

void usage(char* arg0) {
	std::cerr << "usage: " << arg0 << " [-o <file>] <file>" << std::endl;
}
void execute(std::istream& input) {
	char* buf = new char[8];
	for(int lineNumber = 0; input.good(); ++lineNumber) {
		input.read(buf, 3);
		Segment target = static_cast<Segment>(buf[0]);
		word address = iris::encodeBits<word, byte, word, 0xFF00, 8>(
				iris::encodeBits<word, byte, word, 0x00FF>(word(0), byte(buf[1])),
				byte(buf[2]));
		switch(target) {
			case Segment::Code:
				input.read(buf, 4);
				proc.setInstructionMemory(address, iris16::encodeDword(buf[0], buf[1], buf[2], buf[3]));
				break;
			case Segment::Data:
				input.read(buf, 2);
				proc.setDataMemory(address, iris16::encodeWord(buf[0], buf[1]));
				break;
			default:
				std::cerr << "error: line " << lineNumber << ", unknown segment " << (byte)target << std::endl;
				exit(1);
				break;
		}
	}
	delete[] buf;
}
