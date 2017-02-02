// Cisc0CoreAssembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "Cisc0Core.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <pegtl/parse.hh>
#include <vector>

namespace cisc0 {
    template<typename R> struct Action : pegtl::nothing<R> { };
    void reportError(const std::string& msg) {
        throw syn::Problem(msg);
    }

    class AssemblerWord {
        public:
            AssemblerWord(Address currAddress, Word value) : _currAddress(currAddress), _value(value), _isLabel(false) { }
            AssemblerWord(Address currAddress, const std::string& labelTitle) : _currAddress(currAddress), _value(0), _isLabel(true), _label(labelTitle) { }
            virtual ~AssemblerWord() { }
            Address getAddress() noexcept const { return _currAddress; }
            Word getValue() noexcept const { return _value; }
            void setValue(Word value) noexcept { _value = value; }
            bool isLabel() noexcept const { return _isLabel; }
            std::string getLabel() noexcept const { return _label; }
        private:
            Address _currAddress;
            Word _value;
            bool _isLabel;
            std::string _label;
    };

#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
//DefSymbol(Nop, nop);
DefSymbol(Arithmetic, arithmetic);
DefSymbol(Shift, shift);
DefSymbol(Logical, logical);
DefSymbol(Compare, compare);
DefSymbol(Branch, branch);
//DefSymbol(Return, return);
DefSymbol(System, system);
DefSymbol(Move, move);
DefSymbol(Set, set);
DefSymbol(Swap, swap);
DefSymbol(Memory, memory);
DefSymbol(Complex, complex);


}



