/**
 * @file
 * The assembler related functions for cisc0
 * @copyright
 * syn
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef CISC0_CORE_ASSEMBLER_STRUCTURES_H__
#define CISC0_CORE_ASSEMBLER_STRUCTURES_H__

#include <string>
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <tao/pegtl/parse.hpp>
#include <vector>

#include "Base.h"
#include "AssemblerBase.h"
#include "Problem.h"
#include "ClipsExtensions.h"
#include "Cisc0CoreInstructionEncoder.h"
#include "Cisc0ClipsExtensions.h"
#include "Cisc0CoreAssemblerKeywords.h"
#include "Cisc0CoreAssemblerStructures.h"

namespace cisc0 {
#define DefTranslators(type, str) \
    type stringTo ## type ( const std::string& title ) noexcept; \
    const std::string& str ## ToString ( type value ) noexcept
    DefTranslators(CompareStyle, compareStyle);
    DefTranslators(ArithmeticOps, arithmeticOps);
    DefTranslators(ComplexSubTypes, complexSubTypes);
    DefTranslators(MemoryOperation, memoryOperation);
    DefTranslators(EncodingOperation, encodingOperation);
    DefTranslators(ExtendedOperation, extendedOperation);
    DefTranslators(LogicalOps, logicalOps);
    DefTranslators(ParsingOperation, parsingOperation);
    DefTranslators(Operation, operation);
#undef DefTranslators

    /**
     * Given an integer index, convert it to a string representation
     * understandable by the assembler.
     * @param index the numerical index of a register, this will masked to
     * the appropriate range automatically.
     * @return the string representation of the provided register index
     */
    const std::string& registerIndexToString(Word index);

    /**
     * Given an integer index, convert it to a string representation
     * understandable by the assembler.
     * @param index the numerical index of a register, this will masked to
     * the appropriate range automatically.
     * @return the string representation of the provided register index
     */
    inline const std::string& translateRegister(Word index) { return registerIndexToString(index); }

    /**
     * Given a string representation of a register, see if it is possible to
     * translate it to an index value.
     * @param input The string input to attempt to convert
     * @return The index corresponding to the provided input
     */
	Word translateRegister(const std::string& input);

    /**
     * Given an encoded instruction, convert it back to a text representation
     * and save it to the provided stream.
     * @param out the stream to output the data to
     * @param first the first word that makes up the encoded instruction
     * @param second the second word that makes up the encoded instruction (not always applicable)
     * @param third the third word that makes up the encoded instruction (not always applicable)
     */
    void translateInstruction(std::ostream& out, Word first, Word second = 0, Word third = 0) noexcept;

    /**
     * Given an encoded instruction, convert it back to a text representation
     * and return that as a string
     * @param out the stream to output the data to
     * @param first the first word that makes up the encoded instruction
     * @param second the second word that makes up the encoded instruction (not always applicable)
     * @param third the third word that makes up the encoded instruction (not always applicable)
     * @return a string containing the text representation of the encoded * instruction.
     */
    std::string translateInstruction(Word first, Word second = 0, Word third = 0) noexcept;

    namespace assembler {
	    using AssemblerWord = syn::AssemblerWord<RegisterValue>;
        /**
         * Keeps track of the overall state throughout the lifetime of a given
         * parsing
         */
	    struct AssemblerState : public syn::LabelTracker<RegisterValue>, public syn::AddressTracker<RegisterValue>, public syn::FinishedDataTracker<InstructionEncoder> {
			public:
				using Self = AssemblerState;
				using LabelParent = syn::LabelTracker<RegisterValue>;
				using AddressParent = syn::AddressTracker<RegisterValue>;
				using FinishedDataParent = syn::FinishedDataTracker<InstructionEncoder>;

			public:
	    		std::vector<AssemblerWord> finalWords;
	    		std::vector<AssemblerWord> wordsToResolve;
            	void output(void* env, CLIPSValue* ret) noexcept;
	    		void resolveInstructions();
	    		void resolveDeclarations();
				void reset() noexcept;
	    };
	    template<int width>
	    	struct AssemblerWordCreator {
	    		static_assert(width > 0, "Can't have a width of zero or less!");
	    		template<typename Input>
	    			AssemblerWordCreator(const Input& in, AssemblerState& parent) { }
	    		virtual ~AssemblerWordCreator() { }
	    		template<typename Input>
	    			void success(const Input& in, AssemblerState& parent) {
                        auto address = parent.getCurrentAddress();
	    				if (_isLabel) {
	    					parent.wordsToResolve.emplace_back(address, _label, width);
	    				} else {
	    					parent.wordsToResolve.emplace_back(address, _value, width);
	    				}
	    				parent.incrementCurrentAddress(width);
	    			}
	    		void setLabel(const std::string& name) noexcept {
	    			_label = name;
	    			_isLabel = true;
	    		}
	    		void setValue(RegisterValue value) noexcept {
	    			_isLabel = false;
	    			_value = value;
	    		}
	    		bool _isLabel;
	    		std::string _label;
	    		RegisterValue _value;
	    	};
	    using WordCreator = AssemblerWordCreator<1>;
	    using DwordCreator = AssemblerWordCreator<2>;

	    struct ChangeCurrentAddress : public syn::NumberContainer<RegisterValue> {
	    	using syn::NumberContainer<RegisterValue>::NumberContainer;

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.setCurrentAddress(getValue());
	    		}
	    };
	    struct RegisterLabel : public syn::NameToAddressMapping<Address> {
	    	using Parent = syn::NameToAddressMapping<Address>;
	    	template<typename Input>
	    		RegisterLabel(const Input& in, AssemblerState& parent) : Parent(in, parent) {
	    			setValue(parent.getCurrentAddress());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.registerLabel(getTitle(), getValue());
	    		}
	    };
	    struct AssemblerInstruction : public InstructionEncoder {
	    	template<typename Input>
	    		AssemblerInstruction(const Input& in, AssemblerState& parent) {
	    			clear();
	    			setAddress(parent.getCurrentAddress());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerState& parent) {
	    			parent.incrementCurrentAddress(numWords());
	    			// for now, make a copy because I do not care!
	    		    parent.copyToFinishedData(*this);
	    		}
	    };

	    struct NumberContainer : public syn::NumberContainer<RegisterValue> {
	    	using syn::NumberContainer<RegisterValue>::NumberContainer;

	    	template<typename Input>
	    		void success(const Input& in, AssemblerInstruction& parent) {
	    			parent.setFullImmediate(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, RegisterLabel& parent) {
	    			parent.setValue(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, ChangeCurrentAddress& parent) {
                    parent.setValue(getValue());
	    		}

	    	template<typename Input>
	    		void success(const Input& in, AssemblerWord& parent) {
	    			parent.setValue(getValue());
	    		}
	    	template<typename Input, int width>
	    		void success(const Input& in, AssemblerWordCreator<width>& parent) {
	    			parent.setValue(getValue());
	    		}
	    	template<typename Input>
	    		void success(const Input& in, WordCreator& parent) {
	    			success<Input, 1>(in, parent);
	    		}

	    	template<typename Input>
	    		void success(const Input& in, DwordCreator& parent) {
	    			success<Input, 2>(in, parent);
	    		}
	    };

    } // end namespace assembler
} // end namespace cisc0

#endif // end CISC0_CORE_ASSEMBLER_STRUCTURES_H__

