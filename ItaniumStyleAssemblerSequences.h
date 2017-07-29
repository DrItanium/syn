/*
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


// Basic assembler routines that all syn targets use
#ifndef SYN_ITANIUM_STYLE_ASSEMBLER_SEQUENCES_H__ 
#define SYN_ITANIUM_STYLE_ASSEMBLER_SEQUENCES_H__ 
#include <functional>
#include <sstream>
#include <bitset>
#include <map>
#include <pegtl.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>

#include "BaseArithmetic.h"
#include "AssemblerBase.h"
namespace syn {
	namespace itanium {
		template<typename Keyword, typename Destination, typename Source, typename Space = AsmSeparator> 
		struct TwoArgumentInstruction : pegtl::seq<
										Keyword,
										Space,
										Destination,
										EqualsSignSeparator,
										Source> { };

		template<typename Keyword, typename Destination, typename Source0, typename Source1, typename Space = AsmSeparator>
		struct ThreeArgumentInstruction : pegtl::seq<
												 Keyword, 
												 Space, 
												 Destination, 
												 EqualsSignSeparator, 
												 Source0, 
												 CommaSeparator, 
												 Source1> { };

		template<typename Keyword, typename Destination, typename Destination2, typename Source0, typename Source1, typename Space = AsmSeparator>
		struct PredicateInstruction : pegtl::seq<
												  Keyword, 
												  Space, 
												  Destination, 
												  CommaSeparator, 
												  Destination2, 
												  EqualsSignSeparator, 
												  Source0, 
												  CommaSeparator, 
												  Source1> { };

		template<typename T>
		struct PredicateSpecifier : pegtl::seq<SymbolLeftParen, T, SymbolRightParen> { };

	}

} // end namespace syn


#endif // end SYN_ITANIUM_STYLE_ASSEMBLER_SEQUENCES_H__ 
