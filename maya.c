// maya
// Copyright (c) 2012-2016, Joshua Scoggins
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include "clips.h"
#include "miscfun.h"
#include "extnfunc.h"
#include "maya.h"
#include "mayasetup.h"

#if BOOST_EXTENSIONS
#include "boost.h"
#endif 

#if FUNCTIONAL_EXTENSIONS
#include "functional.h"
#endif

#if !MAYA_EXTENSIONS
void InstallMayaExtensions(void* environment) { }
#else
static void EmptyFunction(Environment*, UDFContext*, UDFValue*);
static void Functionp(Environment*, UDFContext*, UDFValue*);
static void LastFunction(Environment*, UDFContext* context, UDFValue* ret);

void InstallMayaExtensions(Environment* environment) {
	AddUDF(environment, "empty$", "b", 1, 1, "m", EmptyFunction, "EmptyFunction", NULL);
	AddUDF(environment, "functionp", "b", 1, 1, "y", Functionp, "Functionp", NULL);
	AddUDF(environment, "quit", "v", 0, 1, "l", ExitCommand, "ExitCommand", NULL);
	AddUDF(environment, "bye", "v", 0, 1, "l", ExitCommand, "ExitCommand", NULL);
	AddUDF(environment, "last$", "m", 1, 1, "m", LastFunction, "LastFunction", NULL);
#if  BOOST_EXTENSIONS
	InstallBoostExtensions(environment);
#endif
#if FUNCTIONAL_EXTENSIONS
	InstallFunctionalExtensions(environment);
#endif
}

void
LastFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue theArg;
	Multifield* theList;

	/*===================================*/
	/* Get the segment to be subdivided. */
	/*===================================*/
	if (!UDFFirstArgument(context, MULTIFIELD_BIT, &theArg)) {
		return; 
	}

	theList = theArg.multifieldValue;

	ret->value = theList;
	if (theArg.range >= 1) {
		ret->begin = (theArg.begin + theArg.range) - 1;
		ret->range = 1;
	} else {
		ret->begin = theArg.begin;
		ret->range = theArg.range;
	}
}

void
Functionp(Environment* env, UDFContext* context, UDFValue* ret) {
	Expression theRef;
	UDFValue theArg;
	ret->lexemeValue = (UDFFirstArgument(context, LEXEME_BITS, &theArg) && GetFunctionReference(env, theArg.lexemeValue->contents, &theRef)) ? TrueSymbol(env) : FalseSymbol(env);
}
void
EmptyFunction(Environment* env, UDFContext* context, UDFValue* ret) {
	UDFValue theArg;
	if (!UDFFirstArgument(context, MULTIFIELD_BIT, &theArg)) {
		return;
	}
	ret->lexemeValue = (theArg.range > 0) ? FalseSymbol(env) : TrueSymbol(env);
}


#endif // end MAYA_EXTENSIONS
