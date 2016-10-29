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
extern "C" {
#include "clips.h"
}
#include "mayasetup.h"
#include "functional.h"
#include <functional>
#include <string>

#if FUNCTIONAL_EXTENSIONS
static void MapFunction(UDFContext* context, CLIPSValue* ret);
static void FilterFunction(UDFContext* context, CLIPSValue* ret);
static void ExistsFunction(UDFContext* context, CLIPSValue* ret);
static void NotExistsFunction(UDFContext* context, CLIPSValue* ret);
#endif


extern "C" void InstallFunctionalExtensions(void* theEnv) {
#if FUNCTIONAL_EXTENSIONS
	EnvAddUDF((Environment*)theEnv, "map", "m", MapFunction, "MapFunction", 1, UNBOUNDED, "*;y;*", NULL);
	EnvAddUDF((Environment*)theEnv, "filter", "m", FilterFunction, "FilterFunction", 1, UNBOUNDED, "*;y;*", NULL);
	EnvAddUDF((Environment*)theEnv, "exists", "m", ExistsFunction, "ExistsFunction", 1, UNBOUNDED, "*;y;*", NULL);
	EnvAddUDF((Environment*)theEnv, "not-exists", "m", NotExistsFunction, "NotExistsFunction", 1, UNBOUNDED, "*;y;*", NULL);
#endif
}

#if FUNCTIONAL_EXTENSIONS
void
MapFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue func;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &func)) {
		CVSetBoolean(ret, false);
		return;
	} else {
		auto body = [](UDFContext* context, CLIPSValue* ret, CLIPSValue* theArg, const std::string& name, FUNCTION_REFERENCE* fref, struct FunctionDefinition *theFunction) -> bool {
			struct multifield *theMultifield = nullptr;
			struct expr *lastAdd = nullptr, 
						*nextAdd = nullptr, 
						*multiAdd = nullptr;
			Environment* theEnv = UDFContextEnvironment(context);
			ExpressionInstall(theEnv,fref);

			switch(GetpType(theArg)) {
				case MULTIFIELD:
					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));

					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;

					multiAdd = NULL;
					theMultifield = (struct multifield *) GetpValue(theArg);
					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
						if (multiAdd == NULL) {
							lastAdd->argList = nextAdd;
						} else {
							multiAdd->nextArg = nextAdd;
						}
						multiAdd = nextAdd;
					}

					ExpressionInstall(theEnv,lastAdd);
					break;

				default:
					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;
					ExpressionInstall(theEnv,lastAdd);
					break;
			}

			/*===========================================================*/
			/* Verify a deffunction has the correct number of arguments. */
			/*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
			if (fref->type == PCALL) {
				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
					PrintErrorID(theEnv,"MISCFUN",4,false);
					EnvPrintRouter(theEnv,WERROR,"Function map called with the wrong number of arguments for deffunction ");
					EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,fref->value));
					EnvPrintRouter(theEnv,WERROR,"\n");
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}
#endif

			/*=========================================*/
			/* Verify the correct number of arguments. */
			/*=========================================*/

			if (fref->type == FCALL) {
				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}

			/*======================*/
			/* Call the expression. */
			/*======================*/

			EvaluateExpression(theEnv,fref,ret);

			/*========================================*/
			/* Return the expression data structures. */
			/*========================================*/

			ExpressionDeinstall(theEnv,fref);
			ReturnExpression(theEnv,fref->argList);
			fref->argList = nullptr;

			return true;
		};
		std::string name(CVToString(&func));
		Environment* env = UDFContextEnvironment(context);
		struct expr *tmp2 = nullptr;
		struct FunctionDefinition *theFunction = nullptr;
		CLIPSValue curr, tmp;
		FUNCTION_REFERENCE fref;

		if (!GetFunctionReference(env, name.c_str(), &fref)) {
			ExpectedTypeError1(env,"map",1,"function, deffunction, or generic function name");
			return;
		}

		if (fref.type == FCALL) {
			theFunction = FindFunction(env, name.c_str());
			if (theFunction->parser != NULL) {
				ExpectedTypeError1(env,"map",1,"function without specialized parser");
				return;
			}
		}

		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
				CVSetBoolean(ret, false);
				return;
			} else {
				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
					if (!tmp2) {
						tmp2 = ConvertValueToExpression(env, &tmp);
					} else {
						tmp2 = AppendExpressions(tmp2, ConvertValueToExpression(env, &tmp));
					}
				} else {
					return;
				}
			}
		}
		StoreInMultifield(env, ret, tmp2, true);
	}
}

void
FilterFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue func;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &func)) {
		CVSetBoolean(ret, false);
		return;
	} else {
		auto body = [](UDFContext* context, CLIPSValue* ret, CLIPSValue* theArg, const std::string& name, FUNCTION_REFERENCE* fref, struct FunctionDefinition *theFunction) -> bool {
			struct multifield *theMultifield = nullptr;
			struct expr *lastAdd = nullptr, 
						*nextAdd = nullptr, 
						*multiAdd = nullptr;
			Environment* theEnv = UDFContextEnvironment(context);
			ExpressionInstall(theEnv,fref);

			switch(GetpType(theArg)) {
				case MULTIFIELD:
					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));

					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;

					multiAdd = NULL;
					theMultifield = (struct multifield *) GetpValue(theArg);
					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
						if (multiAdd == NULL) {
							lastAdd->argList = nextAdd;
						} else {
							multiAdd->nextArg = nextAdd;
						}
						multiAdd = nextAdd;
					}

					ExpressionInstall(theEnv,lastAdd);
					break;

				default:
					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;
					ExpressionInstall(theEnv,lastAdd);
					break;
			}

			/*===========================================================*/
			/* Verify a deffunction has the correct number of arguments. */
			/*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
			if (fref->type == PCALL) {
				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
					PrintErrorID(theEnv,"MISCFUN",4,false);
					EnvPrintRouter(theEnv,WERROR,"Function filter called with the wrong number of arguments for deffunction ");
					EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,fref->value));
					EnvPrintRouter(theEnv,WERROR,"\n");
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}
#endif

			/*=========================================*/
			/* Verify the correct number of arguments. */
			/*=========================================*/

			if (fref->type == FCALL) {
				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}

			/*======================*/
			/* Call the expression. */
			/*======================*/

			EvaluateExpression(theEnv,fref,ret);

			/*========================================*/
			/* Return the expression data structures. */
			/*========================================*/

			ExpressionDeinstall(theEnv,fref);
			ReturnExpression(theEnv,fref->argList);
			fref->argList = nullptr;

			return true;
		};
		std::string name(CVToString(&func));
		Environment* env = UDFContextEnvironment(context);
		struct expr *tmp2 = nullptr;
		struct FunctionDefinition *theFunction = nullptr;
		CLIPSValue curr, tmp;
		FUNCTION_REFERENCE fref;

		if (!GetFunctionReference(env, name.c_str(), &fref)) {
			ExpectedTypeError1(env,"filter",1,"function, deffunction, or generic function name");
			return;
		}

		if (fref.type == FCALL) {
			theFunction = FindFunction(env, name.c_str());
			if (theFunction->parser != NULL) {
				ExpectedTypeError1(env,"filter",1,"function without specialized parser");
				return;
			}
		}

		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
				CVSetBoolean(ret, false);
				return;
			} else {
				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
					if (!mCVIsFalseSymbol(&tmp)) {
						if (!tmp2) {
							tmp2 = ConvertValueToExpression(env, &curr);
						} else {
							tmp2 = AppendExpressions(tmp2, ConvertValueToExpression(env, &curr));
						}
					}
				} else {
					return;
				}
			}
		}
		StoreInMultifield(env, ret, tmp2, true);
	}
}

void
ExistsFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue func;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &func)) {
		CVSetBoolean(ret, false);
		return;
	} else {
		auto body = [](UDFContext* context, CLIPSValue* ret, CLIPSValue* theArg, const std::string& name, FUNCTION_REFERENCE* fref, struct FunctionDefinition *theFunction) -> bool {
			struct multifield *theMultifield = nullptr;
			struct expr *lastAdd = nullptr, 
						*nextAdd = nullptr, 
						*multiAdd = nullptr;
			Environment* theEnv = UDFContextEnvironment(context);
			ExpressionInstall(theEnv,fref);

			switch(GetpType(theArg)) {
				case MULTIFIELD:
					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));

					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;

					multiAdd = NULL;
					theMultifield = (struct multifield *) GetpValue(theArg);
					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
						if (multiAdd == NULL) {
							lastAdd->argList = nextAdd;
						} else {
							multiAdd->nextArg = nextAdd;
						}
						multiAdd = nextAdd;
					}

					ExpressionInstall(theEnv,lastAdd);
					break;

				default:
					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;
					ExpressionInstall(theEnv,lastAdd);
					break;
			}

			/*===========================================================*/
			/* Verify a deffunction has the correct number of arguments. */
			/*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
			if (fref->type == PCALL) {
				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
					PrintErrorID(theEnv,"MISCFUN",4,false);
					EnvPrintRouter(theEnv,WERROR,"Function exists called with the wrong number of arguments for deffunction ");
					EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,fref->value));
					EnvPrintRouter(theEnv,WERROR,"\n");
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}
#endif

			/*=========================================*/
			/* Verify the correct number of arguments. */
			/*=========================================*/

			if (fref->type == FCALL) {
				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}

			/*======================*/
			/* Call the expression. */
			/*======================*/

			EvaluateExpression(theEnv,fref,ret);

			/*========================================*/
			/* Return the expression data structures. */
			/*========================================*/

			ExpressionDeinstall(theEnv,fref);
			ReturnExpression(theEnv,fref->argList);
			fref->argList = nullptr;

			return true;
		};
		std::string name(CVToString(&func));
		Environment* env = UDFContextEnvironment(context);
		struct expr *tmp2 = nullptr;
		struct FunctionDefinition *theFunction = nullptr;
		CLIPSValue curr, tmp;
		FUNCTION_REFERENCE fref;

		if (!GetFunctionReference(env, name.c_str(), &fref)) {
			ExpectedTypeError1(env,"exists",1,"function, deffunction, or generic function name");
			return;
		}

		if (fref.type == FCALL) {
			theFunction = FindFunction(env, name.c_str());
			if (theFunction->parser != NULL) {
				ExpectedTypeError1(env,"exists",1,"function without specialized parser");
				return;
			}
		}

		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
				CVSetBoolean(ret, false);
				return;
			} else {
				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
					if (!mCVIsFalseSymbol(&tmp)) {
						CVSetBoolean(ret, true);
						return;
					}
				} else {
					return;
				}
			}
		}
		CVSetBoolean(ret, false);
	}
}

void
NotExistsFunction(UDFContext* context, CLIPSValue* ret) {
	CLIPSValue func;
	if (!UDFFirstArgument(context, LEXEME_TYPES, &func)) {
		CVSetBoolean(ret, false);
		return;
	} else {
		auto body = [](UDFContext* context, CLIPSValue* ret, CLIPSValue* theArg, const std::string& name, FUNCTION_REFERENCE* fref, struct FunctionDefinition *theFunction) -> bool {
			struct multifield *theMultifield = nullptr;
			struct expr *lastAdd = nullptr, 
						*nextAdd = nullptr, 
						*multiAdd = nullptr;
			Environment* theEnv = UDFContextEnvironment(context);
			ExpressionInstall(theEnv,fref);

			switch(GetpType(theArg)) {
				case MULTIFIELD:
					nextAdd = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"create$"));

					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;

					multiAdd = NULL;
					theMultifield = (struct multifield *) GetpValue(theArg);
					for (int j = GetpDOBegin(theArg); j <= GetpDOEnd(theArg); j++) {
						nextAdd = GenConstant(theEnv,GetMFType(theMultifield,j),GetMFValue(theMultifield,j));
						if (multiAdd == NULL) {
							lastAdd->argList = nextAdd;
						} else {
							multiAdd->nextArg = nextAdd;
						}
						multiAdd = nextAdd;
					}

					ExpressionInstall(theEnv,lastAdd);
					break;

				default:
					nextAdd = GenConstant(theEnv,GetpType(theArg),GetpValue(theArg));
					if (lastAdd == NULL) { 
						fref->argList = nextAdd; 
					} else { 
						lastAdd->nextArg = nextAdd; 
					}
					lastAdd = nextAdd;
					ExpressionInstall(theEnv,lastAdd);
					break;
			}

			/*===========================================================*/
			/* Verify a deffunction has the correct number of arguments. */
			/*===========================================================*/

#if DEFFUNCTION_CONSTRUCT
			if (fref->type == PCALL) {
				if (!CheckDeffunctionCall(theEnv,fref->value,CountArguments(fref->argList))) {
					PrintErrorID(theEnv,"MISCFUN",4,false);
					EnvPrintRouter(theEnv,WERROR,"Function not-exists called with the wrong number of arguments for deffunction ");
					EnvPrintRouter(theEnv,WERROR,EnvGetDeffunctionName(theEnv,fref->value));
					EnvPrintRouter(theEnv,WERROR,"\n");
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}
#endif

			/*=========================================*/
			/* Verify the correct number of arguments. */
			/*=========================================*/

			if (fref->type == FCALL) {
				if (CheckExpressionAgainstRestrictions(theEnv,fref,theFunction,name.c_str())) {
					ExpressionDeinstall(theEnv,fref);
					ReturnExpression(theEnv,fref->argList);
					return false;
				}
			}

			/*======================*/
			/* Call the expression. */
			/*======================*/

			EvaluateExpression(theEnv,fref,ret);

			/*========================================*/
			/* Return the expression data structures. */
			/*========================================*/

			ExpressionDeinstall(theEnv,fref);
			ReturnExpression(theEnv,fref->argList);
			fref->argList = nullptr;

			return true;
		};
		std::string name(CVToString(&func));
		Environment* env = UDFContextEnvironment(context);
		struct expr *tmp2 = nullptr;
		struct FunctionDefinition *theFunction = nullptr;
		CLIPSValue curr, tmp;
		FUNCTION_REFERENCE fref;

		if (!GetFunctionReference(env, name.c_str(), &fref)) {
			ExpectedTypeError1(env,"not-exists",1,"function, deffunction, or generic function name");
			return;
		}

		if (fref.type == FCALL) {
			theFunction = FindFunction(env, name.c_str());
			if (theFunction->parser != NULL) {
				ExpectedTypeError1(env,"not-exists",1,"function without specialized parser");
				return;
			}
		}

		while (UDFHasNextArgument(context)) {
			if (! UDFNextArgument(context,ANY_TYPE,&curr)) {
				CVSetBoolean(ret, false);
				return;
			} else {
				if (body(context, &tmp, &curr, name, &fref, theFunction)) {
					if (!mCVIsFalseSymbol(&tmp)) {
						CVSetBoolean(ret, false);
						return;
					}
				} else {
					return;
				}
			}
		}
		CVSetBoolean(ret, true);
	}
}

#endif

