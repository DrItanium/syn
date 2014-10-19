#include "clips.h" 
#include "binops.h"


static uvlong Slice(uvlong w, uvlong s, uvlong e);



extern void BinaryOperationsFunctionDefinitions(void* theEnv) {
    EnvDefineFunction2(theEnv, "right-shift", 'g', PTIEF RightShiftFunction, "RightShiftFunction", "22i");
    EnvDefineFunction2(theEnv, "left-shift", 'g', PTIEF LeftShiftFunction, "LeftShiftFunction", "22i");
    EnvDefineFunction2(theEnv, "binary-and", 'g', PTIEF BinaryAndFunction, "BinaryAndFunction", "22i");
    EnvDefineFunction2(theEnv, "binary-or", 'g', PTIEF BinaryOrFunction, "BinaryOrFunction", "22i");
    EnvDefineFunction2(theEnv, "binary-xor", 'g', PTIEF BinaryXorFunction, "BinaryXorFunction", "22i");
    EnvDefineFunction2(theEnv, "binary-not", 'g', PTIEF BinaryNotFunction, "BinaryNotFunction", "11i");
    EnvDefineFunction2(theEnv, "slice", 'g', PTIEF SliceFunction, "SliceFunction", "33i");
    EnvDefineFunction2(theEnv, "slice8", 'm', PTIEF Slice8Function, "Slice8Function", "11i");
    EnvDefineFunction2(theEnv, "slice4", 'm', PTIEF Slice4Function, "Slice4Function", "11i");
    EnvDefineFunction2(theEnv, "slice2", 'm', PTIEF Slice2Function, "Slice2Function", "11i");
    EnvDefineFunction2(theEnv, "merge", 'g', PTIEF MergeFunction, "MergeFunction", "11m");
}
//use unsigned long long to ensure that logical shift is used instead of
//arithmetic shift
extern vlong RightShiftFunction(void* theEnv) {
    uvlong a, b;

    a = EnvRtnLong(theEnv, 1);
    b = EnvRtnLong(theEnv, 2);
    return (uvlong)(a >> b);
}
extern vlong LeftShiftFunction(void* theEnv) {
    uvlong a, b;

    a = EnvRtnLong(theEnv, 1);
    b = EnvRtnLong(theEnv, 2);
    return (uvlong)(a << b);

}
extern vlong BinaryAndFunction(void* theEnv) {
    uvlong a, b;

    a = EnvRtnLong(theEnv, 1);
    b = EnvRtnLong(theEnv, 2);
    return (uvlong)(a & b);

}
extern vlong BinaryOrFunction(void* theEnv) {
    uvlong a, b;

    a = EnvRtnLong(theEnv, 1);
    b = EnvRtnLong(theEnv, 2);
    return (uvlong)(a | b);

}
extern vlong BinaryXorFunction(void* theEnv) {
    uvlong a, b;

    a = EnvRtnLong(theEnv, 1);
    b = EnvRtnLong(theEnv, 2);
    return (uvlong)(a ^ b);
}
extern vlong BinaryNotFunction(void* theEnv) {
    uvlong a;

    a = EnvRtnLong(theEnv, 1);
    return (uvlong)(~a);
}
uvlong Slice(uvlong w, uvlong s, uvlong e) {
    //TODO: fix this code so that it takes the underlying platform into account
    if(s == 0LL && e == 64LL) {
        return w;
    } else if(s <= 64LL && e <= 64LL && e > s) {
        uvlong width, ff, fb;
#ifdef DEBUG
        uvlong result;
#endif
        width = e - s;
        ff = 64LL - e;
        fb = 64LL - width;
#ifdef DEBUG
        result = (w << ff) >> fb; 
        printf("(%lld << %lld) >> %lld = %lld\n", w, ff, fb, result);
        return result;
#else
        return ((w << ff) >> fb);
#endif
    } else {
        return 0LL;
    }
}

extern vlong SliceFunction(void* theEnv) {
    uvlong w, s, e;

    w = EnvRtnLong(theEnv, 1);
    s = EnvRtnLong(theEnv, 2);
    e = EnvRtnLong(theEnv, 3);
    return (vlong)Slice(w,s,e);
}

#define SetMultifieldSlot(index, from, to) \
    SetMFType(multifield, index, INTEGER); \
SetMFValue(multifield, index, EnvAddLong(theEnv, (vlong)Slice(value, from, to)))

extern void Slice8Function(void* theEnv, DATA_OBJECT_PTR retVal) {
    void* multifield;
    uvlong value;

    value = EnvRtnLong(theEnv, 1);
    multifield = EnvCreateMultifield(theEnv, 8);
    SetMultifieldSlot(1, 0LL, 8LL);
    SetMultifieldSlot(2, 8LL, 16LL);
    SetMultifieldSlot(3, 16LL, 24LL);
    SetMultifieldSlot(4, 24LL, 32LL);
    SetMultifieldSlot(5, 32LL, 40LL);
    SetMultifieldSlot(6, 40LL, 48LL);
    SetMultifieldSlot(7, 48LL, 56LL);
    SetMultifieldSlot(8, 56LL, 64LL);

    SetpType(retVal, MULTIFIELD);
    SetpValue(retVal, multifield);

    SetpDOBegin(retVal, 1);
    SetpDOEnd(retVal, 8);
}

extern void Slice4Function(void* theEnv, DATA_OBJECT_PTR retVal) {
    void* multifield;
    uvlong value;

    value = EnvRtnLong(theEnv, 1);
    multifield = EnvCreateMultifield(theEnv, 4);
    SetMultifieldSlot(1, 0LL, 16LL);
    SetMultifieldSlot(2, 16LL, 32LL);
    SetMultifieldSlot(3, 32LL, 48LL);
    SetMultifieldSlot(4, 48LL, 64LL);

    SetpType(retVal, MULTIFIELD);
    SetpValue(retVal, multifield);

    SetpDOBegin(retVal, 1);
    SetpDOEnd(retVal, 4);
}
extern void Slice2Function(void* theEnv, DATA_OBJECT_PTR retVal) {
    void* multifield;
    uvlong value;

    value = EnvRtnLong(theEnv, 1); 
    multifield = EnvCreateMultifield(theEnv, 2);
    SetMultifieldSlot(1, 0LL, 32LL);
    SetMultifieldSlot(2, 32LL, 64LL);

    SetpType(retVal, MULTIFIELD);
    SetpValue(retVal, multifield);

    SetpDOBegin(retVal, 1);
    SetpDOEnd(retVal, 2);
}

extern vlong MergeFunction(void* theEnv) {
    DATA_OBJECT arg0;
    uvlong length, result;
    void* multifield;
    uvlong t0,t1,t2,t3,t4,t5,t6,t7;

    if(EnvArgCountCheck(theEnv, "merge", EXACTLY, 1) == -1) {
        return 0LL;
    }
    if(EnvArgTypeCheck(theEnv, "merge", 1, MULTIFIELD, &arg0) == FALSE) {
        return 0LL;
    }

    length = (uvlong)GetDOLength(arg0);
    multifield = GetValue(arg0);
    result = 0LL;
    switch(length) {
        case 1:
            return (uvlong)ValueToLong(GetMFValue(multifield, 1));
        case 2:
            t0 = (uvlong)ValueToLong(GetMFValue(multifield, 2));
            result = (uvlong)ValueToLong(GetMFValue(multifield, 1));
            result |= (t0 << 32LL);
            break;
        case 4:
            t0 = (uvlong)ValueToLong(GetMFValue(multifield, 4));
            t1 = (uvlong)ValueToLong(GetMFValue(multifield, 3));
            t2 = (uvlong)ValueToLong(GetMFValue(multifield, 2));
            result = (uvlong)ValueToLong(GetMFValue(multifield, 1));
            result |= (t2 << 16LL);
            result |= (t1 << 32LL);
            result |= (t0 << 48LL);
            break;
        case 8:
            t0 = (uvlong)ValueToLong(GetMFValue(multifield,8));
            t1 = (uvlong)ValueToLong(GetMFValue(multifield,7));
            t2 = (uvlong)ValueToLong(GetMFValue(multifield,6));
            t3 = (uvlong)ValueToLong(GetMFValue(multifield,5));
            t4 = (uvlong)ValueToLong(GetMFValue(multifield,4));
            t5 = (uvlong)ValueToLong(GetMFValue(multifield,3));
            t6 = (uvlong)ValueToLong(GetMFValue(multifield,2));
            t7 = (uvlong)ValueToLong(GetMFValue(multifield,1));
            result = t7;
            result |= (t6 << 8LL);
            result |= (t5 << 16LL);
            result |= (t4 << 24LL);
            result |= (t3 << 32LL);
            result |= (t2 << 40LL);
            result |= (t1 << 48LL);
            result |= (t0 << 56LL);
            break;
        default:
            return -1LL;
    }
    return result;
}

#undef SetMultifieldSlot
