/* microcoded processor */
enum {
   RegisterCount = 1024,
};
uvlong registers[RegisterCount];

uvlong getregistercount(void);
void setregister(uint dest, uvlong value);
uvlong getregister(uint index);
